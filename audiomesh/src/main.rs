#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use]
extern crate rocket;

use crossbeam_channel::bounded;
use crossbeam_channel::{Receiver, Sender};
use futures::executor::block_on;
use jack;
use rocket::http::Method;
use rocket::serde::json::*;
use rocket_cors::AllowedMethods;
use rocket_cors::AllowedOrigins;
use std::str::FromStr;
use std::sync::Mutex;
//use rocket_contrib::json::*;
use rocket_cors;
//use rocket_cors::AllowedHeaders;

// use petgraph::dot::{Config, Dot};
use petgraph::stable_graph::*;
use serde_json;
//use processgraph::numerical;
use processgraph::*;
use rocket::State;
//use std::sync::Mutex;
//use std::{f32, thread, time};
use rocket::response::content;
//use std::mem;

use processgraph::lag;
use processgraph::process::*;

static mut JACK_PROCESS: Option<jack::AsyncClient<(), JackProc>> = None;

enum UpdateMessage {
    Randomize,
    RandomCircle,
    DumpGraph,
    MatrixConnect,
    SetGraph(UGenGraphStructure, bool),
    AddEdge(NodeIndex, NodeIndex, f64, usize, bool),
    RemoveNode(usize, bool),
    AddNode(Process, bool),
    RemoveEdge(usize, bool),
    SetEdgeWeight(usize, f64),
    SetEdgeFreq(usize, f64),
    SetEdgeDelay(usize, f64),
    SetEdgeFac(f64),
    SetOutput(NodeIndex, usize, f64),
    SetOutputs(Vec<OutputSpec>),
    SetVolume(f64),
    ConnectLeastConnected,
    DisconnectMostConnected,
    SetParameter(usize, u32, f64),
    SetOutputAmp(NodeIndex, f64),
    PollNode(NodeIndex),
}

enum ReturnMessage {
    Graph(String),
    //  Outputs(String),
    PollOutput(String),
}

struct Globals {
    sender: Sender<UpdateMessage>,
    receiver: Receiver<ReturnMessage>,
    matrix_mode: Mutex<bool>,
}

#[get("/matrix")]
fn get_matrix(state: &State<Globals>) -> content::Json<Value> {
    let lock = state.matrix_mode.lock().unwrap();
    content::Json(Value::Bool(*lock))
}

#[post("/matrix/on")]
fn matrix_on(state: &State<Globals>) {
    let mut lock = state.matrix_mode.lock().unwrap();
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    *lock = true;
    sender.send(UpdateMessage::MatrixConnect).unwrap()
}

#[post("/matrix/off")]
fn matrix_off(state: &State<Globals>) {
    let mut lock = state.matrix_mode.lock().unwrap();
    *lock = false;
}

#[post("/volume/<amp>")]
fn set_volume(amp: f64, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::SetVolume(amp)).unwrap()
}

#[delete("/node/<id>")]
fn remove_node(id: usize, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    let matrix = state.matrix_mode.lock().unwrap();
    sender.send(UpdateMessage::RemoveNode(id, *matrix)).unwrap()
}

#[post("/node", data = "<process>")]
fn add_node(process: Json<Process>, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    let matrix = state.matrix_mode.lock().unwrap();
    sender
        .send(UpdateMessage::AddNode(process.into_inner(), *matrix))
        .unwrap()
}

#[post("/graph", data = "<graph>")]
fn set_graph(graph: Json<UGenGraphStructure>, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    let matrix = state.matrix_mode.lock().unwrap();
    sender
        .send(UpdateMessage::SetGraph(graph.into_inner(), *matrix))
        .unwrap()
}

#[post("/node/<id>/output/<output>/amp/<amp>")]
fn node_output(id: usize, output: usize, amp: f64, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender
        .send(UpdateMessage::SetOutput(NodeIndex::new(id), output, amp))
        .unwrap()
}

#[post("/node/<id>/outputamp/<amp>")]
fn node_output_amp(id: usize, amp: f64, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender
        .send(UpdateMessage::SetOutputAmp(NodeIndex::new(id), amp))
        .unwrap()
}

#[post("/outputs", data = "<specvec>")]
fn set_outputs(specvec: Json<Vec<OutputSpec>>, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender
        .send(UpdateMessage::SetOutputs(specvec.into_inner()))
        .unwrap()
}

#[post("/node/<id>/parameter/<input>/<value>")]
fn set_parameter(id: usize, input: u32, value: f64, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender
        .send(UpdateMessage::SetParameter(id, input, value))
        .unwrap()
}

#[delete("/edge/<id>")]
fn remove_edge(id: usize, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    let matrix = state.matrix_mode.lock().unwrap();
    sender.send(UpdateMessage::RemoveEdge(id, *matrix)).unwrap()
}

#[post("/edgefac/<fac>")]
fn set_edge_fac(fac: f64, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::SetEdgeFac(fac)).unwrap()
}

#[post("/edge/<id>/weight/<weight>")]
fn set_edge_weight(id: usize, weight: f64, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    println!("got edge weight {} {}", id, weight);
    sender
        .send(UpdateMessage::SetEdgeWeight(id, weight))
        .unwrap()
}

#[post("/edge/<id>/delay/<delay>")]
fn set_edge_delay(id: usize, delay: f64, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::SetEdgeDelay(id, delay)).unwrap()
}

#[post("/edge/<id>/freq/<freq>")]
fn set_edge_freq(id: usize, freq: f64, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::SetEdgeFreq(id, freq)).unwrap()
}

#[post("/edge/<id_from>/<id_to>/<weight>/<index>")]
fn add_edge(id_from: usize, id_to: usize, weight: f64, index: usize, state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    let matrix = state.matrix_mode.lock().unwrap();
    sender
        .send(UpdateMessage::AddEdge(
            NodeIndex::new(id_from),
            NodeIndex::new(id_to),
            weight,
            index,
            *matrix,
        ))
        .unwrap()
}

#[post("/randomize")]
fn randomize(state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::Randomize).unwrap()
}

#[post("/randomcircle")]
fn random_circle(state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::RandomCircle).unwrap()
}

#[post("/connectleastconnected")]
fn least_connected(state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::ConnectLeastConnected).unwrap()
}

#[post("/disconnectmostconnected")]
fn most_connected(state: &State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::DisconnectMostConnected).unwrap()
}

#[get("/graph")]
fn get_graph(state: &State<Globals>) -> content::Json<String> {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    let receiver = &shared_data.receiver;
    let res = sender.send(UpdateMessage::DumpGraph);
    match receiver.recv().unwrap() {
        ReturnMessage::Graph(g) | ReturnMessage::PollOutput(g) => content::Json(g),
    }
}

#[get("/node/<id>/poll")]
fn poll_node(id: usize, state: &State<Globals>) -> content::Json<String> {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    let receiver = &shared_data.receiver;
    sender
        .send(UpdateMessage::PollNode(NodeIndex::new(id)))
        .unwrap();
    match receiver.recv().unwrap() {
        ReturnMessage::Graph(g) | ReturnMessage::PollOutput(g) => content::Json(g),
    }
}

// #[post("/stop")]
// fn stop(state: State<AudioEngineState>) {
//     let shared_data: &AudioEngineState = state.inner();
//     let mut on = shared_data.on.lock().unwrap();
//     *on = false
// }

fn handle_messages(
    msg: UpdateMessage,
    graph: &mut UGenGraph,
    flow: &mut Vec<NodeIndex>,
    sender: &Sender<ReturnMessage>,
) {
    match msg {
        UpdateMessage::Randomize => {
            graph.graph.clear_edges();
            let nodes: Vec<NodeIndex> = graph.graph.node_indices().collect();
            graph.rnd_connections(&nodes);
            let new_flow = graph.establish_flow();
            *flow = new_flow;
        }
        UpdateMessage::RandomCircle => {
            graph.graph.clear_edges();
            let nodes: Vec<NodeIndex> = graph.graph.node_indices().collect();
            graph.rnd_circle(&nodes, 1);
            let new_flow = graph.establish_flow();
            *flow = new_flow;
        }
        UpdateMessage::DumpGraph => {
            let j = serde_json::to_string(&graph.graph).unwrap();
            sender.send(ReturnMessage::Graph(j)).unwrap()
        }
        UpdateMessage::PollNode(node) => {
            let n = graph.graph.node_weight(node);
            match n {
                Some(n) => {
                    let j = serde_json::to_string(&n.last_value).unwrap();
                    sender.send(ReturnMessage::PollOutput(j)).unwrap()
                }
                _ => sender
                    .send(ReturnMessage::PollOutput("".to_string()))
                    .unwrap(),
            }
        }

        UpdateMessage::SetOutputAmp(node, amp) => {
            if let Some(ugen) = graph.graph.node_weight_mut(node) {
                ugen.output_amp.set_target(amp)
            }
        }

        UpdateMessage::RemoveNode(id, matrix) => {
            graph.graph.remove_node(NodeIndex::new(id));
            graph.update_connections_and_flow(flow, matrix)
        }
        UpdateMessage::SetEdgeFac(fac) => {
            graph.set_edge_fac(fac);
        }
        UpdateMessage::SetEdgeWeight(id, w) => {
            if let Some(e) = graph.graph.edge_weight_mut(EdgeIndex::new(id)) {
                println!("found edge and set to {}", w);
                e.set_weight(w)
            }
        }

        UpdateMessage::SetEdgeDelay(id, d) => {
            if let Some(e) = graph.graph.edge_weight_mut(EdgeIndex::new(id)) {
                e.set_delay(d)
            }
        }

        UpdateMessage::SetEdgeFreq(id, f) => {
            if let Some(e) = graph.graph.edge_weight_mut(EdgeIndex::new(id)) {
                e.set_frequency(f)
            }
        }

        UpdateMessage::RemoveEdge(id, matrix) => {
            graph.graph.remove_edge(EdgeIndex::new(id));
            graph.update_connections_and_flow(flow, matrix)
        }
        UpdateMessage::SetOutput(node, output, amp) => {
            graph.graph[node].set_output(output, amp);
            let new_flow = graph.establish_flow();
            *flow = new_flow;
        }
        UpdateMessage::SetOutputs(outputs) => {
            for spec in outputs.into_iter() {
                graph.set_output(spec);
            }
            let new_flow = graph.establish_flow();
            *flow = new_flow;
        }
        UpdateMessage::AddNode(node, matrix) => {
            let mut ugen = UGen::new(node).clip(ClipType::None);
            ugen.init_after_deserialization();
            let idx = graph.graph.add_node(ugen);
            graph.rnd_connect_if_necessary(idx);
            graph.update_connections_and_flow(flow, matrix)
        }

        UpdateMessage::SetGraph(g, matrix) => {
            graph.graph.clear();
            *graph = UGenGraph::new();
            graph.graph = g;
            graph.init_after_deserialization();
            graph.update_connections_and_flow(flow, matrix)
        }
        UpdateMessage::AddEdge(node_from, node_to, weight, index, matrix) => {
            let _idx = graph.connect(node_from, node_to, Connection::new(index as u32, weight));
        }
        UpdateMessage::ConnectLeastConnected => {
            graph.connect_least_connected();
            graph.update_connections_and_flow(flow, false)
        }
        UpdateMessage::DisconnectMostConnected => {
            graph.disconnect_most_connected();
            graph.update_connections_and_flow(flow, false)
        }

        UpdateMessage::SetParameter(node, idx, value) => {
            graph.set_parameter(NodeIndex::new(node), idx, value);
        }

        UpdateMessage::MatrixConnect => graph.update_connections_and_flow(flow, true),

        _ => (),
    }
}

struct JackProc {
    in_ports: Vec<jack::Port<jack::AudioIn>>,
    out_ports: Vec<jack::Port<jack::AudioOut>>,
    rx: Receiver<UpdateMessage>,
    flow: Vec<NodeIndex>,
    frame_buffer: Vec<f64>,
    amplitude: lag::Lag,
    g: UGenGraph,
    s_ret: Sender<ReturnMessage>,
}

impl jack::ProcessHandler for JackProc {
    fn process(&mut self, _: &jack::Client, ps: &jack::ProcessScope) -> jack::Control {
        let polling = false;

        while let Ok(update) = self.rx.try_recv() {
            match update {
                UpdateMessage::SetVolume(a) => self.amplitude.set_target(a),
                _ => handle_messages(update, &mut self.g, &mut self.flow, &self.s_ret),
            }
        }

        // Get output buffers
        let mut outs: Vec<&mut [f32]> = self
            .out_ports
            .iter_mut()
            .map(|p| p.as_mut_slice(ps))
            .collect();

        // Get input buffers
        let ins: Vec<&[f32]> = self.in_ports.iter().map(|p| p.as_slice(ps)).collect();

        // Write output
        for i in 0..ps.n_frames() {
            let input_samples: Vec<f64> =
                ins.iter().map(|input| input[i as usize] as f64).collect();
            let amp = self.amplitude.tick();
            self.g
                .process(&self.flow, &input_samples, &mut self.frame_buffer);
            if i == 0 && polling {
                println!("{}", self.frame_buffer[0]);
            }

            for (k, o) in outs.iter_mut().enumerate() {
                o[i as usize] = (self.frame_buffer[k] * amp) as f32;
            }
        }

        // Continue as normal
        jack::Control::Continue
    }
}

#[launch]
fn rocket() -> _ {
    let allowed_methods: AllowedMethods = ["Get", "Post", "Put", "Delete"]
        .iter()
        .map(|s| FromStr::from_str(s).unwrap())
        .collect();

    let cors = rocket_cors::CorsOptions {
        allowed_origins: AllowedOrigins::all(),
        allowed_methods: allowed_methods,
        //            .into_iter()
        //            .map(From::from)
        //          .collect(),
        allowed_headers: rocket_cors::AllowedHeaders::All,
        allow_credentials: true,
        ..Default::default()
    };
    // .to_cors()
    // .unwrap();

    // Channels for communication among threads
    let (tx, rx): (Sender<UpdateMessage>, Receiver<UpdateMessage>) = bounded(100);
    let (s_ret, r_ret): (Sender<ReturnMessage>, Receiver<ReturnMessage>) = bounded(100);

    // Create graph and init
    let g = UGenGraph::new();

    let flow = vec![];
    let n_outs = 2;
    let frame_buffer = vec![0.0; n_outs];
    let mut dc_leak_outs = vec![];

    for _i in 0..n_outs {
        dc_leak_outs.push(dc_remove_filter());
    }

    // JACK
    let (client, _status) = jack::Client::new("audiomesh", jack::ClientOptions::empty()).unwrap();

    // set sampling rate in lib
    set_sr(client.sample_rate() as f64);

    // Setting up output ports
    let mut out_ports = Vec::new();
    for i in 1..(n_outs + 1) {
        let name = String::from(format!("out_{}", i));
        out_ports.push(
            client
                .register_port(&name, jack::AudioOut::default())
                .unwrap(),
        );
    }

    // Setting up input ports
    const N_INS: usize = 2;
    let mut in_ports = Vec::new();
    for i in 1..(N_INS + 1) {
        let name = String::from(format!("in_{}", i));
        in_ports.push(
            client
                .register_port(&name, jack::AudioIn::default())
                .unwrap(),
        );
    }

    let amplitude = lag::lag(0.4);

    let jack_process = JackProc {
        in_ports,
        out_ports,
        rx,
        flow,
        frame_buffer,
        amplitude,
        g,
        s_ret,
    };

    unsafe {
        JACK_PROCESS = Some(client.activate_async((), jack_process).unwrap());
    }

    rocket::build()
        .manage(Globals {
            //            graph: &g,
            //            jack_client: active_client,
            sender: tx,
            receiver: r_ret,
            matrix_mode: Mutex::new(false),
        })
        .mount(
            "/",
            routes![
                randomize,
                get_graph,
                remove_node,
                remove_edge,
                node_output,
                add_node,
                //                get_outputs,
                add_edge,
                set_volume,
                least_connected,
                most_connected,
                set_parameter,
                set_edge_weight,
                poll_node,
                set_graph,
                random_circle,
                set_outputs,
                set_edge_fac,
                node_output_amp,
                set_edge_delay,
                set_edge_freq,
                get_matrix,
                matrix_on,
                matrix_off,
            ],
        )
        .attach(cors.to_cors().expect("Cors failed"))
    //        .manage(cors)
}
