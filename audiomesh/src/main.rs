#![feature(proc_macro_hygiene, decl_macro)]

// TODO
// - export/load graph descriptions
// - return graph on update functions (?)
// - add specific endpoints for delays
// - parameter setting (with endpoint)
// - deal with sr for freqs etc

// DONE
// - get graph via serde/api
// - set listening outputs
// - get output indices
// - randomize check if nodeexists
// - edge operations via api
// - output volume

#[macro_use]
extern crate rocket;

use crossbeam_channel::bounded;
use crossbeam_channel::{Receiver, Sender};
use jack;
use rocket::http::Method;
use rocket_contrib::json::*;
use rocket_cors;
use rocket_cors::AllowedHeaders;

// use petgraph::dot::{Config, Dot};
use petgraph::stable_graph::*;
use serde_json;
//use processgraph::numerical;
use processgraph::*;
use rocket::State;
//use std::sync::Mutex;
//use std::{f32, thread, time};
use rocket::response::content;
use std::mem;

mod lag;

enum UpdateMessage {
    Randomize,
    DumpGraph,
    DumpOutputs,
    AddEdge(NodeIndex, NodeIndex, f64, usize),
    RemoveNode(usize),
    AddNode(Process),
    RemoveEdge(usize),
    SetOutput(NodeIndex, usize),
    SetVolume(f64),
}

enum ReturnMessage {
    Graph(String),
    Outputs(String),
}

struct Globals {
    sender: Sender<UpdateMessage>,
    receiver: Receiver<ReturnMessage>,
}

#[post("/volume/<amp>")]
fn set_volume(amp: f64, state: State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::SetVolume(amp)).unwrap()
}

#[delete("/node/<id>")]
fn remove_node(id: usize, state: State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::RemoveNode(id)).unwrap()
}

#[post("/node", data = "<process>")]
fn add_node(process: Json<Process>, state: State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender
        .send(UpdateMessage::AddNode(process.into_inner()))
        .unwrap()
}

#[post("/node/<id>/output/<output>")]
fn node_output(id: usize, output: usize, state: State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender
        .send(UpdateMessage::SetOutput(NodeIndex::new(id), output))
        .unwrap()
}

#[delete("/edge/<id>")]
fn remove_edge(id: usize, state: State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::RemoveEdge(id)).unwrap()
}

#[post("/edge/<id_from>/<id_to>/<weight>/<index>")]
fn add_edge(id_from: usize, id_to: usize, weight: f64, index: usize, state: State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender
        .send(UpdateMessage::AddEdge(
            NodeIndex::new(id_from),
            NodeIndex::new(id_to),
            weight,
            index,
        ))
        .unwrap()
}

#[post("/randomize")]
fn randomize(state: State<Globals>) {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    sender.send(UpdateMessage::Randomize).unwrap()
}

#[get("/outputs")]
fn get_outputs(state: State<Globals>) -> content::Json<String> {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    let receiver = &shared_data.receiver;
    sender.send(UpdateMessage::DumpOutputs).unwrap();
    match receiver.recv().unwrap() {
        ReturnMessage::Outputs(g) | ReturnMessage::Graph(g) => content::Json(g),
    }
}

#[get("/graph")]
fn get_graph(state: State<Globals>) -> content::Json<String> {
    let shared_data: &Globals = state.inner();
    let sender = &shared_data.sender;
    let receiver = &shared_data.receiver;
    sender.send(UpdateMessage::DumpGraph).unwrap();
    match receiver.recv().unwrap() {
        ReturnMessage::Outputs(g) | ReturnMessage::Graph(g) => content::Json(g),
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
    //    nodes: &mut Vec<NodeIndex>,
    flow: &mut Vec<NodeIndex>,
    output_indices: &mut [NodeIndex],
    sender: &Sender<ReturnMessage>,
) {
    match msg {
        UpdateMessage::Randomize => {
            graph.clear_edges();
            let nodes: Vec<NodeIndex> = graph.node_indices().collect();
            rnd_connections(graph, &nodes, 1);
            let new_flow = establish_flow(graph, output_indices);
            //            let _ = mem::replace(flow, new_flow);
            *flow = new_flow;
            //          return ();
        }
        UpdateMessage::DumpGraph => {
            let j = serde_json::to_string(graph).unwrap();
            sender.send(ReturnMessage::Graph(j)).unwrap()
        }
        UpdateMessage::DumpOutputs => {
            let j = serde_json::to_string(output_indices).unwrap();
            sender.send(ReturnMessage::Outputs(j)).unwrap()
        }
        UpdateMessage::RemoveNode(id) => {
            graph.remove_node(NodeIndex::new(id));
            // TODO the following lines should be in one function
            ensure_connectivity(graph);
            let new_flow = establish_flow(graph, output_indices);
            *flow = new_flow;
            //let _ = mem::replace(flow, new_flow);
            //            println!("{:?}", collect_components(graph));
        }
        UpdateMessage::RemoveEdge(id) => {
            graph.remove_edge(EdgeIndex::new(id));
            ensure_connectivity(graph);
            let new_flow = establish_flow(graph, output_indices);
            *flow = new_flow;
            //		let _ = mem::replace(flow, new_flow);

            //          println!("{:?}", collect_components(graph));
        }
        UpdateMessage::SetOutput(node, output) => output_indices[output] = node,
        UpdateMessage::AddNode(node) => {
            let _idx = graph.add_node(UGen::new(node));
            ensure_connectivity(graph);
            let new_flow = establish_flow(graph, output_indices);
            *flow = new_flow;
            //let _ = mem::replace(flow, new_flow);
            //        println!("{:?}", collect_components(graph));
        }
        UpdateMessage::AddEdge(node_from, node_to, weight, index) => {
            let _idx = graph.add_edge(node_from, node_to, (index as u32, weight));
        }
        _ => (),
    }
}

fn main() {
    // Cors header opetions
    let cors = rocket_cors::CorsOptions {
        allowed_methods: vec![Method::Get, Method::Put, Method::Post, Method::Delete]
            .into_iter()
            .map(From::from)
            .collect(),
        allowed_headers: AllowedHeaders::some(&["Authorization", "Accept"]),
        allow_credentials: true,
        ..Default::default()
    }
    .to_cors()
    .unwrap();

    // Channels for communication among threads
    let (tx, rx): (Sender<UpdateMessage>, Receiver<UpdateMessage>) = bounded(100);
    let (s_ret, r_ret): (Sender<ReturnMessage>, Receiver<ReturnMessage>) = bounded(100);

    // Create graph and init
    let mut g = new_graph();

    let mem1 = g.add_node(mem(0.5).clip(ClipType::Wrap));
    let del1 = g.add_node(delay(21231).clip(ClipType::Wrap));
    let rms = g.add_node(rms());
    let del2 = g.add_node(delay(3).clip(ClipType::Wrap));
    let del3 = g.add_node(delay(4).clip(ClipType::Wrap));
    // let sum = g.add_node(add().clip(ClipType::Wrap));
    // let sum2 = g.add_node(add().clip(ClipType::SoftClip));
    // let filter = g.add_node(lpf(1.0, 6.0));
    // let filter2 = g.add_node(hpf(1000.0, 3.0));
    // let in1 = g.add_node(sound_in(0));
    // let gauss = g.add_node(gauss());

    let nodes = vec![mem1, del1, rms, del2, del3];

    // let nodes = vec![
    //     mem1, rms, del1, del3, filter2, filter, sum, gauss, sum2, del2, in1,
    // ];

    rnd_connections(&mut g, &nodes, 1);
    let mut output_indices = vec![mem1, del1];

    let mut flow = establish_flow(&g, &output_indices);
    let n_outs = output_indices.len();
    let mut frame_buffer = vec![0.0; n_outs];

    // println!("flow: {:?}", flow);
    // println!("{:?}", Dot::with_config(&g, &[Config::EdgeNoLabel]));
    // for i in 0..(64 * 10) {
    //     println!("\n\n");
    //     process_graph(&mut g, &flow, &output_indices, &mut frame_buffer);
    //     println!("result: {:?}", frame_buffer);
    // }

    // JACK
    let (client, _status) = jack::Client::new("audiomesh", jack::ClientOptions::empty()).unwrap();

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

    let mut amplitude = lag::lag(1.0);

    let process = jack::ClosureProcessHandler::new(
        move |_: &jack::Client, ps: &jack::ProcessScope| -> jack::Control {
            // get input messages
            let polling = false;

            while let Ok(update) = rx.try_recv() {
                match update {
                    UpdateMessage::SetVolume(a) => amplitude.set_target(a),
                    _ => handle_messages(update, &mut g, &mut flow, &mut output_indices, &s_ret),
                }
            }

            // Get output buffers
            let mut outs: Vec<&mut [f32]> =
                out_ports.iter_mut().map(|p| p.as_mut_slice(ps)).collect();

            // Get input buffers
            let ins: Vec<&[f32]> = in_ports.iter().map(|p| p.as_slice(ps)).collect();

            // Write output
            for i in 0..ps.n_frames() {
                let input_samples: Vec<f64> =
                    ins.iter().map(|input| input[i as usize] as f64).collect();
                let amp = amplitude.tick();
                process_graph(
                    &mut g,
                    &flow,
                    &output_indices,
                    &input_samples,
                    &mut frame_buffer,
                );
                if i == 0 && polling {
                    println!("{}", frame_buffer[0]);
                }

                for (k, o) in outs.iter_mut().enumerate() {
                    o[i as usize] = (frame_buffer[k] * amp) as f32;
                }
            }

            // Continue as normal
            jack::Control::Continue
        },
    );

    let _active_client = client.activate_async((), process).unwrap();

    rocket::ignite()
        .manage(Globals {
            //            graph: &g,
            sender: tx,
            receiver: r_ret,
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
                get_outputs,
                add_edge,
                set_volume
            ],
        )
        .attach(cors)
        .launch();
}
