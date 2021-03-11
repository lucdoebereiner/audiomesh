mod filters;
pub mod lag;
pub mod process;
use crate::process::*;
//use petgraph::stable_graph::StableGraph;
//use petgraph::graph::Graph;
use petgraph::stable_graph::*;
use petgraph::visit::IntoNodeReferences;
use petgraph::Directed;
use petgraph::Direction::*;
//use rand::rngs::SmallRng;
use rand::Rng;
use std::f64;
use std::fmt;
pub mod numerical;
use petgraph::visit::Bfs;
use rand::seq::SliceRandom;
use rand::thread_rng;
use serde::{Deserialize, Serialize};

static mut DEBUG: bool = false;

// TODO
// move debug flag to main

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ClipType {
    None,
    SoftClip,
    Wrap,
}

// index and weight
pub type Connection = (u32, lag::Lag);

#[derive(Serialize, Deserialize)]
pub struct UGen {
    #[serde(skip)]
    feedback_delay: bool,
    process: Process,
    #[serde(skip)]
    value: Option<f64>,
    #[serde(skip)]
    pub last_value: f64,
    sum_inputs: bool,
    clip: ClipType,
    output_sends: Vec<(usize, lag::Lag)>,
    #[serde(skip_deserializing)]
    process_type: Option<ProcessType>,
}

impl UGen {
    pub fn new(process: Process) -> Self {
        let pt = process.spec().process_type;
        UGen {
            feedback_delay: false,
            process,
            value: None,
            sum_inputs: true,
            last_value: 0.0,
            clip: ClipType::None,
            output_sends: vec![],
            process_type: Some(pt),
        }
    }

    pub fn init_after_deserialization(&mut self) {
        self.process_type = Some(self.process.spec().process_type);
    }

    pub fn clip(self, clip_type: ClipType) -> Self {
        UGen {
            clip: clip_type,
            ..self
        }
    }

    pub fn sum_inputs(self) -> Self {
        UGen {
            sum_inputs: true,
            ..self
        }
    }

    pub fn set_output(&mut self, idx: usize, amp: f64) {
        match self.output_sends.iter().position(|(i, _)| *i == idx) {
            Some(pos) => self.output_sends[pos].1.set_target(amp),
            None => self.output_sends.push((idx, lag::lag(amp))),
        }
    }
}

impl UGen {
    fn process(&mut self, input: &[f64]) -> f64 {
        match self.value {
            Some(v) => v,
            None => {
                let v = self.process.process(input);
                let output = match self.clip {
                    ClipType::None => v,
                    ClipType::SoftClip => v.tanh(),
                    ClipType::Wrap => numerical::wrap(v, -1., 1.),
                };
                if unsafe { DEBUG } {
                    println!("processing: [{:?}] with result {}", self, output)
                }
                self.last_value = output;
                self.value = Some(output);
                self.process.clear_inputs();
                output
            }
        }
    }

    fn reset(&mut self) {
        self.value = None
    }

    fn set_input_with_connection(&mut self, connection: (u32, f64), value: f64) {
        let (idx, weight) = connection;
        if unsafe { DEBUG } {
            println!("setting input of [{:?}] to {}", self, value)
        }
        self.process.set_input(idx, value * weight, self.sum_inputs);
        if unsafe { DEBUG } {
            println!("having set [{:?}]", self)
        }
    }
}

impl fmt::Debug for UGen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "UGenData: name: {}, value: {:?}, process: {:?}, clip: {:?}, sum_inputs: {:?}",
            //self.feedback_delay,
            self.process.name(),
            self.value,
            self.process,
            self.clip,
            self.sum_inputs
        )
    }
}

pub type UGenGraphStructure = StableGraph<UGen, Connection, Directed, DefaultIx>;

pub struct UGenGraph {
    pub graph: UGenGraphStructure, // TODO: goal, make no longer pub, all edge/node manipulations via impl
    current_listening_nodes: Vec<NodeIndex>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct OutputSpec {
    node: usize,
    output: usize,
    amp: f64,
}

impl UGenGraph {
    pub fn new() -> UGenGraph {
        UGenGraph {
            graph: StableGraph::with_capacity(100, 100),
            current_listening_nodes: vec![],
        }
    }

    pub fn connect(&mut self, from: NodeIndex, to: NodeIndex, weight: Connection) -> EdgeIndex {
        self.graph.update_edge(from, to, weight)
    }

    pub fn add(&mut self, ugen: UGen) -> NodeIndex {
        self.graph.add_node(ugen)
    }

    pub fn init_after_deserialization(&mut self) {
        self.graph
            .node_weights_mut()
            .for_each(|u| u.process_type = Some(u.process.spec().process_type));
    }

    pub fn set_parameter(&mut self, node_idx: NodeIndex, idx: u32, input_value: f64) {
        if let Some(node) = self.graph.node_weight_mut(node_idx) {
            node.process.set_input(idx as u32, input_value, false)
        }
    }
    pub fn set_output(&mut self, spec: OutputSpec) {
        self.graph[NodeIndex::new(spec.node)].set_output(spec.output, spec.amp)
    }

    // returns true if it had to update
    pub fn update_listening_nodes(&mut self) -> bool {
        let mut nodes_to_listen_to = vec![];
        for (idx, ugen) in self.graph.node_references() {
            if ugen
                .output_sends
                .iter()
                .any(|(_, w)| w.target > 0.0 || w.current > 0.0)
            {
                nodes_to_listen_to.push(idx);
            }
        }
        if (self.current_listening_nodes.len() == nodes_to_listen_to.len())
            && nodes_to_listen_to
                .iter()
                .all(|n| self.current_listening_nodes.contains(n))
        {
            false
        } else {
            self.current_listening_nodes = nodes_to_listen_to;
            true
        }
    }

    pub fn rnd_connections(&mut self, nodes: &[NodeIndex]) {
        let mut rng = thread_rng();
        let mut shuffled = nodes.to_vec();
        shuffled.shuffle(&mut rng);
        shuffled.iter().for_each(|&idx| {
            self.rnd_connect_if_necessary(idx);
        })
    }

    pub fn get_spec(&self, node: NodeIndex) -> Option<ProcessSpec> {
        self.graph.node_weight(node).map(|u| u.process.spec())
    }

    fn has_two_inputs(&self, node: NodeIndex) -> bool {
        self.get_spec(node)
            .map(|s| s.process_type == ProcessType::TwoInputs)
            .unwrap_or(false)
    }

    pub fn rnd_connect_if_necessary(&mut self, node: NodeIndex) {
        if !self.does_idx_have_sufficient_inputs(node) {
            self.rnd_connect_input(node);
        };
        if !self.does_idx_have_sufficient_outputs(node) {
            self.rnd_connect_output(node);
        }
    }

    fn rnd_connect_output(&mut self, new_node: NodeIndex) {
        let mut rng = thread_rng();
        // output
        if let Some(target) = self.choose_with_input(Some(new_node)) {
            //            let w = rng.gen_range(0.7, 1.0);
            let mut idx: u32 = 0;
            if self.has_two_inputs(target) {
                idx = rng.gen_range(0, 2);
            };
            self.connect(new_node, target, (idx, lag::lag(1.0)));
        }
    }

    fn rnd_connect_input(&mut self, new_node: NodeIndex) {
        let mut rng = thread_rng();
        let mut shuffled: Vec<NodeIndex> = self.graph.node_indices().collect();
        shuffled = shuffled
            .into_iter()
            .filter(|idx| *idx != new_node)
            .collect();
        shuffled.shuffle(&mut rng);

        // input

        self.lacking_input_edges(new_node).iter().for_each(|i| {
            if shuffled.len() > (*i as usize) {
                self.connect(shuffled[(*i as usize)], new_node, (*i, lag::lag(1.0)));
            };
        })

        // if let Some(spec) = self.get_spec(new_node) {
        //     match spec.process_type {
        //         ProcessType::Processor => {
        //             if let Some(source) = shuffled.choose(&mut rng) {
        //                 //                    let w = rng.gen_range(0.7, 1.0);
        //                 self.connect(*source, new_node, (0, lag::lag(1.0)));
        //             }
        //         }
        //         ProcessType::TwoInputs | ProcessType::MultipleInputs => {
        // 	    let lacking = self.lacking_input_edges.iter().for_each()
        //             // let w1 = rng.gen_range(0.7, 1.0);
        //             // let w2 = rng.gen_range(0.7, 1.0);
        //             if shuffled.len() > 0 {
        //                 self.connect(shuffled[0], new_node, (0, lag::lag(1.0)));
        //             };
        //             if shuffled.len() > 1 {
        //                 self.connect(shuffled[1], new_node, (1, lag::lag(1.0)));
        //             }
        //         }
        //         ProcessType::NoInputGenerator => (),
        //     }
        // }
    }

    pub fn rnd_circle(&mut self, nodes: &[NodeIndex], n_connections: u32) -> Vec<EdgeIndex> {
        let mut rng = thread_rng();
        let mut shuffled = nodes.to_vec();
        shuffled.shuffle(&mut rng);
        let mut edges = Vec::new();
        for _i in 0..n_connections {
            let filtered_shuffled: Vec<&NodeIndex> = shuffled
                .iter()
                .filter(|&idx| self.does_idx_have_input(*idx))
                .collect();
            let length = filtered_shuffled.len();
            for (i, &idx) in filtered_shuffled.iter().enumerate() {
                //                let w1 = rng.gen_range(0.7, 1.0);
                edges.push(self.connect(
                    *idx,
                    *filtered_shuffled[(i + 1) % length],
                    (0, lag::lag(1.0)),
                ));
            }

            //            let w2 = rng.gen_range(0.7, 1.0);
            let generators: Vec<&NodeIndex> = shuffled
                .iter()
                .filter(|&idx| !self.does_idx_have_input(*idx))
                .collect();
            for &idx in generators.iter() {
                edges.push(self.connect(
                    *idx,
                    **filtered_shuffled.choose(&mut rng).unwrap(),
                    (0, lag::lag(1.0)),
                ))
            }
        }
        edges
    }

    fn does_idx_have_input(&self, node: NodeIndex) -> bool {
        if let Some(u) = self.graph.node_weight(node) {
            !(u.process.spec().process_type == ProcessType::NoInputGenerator)
        } else {
            false
        }
    }

    fn lacking_input_edges(&self, node: NodeIndex) -> Vec<u32> {
        let mut lacking_indices: Vec<u32> = vec![];
        if let Some(u) = self.graph.node_weight(node) {
            let incoming_edges: Vec<u32> = self
                .graph
                .edges_directed(node, Incoming)
                .map(|e| {
                    let (idx, _) = e.weight();
                    *idx
                })
                .collect();
            match u.process.spec().process_type {
                ProcessType::NoInputGenerator => (),
                ProcessType::TwoInputs | ProcessType::MultipleInputs => {
                    if !incoming_edges.contains(&0) {
                        lacking_indices.push(0);
                    }
                    if !incoming_edges.contains(&1) {
                        lacking_indices.push(1);
                    }
                }
                ProcessType::Processor => {
                    if !incoming_edges.contains(&0) {
                        lacking_indices.push(0);
                    }
                }
            }
            lacking_indices
        } else {
            lacking_indices
        }
    }

    fn does_idx_have_sufficient_inputs(&self, node: NodeIndex) -> bool {
        self.lacking_input_edges(node).is_empty()
        // if let Some(u) = self.graph.node_weight(node) {
        //     let n = self.graph.neighbors_directed(node, Incoming).count();
        //     match u.process.spec().process_type {
        //         ProcessType::NoInputGenerator => true,
        //         ProcessType::TwoInputs | ProcessType::MultipleInputs => n >= 2,
        //         ProcessType::Processor => n >= 1,
        //     }
        // } else {
        //     false
        // }
    }

    fn does_idx_have_sufficient_outputs(&self, node: NodeIndex) -> bool {
        let n = self.graph.neighbors_directed(node, Outgoing).count();
        n > 0
    }

    // guarantees that choosen idx is a process with input
    fn choose_with_input(&self, exclude: Option<NodeIndex>) -> Option<NodeIndex> {
        let mut rng = thread_rng();
        let with_input: Vec<NodeIndex> = self
            .graph
            .node_references()
            .filter_map(|(idx, u)| {
                if !(u.process.spec().process_type == ProcessType::NoInputGenerator) {
                    match exclude {
                        Some(ex) => {
                            if ex != idx {
                                Some(idx)
                            } else {
                                None
                            }
                        }
                        None => Some(idx),
                    }
                } else {
                    None
                }
            })
            .collect();
        with_input.choose(&mut rng).map(|r| *r)
    }

    fn collect_components(&self) -> Vec<Vec<NodeIndex>> {
        let mut sets = Vec::new();

        for node in self.graph.node_indices() {
            let mut bfs = Bfs::new(&self.graph, node);

            let mut all_neighbors = Vec::new();
            while let Some(nx) = bfs.next(&self.graph) {
                all_neighbors.push(nx)
            }

            let intersects_with = sets.iter().position(|set: &Vec<NodeIndex>| {
                all_neighbors.iter().any(|neighbor| set.contains(neighbor))
            });

            match intersects_with {
                Some(idx) => {
                    sets[idx].append(&mut all_neighbors);
                }
                None => sets.push(all_neighbors),
            }

            for s in sets.iter_mut() {
                s.sort();
                s.dedup();
            }
        }

        sets
    }

    fn nodes_with_neighbors(&self) -> Vec<(NodeIndex, Vec<NodeIndex>)> {
        let mut result = Vec::new();
        for node in self.graph.node_indices() {
            let neighbors = self.graph.neighbors_undirected(node).collect();
            result.push((node, neighbors));
        }
        result.sort_by(
            |(_, a): &(NodeIndex, Vec<NodeIndex>), (_, b): &(NodeIndex, Vec<NodeIndex>)| {
                a.len().partial_cmp(&b.len()).unwrap()
            },
        );
        result
    }

    pub fn connect_least_connected(&mut self) {
        let nodes = self.nodes_with_neighbors();
        if let Some((first, _)) = nodes.first() {
            if let Some((future_neighbor, _)) = nodes
                .iter()
                .skip(1)
                .filter(|(_, neighbors)| !(neighbors.contains(first)))
                .next()
            {
                //let w = thread_rng().gen_range(0.7, 1.0);
                self.connect(*first, *future_neighbor, (0, lag::lag(1.0)));
            }
        }
    }

    pub fn disconnect_most_connected(&mut self) {
        let nodes = self.nodes_with_neighbors();
        if let Some((last, _)) = nodes.last() {
            if let Some((neighbor, _)) = nodes
                .iter()
                .rev()
                .skip(1)
                .filter(|(_, neighbors)| neighbors.contains(last))
                .next()
            {
                if let Some((e, _)) = self.graph.find_edge_undirected(*last, *neighbor) {
                    self.graph.remove_edge(e);
                }
            }
        }
    }

    pub fn ensure_connectivity(&mut self) {
        // first do an rnd connect to ensure everyone has sufficient inputs/outputs
        let indices: Vec<NodeIndex> = self.graph.node_indices().collect();
        indices
            .iter()
            .for_each(|idx| self.rnd_connect_if_necessary(*idx));
        let components = self.collect_components();
        let mut rng = thread_rng();
        if let Some((first, rest)) = components.split_first() {
            for disconnected_node in rest.iter() {
                //            let w1 = rng.gen_range(0.7, 1.0);
                //           let w2 = rng.gen_range(0.7, 1.0);
                self.connect(
                    disconnected_node[0],
                    *first.choose(&mut rng).unwrap(),
                    (0, lag::lag(1.0)),
                );
                self.connect(
                    *first.choose(&mut rng).unwrap(),
                    disconnected_node[0],
                    (0, lag::lag(2.0)),
                );
            }
        }
    }

    pub fn establish_flow(&mut self) -> Vec<NodeIndex> {
        let _ = self.update_listening_nodes();
        let mut to_visit = self.current_listening_nodes.to_owned();
        let mut visited: Vec<NodeIndex> = to_visit.to_owned();

        while !to_visit.is_empty() {
            match to_visit.pop() {
                Some(current_node) => self
                    .graph
                    .neighbors_directed(current_node, Incoming)
                    .for_each(|neighbor| {
                        if !visited.contains(&neighbor) {
                            visited.push(neighbor);
                            match self.graph.node_weight(neighbor) {
                                Some(fb) => {
                                    if !fb.feedback_delay {
                                        to_visit.insert(0, neighbor);
                                    }
                                }
                                None => (),
                            }
                        }
                    }),
                None => break,
            }
        }
        visited.reverse();
        visited
    }

    pub fn update_connections_and_flow(&mut self, flow: &mut Vec<NodeIndex>) {
        self.ensure_connectivity();
        *flow = self.establish_flow();
    }

    /// Calls a ugen and sends result to connected ugens
    pub fn call_and_output(&mut self, idx: NodeIndex, input: &[f64]) {
        match self.graph.node_weight_mut(idx) {
            Some(ugen) => {
                let output = ugen.process(input);
                let mut neighbors = self.graph.neighbors_directed(idx, Outgoing).detach();
                while let Some(neighbor_idx) = neighbors.next_node(&self.graph) {
                    let edge = self
                        .graph
                        .find_edge(idx, neighbor_idx)
                        .and_then(|e| self.graph.edge_weight(e))
                        .map(|e| {
                            let (idx, w) = &*e;
                            (*idx, w.current)
                        }); // deref to stop borrowing
                    match edge {
                        Some(connection) => {
                            self.graph[neighbor_idx].set_input_with_connection(connection, output)
                        }
                        None => (),
                    }
                }
            }
            None => (),
        }
    }

    pub fn process(
        &mut self,
        flow: &Vec<NodeIndex>,
        input_buffer: &[f64],
        output_buffer: &mut [f64],
    ) {
        flow.iter()
            .for_each(|&idx| self.call_and_output(idx, input_buffer));

        output_buffer.iter_mut().for_each(|x| *x = 0.0);

        for ugen in self.graph.node_weights_mut() {
            let current_value = ugen.value.unwrap_or(0.0);
            for (out_i, amp_out) in ugen.output_sends.iter_mut() {
                output_buffer[*out_i % output_buffer.len()] += current_value * amp_out.tick();
            }
        }
        for &i in flow {
            match self.graph.node_weight_mut(i) {
                Some(n) => {
                    n.reset();
                }
                None => (),
            }
        }
        for e in self.graph.edge_weights_mut() {
            let (_, w) = e;
            w.tick();
            ()
        }
    }
}

// UGENS
// pub fn add() -> UGen {
//     UGen::new(Process::Add { inputs: Vec::new() })
// }

// pub fn mul() -> UGen {
//     UGen::new(Process::Mul { inputs: Vec::new() })
// }

// pub fn square() -> UGen {
//     UGen::new(Process::Square { input: 0.0 })
// }

// pub fn sound_in(index: usize) -> UGen {
//     UGen::new(Process::SoundIn {
//         input: 0.0,
//         index,
//         factor: lag::lag(1.0),
//     })
// }

// pub fn sqrt() -> UGen {
//     UGen::new(Process::Sqrt { input: 0.0 })
// }

// fn lpf2000() -> Process {
//     filter(filters::FilterType::BLPF, 2000.0, 6.0)
// }

// pub fn rms() -> UGen {
//     UGen::new(rms_proc())
// }

// pub fn spike(threshold: f64, t_const: f64, r: f64, t_rest: usize) -> UGen {
//     UGen::new(Process::Spike {
//         input: 0.0,
//         v: 0.0,
//         last_v: 0.0,
//         threshold: lag::lag(threshold),
//         t_const: lag::lag(t_const),
//         r: lag::lag(r),
//         t_rest,
//         t_this_rest: t_rest,
//         t_rest_counter: 0,
//     })
// }

// pub fn mem(init: f64) -> UGen {
//     UGen::new(Process::Mem {
//         input: init,
//         last_value: 0.0,
//     })
// }

// pub fn constant(v: f64) -> UGen {
//     UGen::new(Process::Constant { value: v })
// }

// pub fn sin() -> UGen {
//     UGen::new(Process::Sin {
//         input: 0.0,
//         mul: lag::lag(1.0),
//     })
// }

// pub fn sin_osc(freq: f64, freq_mul: f64) -> UGen {
//     UGen::new(Process::SinOsc {
//         input: 0.0,
//         freq: lag::lag(freq),
//         freq_mul: lag::lag(freq_mul),
//         phase: 0.0,
//     })
// }

// fn filter(filter_type: filters::FilterType, freq: f64, q: f64) -> Process {
//     unsafe {
//         Process::Filter {
//             input: 0.0,
//             filter: filters::Biquad::new(filter_type, freq, q, SR),
//         }
//     }
// }

// pub fn lpf(freq: f64, q: f64) -> UGen {
//     UGen::new(filter(filters::FilterType::BLPF, freq, q))
// }

// pub fn resonator(freq: f64, decay: f64) -> UGen {
//     UGen::new(Process::Resonator {
//         input: 0.0,
//         freq_mod: 0.0,
//         freq_center: lag::lag(freq),
//         freq_factor: lag::lag(100.0),
//         resonator: filters::ComplexRes::new(freq, decay, get_sr()),
//     })
// }

// pub fn ring() -> UGen {
//     UGen::new(Process::Ring {
//         inputs: Vec::new(),
//         input_counter: 0,
//     })
// }

// pub fn hpf(freq: f64, q: f64) -> UGen {
//     UGen::new(filter(filters::FilterType::BHPF, freq, q))
// }

// pub fn bpf(freq: f64, q: f64) -> UGen {
//     UGen::new(filter(filters::FilterType::BBPF, freq, q))
// }

// pub fn noise(seed: u64) -> UGen {
//     UGen::new(Process::Noise {
//         rng: SeedableRng::seed_from_u64(seed),
//     })
// }

// pub fn ducking() -> UGen {
//     UGen::new(Process::Ducking {
//         input: 0.0,
//         input_level: vec![],
//         output_factor: ducking_lag(),
//     })
// }

// pub fn env_follow() -> UGen {
//     UGen::new(Process::EnvFollow {
//         input: 0.0,
//         input_level: vec![],
//     })
// }

// pub fn pll() -> UGen {
//     UGen::new(Process::PLL {
//         input: 0.0,
//         factor: lag::lag(0.25),
//         phase: 0.0,
//         error: pll_error_lag(),
//     })
// }

// fn sinosc(freq: f64) -> Process {
//     Process::SinOsc {
//         input: 0.0,
//         freq: lag::lag(freq),
//         phase: 0.0,
//     }
// }

// fn map_process(func: fn(f64) -> f64) -> UGen {
//     UGen::new(Process::Map {
//         input: 0.0,
//         func: func,
//     })
// }

// pub fn wrap(lo: f64, hi: f64) -> UGen {
//     UGen::new(Process::Wrap { input: 0.0, lo, hi })
// }

// pub fn softclip() -> UGen {
//     UGen::new(Process::Softclip { input: 0.0 })
// }

// pub fn compressor(threshold: f64, ratio: f64, makeup: f64) -> UGen {
//     UGen::new(Process::Compressor {
//         input: 0.0,
//         input_level: vec![],
//         threshold: lag::lag(threshold),
//         ratio: lag::lag(ratio),
//         makeup: lag::lag(makeup),
//     })
// }

// pub fn bitneg() -> UGen {
//     UGen::new(Process::BitNeg { input: 0.0 })
// }

// pub fn gauss() -> UGen {
//     UGen::new(Process::Gauss { input: 0.0 })
// }

// pub fn lpf1(freq: f64) -> UGen {
//     UGen::new(Process::LPF1 {
//         input: 0.0,
//         freq: lag::lag(freq),
//         p: lpf1_calc_p(freq),
//         last_out: 0.0,
//     })
// }

// pub fn delay(n: usize) -> UGen {
//     UGen::new(Process::Delay {
//         input: vec![0.0; n],
//         rec_idx: 0,
//     })
// }
// pub fn curvelin(in_min: f64, in_max: f64, out_min: f64, out_max: f64) -> UGen {
//     UGen::new(Process::CurveLin {
//         input: 0.0,
//         in_min,
//         in_max,
//         out_min,
//         out_max,
//         curve: -4.,
//     })
// }

// fn fb_delay() -> UGen {
//     UGen {
//         feedback_delay: true,
//         process: Process::Mem {
//             input: 0.0,
//             last_value: 0.0,
//         },
//         value: None,
//         last_value: 0.0,
//         clip: ClipType::None,
//         sum_inputs: true,
//         output_sends: vec![(0, lag::lag(1.0))],
//     }
// }

// pub fn band_pass2(g: &mut UGenGraph, f1: f64, f2: f64, q: f64) -> (NodeIndex, NodeIndex) {
//     let low1 = g.graph.add_node(lpf(f2, q));
//     let low2 = g.graph.add_node(lpf(f2, q));
//     let high1 = g.graph.add_node(hpf(f1, q));
//     let high2 = g.graph.add_node(hpf(f1, q));
//     g.connect(low1, low2, (0, lag::lag(1.0)));
//     g.connect(low2, high1, (0, lag::lag(1.0)));
//     g.connect(high1, high2, (0, lag::lag(1.0)));
//     (low1, high2)
// }

// fn geo_series(n: u32, start: f64, fac: f64) -> Vec<f64> {
//     let mut output = vec![start];
//     let mut last = start;
//     for _i in 0..n {
//         last = last * fac;
//         output.push(last);
//     }
//     output
// }

// fn geom_from_to(n: u32, from: f64, to: f64) -> Vec<f64> {
//     let fac = (to / from).powf(1. / ((n - 1) as f64));
//     geo_series(n, from, fac)
// }

// pub fn filter_bank(
//     g: &mut UGenGraph,
//     n: u32,
//     f1: f64,
//     f2: f64,
//     q: f64,
// ) -> (NodeIndex, Vec<NodeIndex>) {
//     let filters: Vec<(NodeIndex, NodeIndex)> = geom_from_to(n, f1, f2)
//         .windows(2)
//         .map(|pair| band_pass2(g, pair[0], pair[1], q))
//         .collect();
//     let input_sum = g.graph.add_node(add());
//     let outputs = filters
//         .iter()
//         .map(|(input, output)| {
//             g.connect(input_sum, *input, (0, lag::lag(1.0)));
//             *output
//         })
//         .collect();
//     (input_sum, outputs)
// }
