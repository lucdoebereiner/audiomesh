mod filters;
use petgraph::stable_graph::StableGraph;
use petgraph::stable_graph::*;
use petgraph::Directed;
use petgraph::Direction::*;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use std::f64;
use std::fmt;

const PI: f64 = f64::consts::PI;
const SR: f64 = 44100f64;
static FREQ_FAC: f64 = 2.0 * PI / SR;
const DEBUG: bool = false;

#[derive(Debug)]
enum Process {
    Sin { input: f64 },
    SinOsc { freq: f64, phase: f64 },
    Mul { inputs: Vec<f64> },
    Add { inputs: Vec<f64> },
    Mem { value: f64 },
    Map { input: f64, func: fn(f64) -> f64 },
    Constant { value: f64 },
    Filter { input: f64, filter: filters::Biquad },
    Noise { rng: SmallRng },
}

fn process(process: &mut Process) -> f64 {
    match process {
        Process::Sin { input } => input.sin(),
        Process::SinOsc {
            freq,
            ref mut phase,
        } => {
            let output = phase.sin();
            *phase += *freq * FREQ_FAC;
            output
        }
        Process::Mul { inputs } => {
            let result = inputs.iter().product();
            inputs.clear();
            result
        }
        Process::Add { inputs } => {
            let result = inputs.iter().sum();
            inputs.clear();
            result
        }
        Process::Mem { value } => *value,
        Process::Constant { value } => *value,
        Process::Map { input, func } => (func)(*input),
        Process::Filter {
            input,
            ref mut filter,
        } => filter.process(*input),
        Process::Noise { ref mut rng } => rng.gen_range(-1.0, 1.0),
    }
}

fn set_input(process: &mut Process, idx: u32, input_value: f64) {
    match process {
        Process::Sin { ref mut input } => *input = input_value,
        Process::SinOsc { ref mut freq, .. } => *freq = input_value,
        Process::Mul { ref mut inputs } | Process::Add { ref mut inputs } => {
            inputs.push(input_value)
        }
        Process::Mem { ref mut value } => *value = input_value,
        Process::Constant { .. } | Process::Noise { .. } => (),
        Process::Map { ref mut input, .. } => *input = input_value,
        Process::Filter {
            ref mut input,
            ref mut filter,
        } => match idx {
            0 => *input = input_value,
            1 => filter.update_coefficients(input_value, filter.q, SR),
            2 => filter.update_coefficients(filter.freq, input_value, SR),
            _ => panic!("wrong index into filter: {}", idx),
        },
    }
}

fn name(process: &Process) -> &'static str {
    match process {
        Process::Sin { .. } => "sin",
        Process::SinOsc { .. } => "sinosc",
        Process::Mul { .. } => "mul",
        Process::Add { .. } => "add",
        Process::Mem { .. } => "mem",
        Process::Constant { .. } => "constant",
        Process::Map { .. } => "map",
        Process::Filter { .. } => "filter",
        Process::Noise { .. } => "noise",
    }
}

pub struct UGen {
    feedback_delay: bool,
    process: Process,
    value: Option<f64>,
}

impl UGen {
    fn new(process: Process) -> UGen {
        UGen {
            feedback_delay: false,
            process,
            value: None,
        }
    }
}

impl UGen {
    fn process(&mut self) -> f64 {
        match self.value {
            Some(v) => v,
            None => {
                let v = process(&mut self.process);
                self.value = Some(v);
                v
            }
        }
    }

    fn reset(&mut self) {
        self.value = None
    }

    fn set_input(&mut self, idx: u32, value: f64) {
        set_input(&mut self.process, idx, value)
    }
}

impl fmt::Debug for UGen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "UGenData: fb_del: {}, name: {}, value: {:?}, process: {:?}",
            self.feedback_delay,
            name(&self.process),
            self.value,
            self.process
        )
    }
}

pub fn add() -> UGen {
    UGen::new(Process::Add { inputs: Vec::new() })
}

pub fn mul() -> UGen {
    UGen::new(Process::Add { inputs: Vec::new() })
}

pub fn mem() -> UGen {
    UGen::new(Process::Mem { value: 0.0 })
}

pub fn constant(v: f64) -> UGen {
    UGen::new(Process::Constant { value: v })
}

pub fn sin() -> UGen {
    UGen::new(Process::Sin { input: 0.0 })
}

fn filter(filter_type: filters::FilterType, freq: f64, q: f64) -> Process {
    Process::Filter {
        input: 0.0,
        filter: filters::Biquad::new(filter_type, freq, q, SR),
    }
}

pub fn lpf(freq: f64, q: f64) -> UGen {
    UGen::new(filter(filters::FilterType::BLPF, freq, q))
}

pub fn hpf(freq: f64, q: f64) -> UGen {
    UGen::new(filter(filters::FilterType::BHPF, freq, q))
}

pub fn bpf(freq: f64, q: f64) -> UGen {
    UGen::new(filter(filters::FilterType::BBPF, freq, q))
}

pub fn noise(seed: u64) -> UGen {
    UGen::new(Process::Noise {
        rng: SeedableRng::seed_from_u64(seed),
    })
}

fn sinosc(freq: f64) -> Process {
    Process::SinOsc {
        freq: freq,
        phase: 0.0,
    }
}

fn map_process(func: fn(f64) -> f64) -> Process {
    Process::Map {
        input: 0.0,
        func: func,
    }
}

fn fb_delay() -> UGen {
    UGen {
        feedback_delay: true,
        process: Process::Mem { value: 0.0 },
        value: None,
    }
}

pub type UGenGraph = StableGraph<UGen, u32, Directed, DefaultIx>;

pub fn new_graph() -> UGenGraph {
    StableGraph::with_capacity(100, 100)
}

pub fn band_pass2(g: &mut UGenGraph, f1: f64, f2: f64, q: f64) -> (NodeIndex, NodeIndex) {
    let low1 = g.add_node(lpf(f2, q));
    let low2 = g.add_node(lpf(f2, q));
    let high1 = g.add_node(hpf(f1, q));
    let high2 = g.add_node(hpf(f1, q));
    g.add_edge(low1, low2, 0);
    g.add_edge(low2, high1, 0);
    g.add_edge(high1, high2, 0);
    (low1, high2)
}

fn geo_series(n: u32, start: f64, fac: f64) -> Vec<f64> {
    let mut output = vec![start];
    let mut last = start;
    for _i in 0..n {
        last = last * fac;
        output.push(last);
    }
    output
}

fn geom_from_to(n: u32, from: f64, to: f64) -> Vec<f64> {
    let fac = (to / from).powf(1. / ((n - 1) as f64));
    geo_series(n, from, fac)
}

pub fn filter_bank(
    g: &mut UGenGraph,
    n: u32,
    f1: f64,
    f2: f64,
    q: f64,
) -> (NodeIndex, Vec<NodeIndex>) {
    let filters: Vec<(NodeIndex, NodeIndex)> = geom_from_to(n, f1, f2)
        .windows(2)
        .map(|pair| band_pass2(g, pair[0], pair[1], q))
        .collect();
    let input_sum = g.add_node(add());
    let outputs = filters
        .iter()
        .map(|(input, output)| {
            g.add_edge(input_sum, *input, 0);
            *output
        })
        .collect();
    (input_sum, outputs)
}

// /// Inserts additional nodes and edges for recursive connections
// fn insert_fb_nodes(
//     graph: &mut UGenGraph,
//     mut visited: Vec<NodeIndex>,
//     mut to_visit: Vec<NodeIndex>,
// ) {
//     //    println!("\nto_visit: {:?}", to_visit);
//     let mut to_visit_next: Vec<NodeIndex> = Vec::new();
//     match to_visit.pop() {
//         Some(current_node) => {
//             let incoming = graph
//                 .neighbors_directed(current_node, Incoming)
//                 .filter(|&idx| match graph.node_weight(idx) {
//                     Some(n) => !n.feedback_delay,
//                     None => false,
//                 })
//                 .collect::<Vec<_>>();
//             incoming.iter().for_each(|&neighbor| {
//                 // feedback
//                 // println!("current node: {:?}, neighbor: {:?}", current_node, neighbor);
//                 if visited.contains(&neighbor) || current_node == neighbor {
//                     match graph.find_edge(neighbor, current_node) {
//                         Some(fb_edge) => {
//                             // remove edge and insert new feedback edges and node
//                             graph.remove_edge(fb_edge);
//                             let fb_node = graph.add_node(fb_delay());
//                             graph.add_edge(neighbor, fb_node, 0);
//                             graph.add_edge(fb_node, current_node, 0);
//                         }
//                         None => (),
//                     }
//                 }
//                 visited.push(current_node);
//                 // add nodes to visit next
//                 if !to_visit.contains(&neighbor) && !visited.contains(&neighbor) {
//                     to_visit_next.push(neighbor)
//                 }
//             });
//         }
//         None => return,
//     }
//     //    println!("to_visit_next: {:?}", to_visit_next);
//     // recursion, add nodes to visit and pop current node
//     to_visit_next.append(&mut to_visit);
//     insert_fb_nodes(graph, visited, to_visit_next);
// }

pub fn establish_flow(graph: &UGenGraph, start_nodes: &[NodeIndex]) -> Vec<NodeIndex> {
    let mut to_visit = start_nodes.to_vec();
    let mut visited: Vec<NodeIndex> = start_nodes.to_vec();

    while !to_visit.is_empty() {
        match to_visit.pop() {
            Some(current_node) => {
                graph
                    .neighbors_directed(current_node, Incoming)
                    .for_each(|neighbor| {
                        if !visited.contains(&neighbor) {
                            visited.push(neighbor);
                            match graph.node_weight(neighbor) {
                                Some(fb) => {
                                    if !fb.feedback_delay {
                                        to_visit.insert(0, neighbor);
                                    }
                                }
                                None => (),
                            }
                        }
                    })
            }
            None => break,
        }
    }
    visited.reverse();
    visited
}

/// Calls a ugen and sends result to connected ugens
pub fn call_and_output(graph: &mut UGenGraph, idx: NodeIndex) {
    match graph.node_weight_mut(idx) {
        Some(ugen) => {
            let output = ugen.process();
            if DEBUG {
                println!("{:?}", ugen)
            };
            let mut neighbors = graph.neighbors_directed(idx, Outgoing).detach();
            while let Some(neighbor_idx) = neighbors.next_node(graph) {
                graph[neighbor_idx].set_input(0, output);
            }
        }
        None => (),
    }
}

pub fn process_graph(
    graph: &mut UGenGraph,
    flow: &Vec<NodeIndex>,
    listening_nodes: &[NodeIndex],
    output_buffer: &mut [f64],
) {
    flow.iter().for_each(|&idx| call_and_output(graph, idx));
    for (i, &listening_node) in listening_nodes.iter().enumerate() {
        let output = match graph.node_weight(listening_node) {
            Some(node) => node.value.unwrap_or(0.0),
            None => 0.0,
        };
        output_buffer[i] = output;
    }
    for &i in flow {
        match graph.node_weight_mut(i) {
            Some(n) => {
                n.reset();
            }
            None => (),
        }
    }
}
