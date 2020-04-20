mod filters;
use petgraph::stable_graph::StableGraph;
use petgraph::stable_graph::*;
use petgraph::Directed;
use petgraph::Direction::*;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use std::f64;
use std::fmt;
pub mod numerical;
use rand::seq::SliceRandom;
use rand::thread_rng;

const PI: f64 = f64::consts::PI;
const SR: f64 = 44100f64;
static FREQ_FAC: f64 = 2.0 * PI / SR;
const DEBUG: bool = false;

// TODO
// rms
// outside input
// lag
// variable delay times
// strength of connection (factor)
// input indices

#[derive(Debug)]
enum Process {
    Sin {
        input: f64,
    },
    SinOsc {
        freq: f64,
        phase: f64,
    },
    Mul {
        inputs: Vec<f64>,
    },
    Add {
        inputs: Vec<f64>,
    },
    Mem {
        input: f64,
        last_value: f64,
    },
    Map {
        input: f64,
        func: fn(f64) -> f64,
    },
    Constant {
        value: f64,
    },
    Filter {
        input: f64,
        filter: filters::Biquad,
    },
    Noise {
        rng: SmallRng,
    },
    Wrap {
        input: f64,
        lo: f64,
        hi: f64,
    },
    Softclip {
        input: f64,
    },
    Delay {
        input: Vec<f64>,
        rec_idx: usize,
    },
    BitNeg {
        input: f64,
    },
    BitOr {
        input1: f64,
        input2: f64,
    },
    BitXOr {
        input1: f64,
        input2: f64,
    },
    BitAnd {
        input1: f64,
        input2: f64,
    },
    CurveLin {
        input: f64,
        in_min: f64,
        in_max: f64,
        out_min: f64,
        out_max: f64,
        curve: f64,
    },
    Gauss {
        input: f64,
    },
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
        Process::Mul { inputs } => inputs.iter().product(),

        Process::Add { inputs } => inputs.iter().sum(),

        // Process::Mem {
        //     input,
        //     ref mut last_value,
        // } => {
        //     let output = *last_value;
        //     *last_value = *input;
        //     output
        // }
        Process::Mem { input, .. } => *input,

        Process::Constant { value } => *value,
        Process::Map { input, func } => (func)(*input),
        Process::Filter {
            input,
            ref mut filter,
        } => filter.process(*input),
        Process::Noise { ref mut rng } => rng.gen_range(-1.0, 1.0),
        Process::Wrap { input, lo, hi } => numerical::wrap(*input, *lo, *hi),
        Process::Softclip { input } => input.tanh(),
        Process::Delay {
            input,
            ref mut rec_idx,
        } => {
            let prev_idx = ((*rec_idx - 1) + input.len()) % input.len();
            let result = input[prev_idx];
            *rec_idx = (*rec_idx + 1) % input.len();
            result
        }
        Process::BitNeg { input } => numerical::bit_neg(*input),
        Process::BitOr { input1, input2 } => numerical::bit_or(*input1, *input2),
        Process::BitXOr { input1, input2 } => numerical::bit_xor(*input1, *input2),
        Process::BitAnd { input1, input2 } => numerical::bit_and(*input1, *input2),
        Process::CurveLin {
            input,
            in_min,
            in_max,
            out_min,
            out_max,
            curve,
        } => numerical::curvelin(*input, *in_min, *in_max, *out_min, *out_max, *curve),
        Process::Gauss { input } => numerical::gauss_curve(*input),
    }
}

fn set_or_add(field: &mut f64, value: f64, add: bool) {
    if add {
        *field += value
    } else {
        *field = value
    }
}

fn set_input(process: &mut Process, idx: u32, input_value: f64, add: bool) {
    match process {
        Process::Sin { ref mut input }
        | Process::Gauss { ref mut input }
        | Process::Softclip { ref mut input }
        | Process::Mem { ref mut input, .. }
        | Process::BitNeg { ref mut input } => set_or_add(input, input_value, add),
        Process::SinOsc { ref mut freq, .. } => set_or_add(freq, input_value, add),
        Process::Mul { ref mut inputs } | Process::Add { ref mut inputs } => {
            inputs.push(input_value)
        }
        Process::Constant { .. } | Process::Noise { .. } => (),
        Process::Map { ref mut input, .. } => set_or_add(input, input_value, add),
        Process::Filter {
            ref mut input,
            ref mut filter,
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => filter.update_coefficients(input_value, filter.q, SR),
            2 => filter.update_coefficients(filter.freq, input_value, SR),
            _ => panic!("wrong index into {}: {}", name(process), idx),
        },
        Process::Wrap {
            // todo use idx
            ref mut input,
            ref mut lo,
            ref mut hi,
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => *lo = input_value,
            2 => *hi = input_value,
            _ => panic!("wrong index into {}: {}", name(process), idx),
        },
        Process::Delay {
            ref mut input,
            rec_idx,
        } => {
            if add {
                input[*rec_idx] += input_value
            } else {
                input[*rec_idx] = input_value
            }
        }
        Process::BitOr {
            ref mut input1,
            ref mut input2,
        }
        | Process::BitXOr {
            ref mut input1,
            ref mut input2,
        }
        | Process::BitAnd {
            ref mut input1,
            ref mut input2,
        } => match idx {
            0 => set_or_add(input1, input_value, add),
            1 => set_or_add(input2, input_value, add),
            _ => panic!("wrong index into {}: {}", name(process), idx),
        },
        Process::CurveLin {
            ref mut input,
            ref mut in_min,
            ref mut in_max,
            ref mut out_min,
            ref mut out_max,
            ref mut curve,
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => *in_min = input_value,
            2 => *in_max = input_value,
            3 => *out_min = input_value,
            4 => *out_max = input_value,
            5 => *curve = input_value,
            _ => panic!("wrong index into {}: {}", name(process), idx),
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
        Process::Wrap { .. } => "wrap",
        Process::Softclip { .. } => "softclip",
        Process::Delay { .. } => "delay",
        Process::BitNeg { .. } => "bitneg",
        Process::BitOr { .. } => "bitor",
        Process::BitXOr { .. } => "bitxor",
        Process::BitAnd { .. } => "bitand",
        Process::CurveLin { .. } => "curvelin",
        Process::Gauss { .. } => "gauss",
    }
}

fn clear_inputs(process: &mut Process) {
    match process {
        Process::Wrap { ref mut input, .. }
        | Process::Filter { ref mut input, .. }
        | Process::Map { ref mut input, .. }
        | Process::CurveLin { ref mut input, .. }
        | Process::Sin { ref mut input }
        | Process::Gauss { ref mut input }
        | Process::Softclip { ref mut input }
        | Process::Mem { ref mut input, .. }
        | Process::BitNeg { ref mut input } => *input = 0.0,
        Process::SinOsc { ref mut freq, .. } => *freq = 0.0,
        Process::Mul { ref mut inputs } | Process::Add { ref mut inputs } => inputs.clear(),
        Process::Constant { .. } => (),
        Process::Noise { .. } => (),
        Process::Delay {
            ref mut input,
            rec_idx,
        } => input[*rec_idx] = 0.0,
        Process::BitOr {
            ref mut input1,
            ref mut input2,
        }
        | Process::BitXOr {
            ref mut input1,
            ref mut input2,
        }
        | Process::BitAnd {
            ref mut input1,
            ref mut input2,
        } => {
            *input1 = 0.0;
            *input2 = 0.0;
        }
    }
}

#[derive(Debug)]
pub enum ClipType {
    None,
    SoftClip,
    Wrap,
}

pub struct UGen {
    feedback_delay: bool,
    process: Process,
    value: Option<f64>,
    sum_inputs: bool, // TODO implement
    clip: ClipType,
}

impl UGen {
    fn new(process: Process) -> Self {
        UGen {
            feedback_delay: false,
            process,
            value: None,
            sum_inputs: true,
            clip: ClipType::None,
        }
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
}

impl UGen {
    fn process(&mut self) -> f64 {
        match self.value {
            Some(v) => v,
            None => {
                let v = process(&mut self.process);
                let output = match self.clip {
                    ClipType::None => v,
                    ClipType::SoftClip => v.tanh(),
                    ClipType::Wrap => numerical::wrap(v, -1., 1.),
                };
                if DEBUG {
                    println!("processing: [{:?}] with result {}", self, output)
                }
                self.value = Some(output);
                clear_inputs(&mut self.process);
                output
            }
        }
    }

    fn reset(&mut self) {
        self.value = None
    }

    fn set_input(&mut self, idx: u32, value: f64) {
        if DEBUG {
            println!("setting input of [{:?}] to {}", self, value)
        }
        set_input(&mut self.process, idx, value, self.sum_inputs)
    }
}

impl fmt::Debug for UGen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "UGenData: name: {}, value: {:?}, process: {:?}, clip: {:?}, sum_inputs: {:?}",
            //self.feedback_delay,
            name(&self.process),
            self.value,
            self.process,
            self.clip,
            self.sum_inputs
        )
    }
}

pub fn add() -> UGen {
    UGen::new(Process::Add { inputs: Vec::new() })
}

pub fn mul() -> UGen {
    UGen::new(Process::Mul { inputs: Vec::new() })
}

pub fn mem(init: f64) -> UGen {
    UGen::new(Process::Mem {
        input: init,
        last_value: 0.0,
    })
}

pub fn constant(v: f64) -> UGen {
    UGen::new(Process::Constant { value: v })
}

pub fn sin() -> UGen {
    UGen::new(Process::Sin { input: 0.0 })
}

pub fn sin_osc(freq: f64) -> UGen {
    UGen::new(Process::SinOsc { freq, phase: 0.0 })
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

pub fn wrap(lo: f64, hi: f64) -> UGen {
    UGen::new(Process::Wrap { input: 0.0, lo, hi })
}

pub fn softclip() -> UGen {
    UGen::new(Process::Softclip { input: 0.0 })
}

pub fn bitneg() -> UGen {
    UGen::new(Process::BitNeg { input: 0.0 })
}

pub fn gauss() -> UGen {
    UGen::new(Process::Gauss { input: 0.0 })
}

pub fn delay(n: usize) -> UGen {
    UGen::new(Process::Delay {
        input: vec![0.0; n],
        rec_idx: 0,
    })
}
pub fn curvelin(in_min: f64, in_max: f64, out_min: f64, out_max: f64) -> UGen {
    UGen::new(Process::CurveLin {
        input: 0.0,
        in_min,
        in_max,
        out_min,
        out_max,
        curve: -4.,
    })
}

fn fb_delay() -> UGen {
    UGen {
        feedback_delay: true,
        process: Process::Mem {
            input: 0.0,
            last_value: 0.0,
        },
        value: None,
        clip: ClipType::None,
        sum_inputs: true,
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

pub fn rnd_connections(
    g: &mut UGenGraph,
    nodes: &[NodeIndex],
    n_connections: u32,
) -> Vec<EdgeIndex> {
    let mut rng = thread_rng();
    let mut shuffled = nodes.to_vec();
    shuffled.shuffle(&mut rng);
    let mut edges = Vec::new();
    for _i in 0..n_connections {
        shuffled.iter().for_each(|&idx| {
            edges.push(g.add_edge(idx, *shuffled.choose(&mut rng).unwrap(), 0));
            edges.push(g.add_edge(*shuffled.choose(&mut rng).unwrap(), idx, 0));
        })
    }
    edges
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
