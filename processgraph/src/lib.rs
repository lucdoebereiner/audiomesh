mod filters;
pub mod lag;
//use petgraph::stable_graph::StableGraph;
//use petgraph::graph::Graph;
use petgraph::stable_graph::*;
use petgraph::Directed;
use petgraph::Direction::*;
//use rand::rngs::SmallRng;
use rand::Rng;
use serde::{Deserializer, Serializer};
use std::f64;
use std::fmt;
pub mod numerical;
use petgraph::visit::Bfs;
use rand::seq::SliceRandom;
use rand::thread_rng;
use serde::{Deserialize, Serialize};

const PI: f64 = f64::consts::PI;
const SR: f64 = 48000f64;
static FREQ_FAC: f64 = 2.0 * PI / SR;
const DEBUG: bool = false;

// TODO
// move debug flag to main
// softclip on filter input optional
// variable delay times
// input indices
// kuramoto
// lag
// zip

// DONE
// not necessary
// remember node index in ugen
// outside input
// process with inner chained processes (rms, filterbank etc)

#[derive(Serialize, Deserialize, Debug)]
pub enum Process {
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
    Square {
        input: f64,
    },
    Sqrt {
        input: f64,
    },
    SoundIn {
        index: usize,
    },

    // Map {
    //     input: f64,
    //     func: fn(f64) -> f64,
    // },
    Constant {
        value: f64,
    },
    Filter {
        input: f64,
        filter: filters::Biquad,
    },
    // Noise {
    //     rng: SmallRng,
    // },
    Wrap {
        input: f64,
        lo: f64,
        hi: f64,
    },
    Softclip {
        input: f64,
    },
    Delay {
        #[serde(serialize_with = "vector_serialize")]
        #[serde(deserialize_with = "vector_deserialize")]
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
    LPF1 {
        input: f64,
        p: f64,
        last_out: f64,
    },
    RMS {
        input: f64,
        chain: Vec<Process>,
    },
}

fn vector_serialize<S, T>(x: &Vec<T>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    s.serialize_u32(x.len() as u32)
}

fn vector_deserialize<'de, D>(deserializer: D) -> Result<Vec<f64>, D::Error>
where
    D: Deserializer<'de>,
{
    let i: usize = <usize as Deserialize>::deserialize(deserializer)?;
    Ok(vec![0.0f64; i])
}

// index always 0, weight always 1
fn process_chain(chain: &mut [Process], input_value: f64, input: &[f64]) -> f64 {
    let mut prev_output = input_value;
    for p in chain {
        set_input(p, 0, prev_output, false);
        prev_output = process(p, input);
    }
    prev_output
}

fn clear_chain(chain: &mut [Process]) {
    for p in chain {
        clear_inputs(p);
    }
}

fn process(process: &mut Process, external_input: &[f64]) -> f64 {
    match process {
        Process::SoundIn { index } => *external_input.get(*index).unwrap_or(&0.0),
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
        Process::Square { input } => input.powf(2.0),
        Process::Sqrt { input } => input.sqrt(),
        Process::Mem { input, .. } => *input,
        Process::Constant { value } => *value,
        //        Process::Map { input, func } => (func)(*input),
        Process::Filter {
            input,
            ref mut filter,
        } => filter.process((*input).tanh()),
        //        Process::Noise { ref mut rng } => rng.gen_range(-1.0, 1.0),
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
        Process::LPF1 {
            input,
            p,
            ref mut last_out,
        } => {
            let output = ((1. - *p) * *input) + (*p * *last_out);
            *last_out = output;
            output
        }
        Process::RMS {
            input,
            ref mut chain,
        } => process_chain(chain, *input, external_input),
    }
}

fn set_or_add(field: &mut f64, value: f64, add: bool) {
    if add {
        *field += value
    } else {
        *field = value
    }
}

pub fn set_parameter(graph: &mut UGenGraph, node_idx: NodeIndex, idx: u32, input_value: f64) {
    if let Some(node) = graph.node_weight_mut(node_idx) {
        set_input(&mut node.process, idx as u32, input_value, false)
    }
}

fn set_input(process: &mut Process, idx: u32, input_value: f64, add: bool) {
    match process {
        Process::Sin { ref mut input }
        | Process::Gauss { ref mut input }
        | Process::Softclip { ref mut input }
	| Process::Square { ref mut input }
	| Process::Sqrt { ref mut input }
        | Process::Mem { ref mut input, .. }
	| Process::RMS { ref mut input, .. }
        | Process::BitNeg { ref mut input } => set_or_add(input, input_value, add),
        Process::SinOsc { ref mut freq, .. } => set_or_add(freq, input_value, add),
        Process::Mul { ref mut inputs } | Process::Add { ref mut inputs } => {
            inputs.push(input_value)
        }
        Process::Constant { .. } | Process::SoundIn { .. }
	//| Process::Noise { .. }
	=> (),
        //        Process::Map { ref mut input, .. } => set_or_add(input, input_value, add),
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
        Process::LPF1 {
            ref mut input,
            ref mut p,
            ..
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => *p = lpf1_calc_p(input_value), // freq input
            _ => panic!("wrong index into {}: {}", name(process), idx),
        },
    }
}

fn lpf1_calc_p(freq: f64) -> f64 {
    1. - (2. * (freq / SR).tan())
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum InputType {
    Any,
    Audio,
    Frequency,
    Q,
    Phase,
    Index,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProcessSpec {
    name: String,
    inputs: Vec<InputType>,
}

fn procspec(name: &'static str, inputs: Vec<InputType>) -> ProcessSpec {
    ProcessSpec {
        name: name.to_string(),
        inputs,
    }
}

fn name(process: &Process) -> String {
    spec(process).name
}

fn spec(process: &Process) -> ProcessSpec {
    match process {
        Process::SoundIn { .. } => procspec("soundin", vec![InputType::Any]),
        Process::Sin { .. } => procspec("sin", vec![InputType::Phase]),
        Process::SinOsc { .. } => procspec("sinosc", vec![InputType::Frequency]),
        Process::Mul { .. } => procspec("mul", vec![InputType::Any]),
        Process::Add { .. } => procspec("add", vec![InputType::Any]),
        Process::Mem { .. } => procspec("mem", vec![InputType::Any]),
        Process::Constant { .. } => procspec("constant", vec![InputType::Any]),
        //        Process::Map { .. } => procspec("map", vec![InputType::Any]),
        Process::Filter { .. } => procspec(
            "filter",
            vec![InputType::Audio, InputType::Frequency, InputType::Q],
        ),
        //        Process::Noise { .. } => procspec("noise", vec![]),
        Process::Wrap { .. } => procspec("wrap", vec![InputType::Any; 2]),
        Process::Softclip { .. } => procspec("softclip", vec![InputType::Any]),
        Process::Square { .. } => procspec("square", vec![InputType::Any]),
        Process::Sqrt { .. } => procspec("sqrt", vec![InputType::Any]),
        Process::Delay { .. } => procspec("delay", vec![InputType::Any]),
        Process::BitNeg { .. } => procspec("bitneg", vec![InputType::Any]),
        Process::BitOr { .. } => procspec("bitor", vec![InputType::Any; 2]),
        Process::BitXOr { .. } => procspec("bitxor", vec![InputType::Any; 2]),
        Process::BitAnd { .. } => procspec("bitand", vec![InputType::Any; 2]),
        Process::CurveLin { .. } => procspec("curvelin", vec![InputType::Any; 6]),
        Process::Gauss { .. } => procspec("gauss", vec![InputType::Audio]),
        Process::RMS { .. } => procspec("rms", vec![InputType::Audio]),
        Process::LPF1 { .. } => procspec("lpf1", vec![InputType::Audio, InputType::Frequency]),
    }
}

fn clear_inputs(process: &mut Process) {
    match process {
        Process::Wrap { ref mut input, .. }
        | Process::Filter { ref mut input, .. }
//        | Process::Map { ref mut input, .. }
        | Process::CurveLin { ref mut input, .. }
        | Process::Sin { ref mut input }
	| Process::Square { ref mut input }
	| Process::Sqrt { ref mut input }
        | Process::Gauss { ref mut input }
        | Process::Softclip { ref mut input }
        | Process::Mem { ref mut input, .. }
	| Process::RMS { ref mut input, .. }
        | Process::LPF1 { ref mut input, .. }
        | Process::BitNeg { ref mut input } => *input = 0.0,
        Process::SinOsc { ref mut freq, .. } => *freq = 0.0,
        Process::Mul { ref mut inputs } | Process::Add { ref mut inputs } => inputs.clear(),
        Process::Constant { .. }  => (),
	| Process::SoundIn { .. } => (),
//        Process::Noise { .. } => (),
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

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ClipType {
    None,
    SoftClip,
    Wrap,
}

// index and weight
pub type Connection = (u32, f64);

#[derive(Serialize, Deserialize)]
pub struct UGen {
    feedback_delay: bool,
    process: Process,
    value: Option<f64>,
    sum_inputs: bool,
    clip: ClipType,
}

impl UGen {
    pub fn new(process: Process) -> Self {
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
    fn process(&mut self, input: &[f64]) -> f64 {
        match self.value {
            Some(v) => v,
            None => {
                let v = process(&mut self.process, input);
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

    // fn set_input(&mut self, idx: u32, value: f64) {
    //     if DEBUG {
    //         println!("setting input of [{:?}] to {}", self, value)
    //     }
    //     set_input(&mut self.process, idx, value, self.sum_inputs)
    // }

    fn set_input_with_connection(&mut self, connection: Connection, value: f64) {
        let (idx, weight) = connection;
        if DEBUG {
            println!("setting input of [{:?}] to {}", self, value)
        }
        set_input(&mut self.process, idx, value * weight, self.sum_inputs);
        if DEBUG {
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

pub fn square() -> UGen {
    UGen::new(Process::Square { input: 0.0 })
}

pub fn sound_in(index: usize) -> UGen {
    UGen::new(Process::SoundIn { index })
}

pub fn sqrt() -> UGen {
    UGen::new(Process::Sqrt { input: 0.0 })
}

pub fn rms() -> UGen {
    UGen::new(Process::RMS {
        input: 0.0,
        chain: vec![
            Process::Square { input: 0.0 },
            Process::LPF1 {
                input: 0.0,
                p: lpf1_calc_p(10.),
                last_out: 0.0,
            },
            Process::Sqrt { input: 0.0 },
        ],
    })
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

// pub fn noise(seed: u64) -> UGen {
//     UGen::new(Process::Noise {
//         rng: SeedableRng::seed_from_u64(seed),
//     })
// }

fn sinosc(freq: f64) -> Process {
    Process::SinOsc {
        freq: freq,
        phase: 0.0,
    }
}

// fn map_process(func: fn(f64) -> f64) -> UGen {
//     UGen::new(Process::Map {
//         input: 0.0,
//         func: func,
//     })
// }

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

pub fn lpf1(freq: f64) -> UGen {
    UGen::new(Process::LPF1 {
        input: 0.0,
        p: lpf1_calc_p(freq),
        last_out: 0.0,
    })
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

pub type UGenGraph = StableGraph<UGen, Connection, Directed, DefaultIx>;

pub fn new_graph() -> UGenGraph {
    StableGraph::with_capacity(100, 100)
}

// enum UGenNode {
//     UniNode(NodeIndex),
//     NetworkNode(NodeIndex, NodeIndex),
// }

// pub fn add_process(g: &mut UGenGraph, UGen) -> UGenNode {

// }

// pub fn rms(g: &mut UGenGraph) -> (NodeIndex, NodeIndex) {
//     let square = g.add_node(map_process(|x| x * x));
//     let filter = g.add_node(lpf1(10.));
//     let sqrt = g.add_node(map_process(|x| x.sqrt()));
//     g.add_edge(square, filter, (0, 1.0));
//     g.add_edge(filter, sqrt, (0, 1.0));
//     (square, sqrt)
// }

pub fn band_pass2(g: &mut UGenGraph, f1: f64, f2: f64, q: f64) -> (NodeIndex, NodeIndex) {
    let low1 = g.add_node(lpf(f2, q));
    let low2 = g.add_node(lpf(f2, q));
    let high1 = g.add_node(hpf(f1, q));
    let high2 = g.add_node(hpf(f1, q));
    g.add_edge(low1, low2, (0, 1.0));
    g.add_edge(low2, high1, (0, 1.0));
    g.add_edge(high1, high2, (0, 1.0));
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
            g.add_edge(input_sum, *input, (0, 1.0));
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
            let w1 = rng.gen_range(0.7, 1.0);
            let w2 = rng.gen_range(0.7, 1.0);
            edges.push(g.add_edge(idx, *shuffled.choose(&mut rng).unwrap(), (0, w1)));
            edges.push(g.add_edge(*shuffled.choose(&mut rng).unwrap(), idx, (0, w2)));
        })
    }
    edges
}

fn collect_components(graph: &UGenGraph) -> Vec<Vec<NodeIndex>> {
    let mut sets = Vec::new();

    for node in graph.node_indices() {
        let mut bfs = Bfs::new(&graph, node);

        let mut all_neighbors = Vec::new();
        while let Some(nx) = bfs.next(&graph) {
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

fn nodes_with_neighbors(graph: &mut UGenGraph) -> Vec<(NodeIndex, Vec<NodeIndex>)> {
    let mut result = Vec::new();
    for node in graph.node_indices() {
        let neighbors = graph.neighbors_undirected(node).collect();
        result.push((node, neighbors));
    }
    result.sort_by(
        |(_, a): &(NodeIndex, Vec<NodeIndex>), (_, b): &(NodeIndex, Vec<NodeIndex>)| {
            a.len().partial_cmp(&b.len()).unwrap()
        },
    );
    result
}

pub fn connect_least_connected(graph: &mut UGenGraph) {
    let nodes = nodes_with_neighbors(graph);
    if let Some((first, _)) = nodes.first() {
        if let Some((future_neighbor, _)) = nodes
            .iter()
            .skip(1)
            .filter(|(_, neighbors)| !(neighbors.contains(first)))
            .next()
        {
            let w = thread_rng().gen_range(0.7, 1.0);
            graph.add_edge(*first, *future_neighbor, (0, w));
        }
    }
}

pub fn disconnect_most_connected(graph: &mut UGenGraph) {
    let nodes = nodes_with_neighbors(graph);
    if let Some((last, _)) = nodes.last() {
        if let Some((neighbor, _)) = nodes
            .iter()
            .rev()
            .skip(1)
            .filter(|(_, neighbors)| neighbors.contains(last))
            .next()
        {
            if let Some((e, _)) = graph.find_edge_undirected(*last, *neighbor) {
                graph.remove_edge(e);
            }
        }
    }
}

pub fn ensure_connectivity(graph: &mut UGenGraph) {
    let components = collect_components(graph);
    let mut rng = thread_rng();
    if let Some((first, rest)) = components.split_first() {
        for disconnected_node in rest.iter() {
            let w = rng.gen_range(0.7, 1.0);
            graph.add_edge(
                disconnected_node[0],
                *first.choose(&mut rng).unwrap(),
                (0, w),
            );
        }
    }
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

pub fn update_connections_and_flow(
    graph: &mut UGenGraph,
    flow: &mut Vec<NodeIndex>,
    output_indices: &mut [NodeIndex],
) {
    ensure_connectivity(graph);
    *flow = establish_flow(graph, output_indices);
}

/// Calls a ugen and sends result to connected ugens
pub fn call_and_output(graph: &mut UGenGraph, idx: NodeIndex, input: &[f64]) {
    match graph.node_weight_mut(idx) {
        Some(ugen) => {
            let output = ugen.process(input);
            let mut neighbors = graph.neighbors_directed(idx, Outgoing).detach();
            while let Some(neighbor_idx) = neighbors.next_node(graph) {
                let edge = graph
                    .find_edge(idx, neighbor_idx)
                    .and_then(|e| graph.edge_weight(e))
                    .map(|e| *e); // deref to stop borrowing
                match edge {
                    Some(connection) => {
                        graph[neighbor_idx].set_input_with_connection(connection, output)
                    }
                    None => (),
                }
            }
        }
        None => (),
    }
}

pub fn process_graph(
    graph: &mut UGenGraph,
    flow: &Vec<NodeIndex>,
    listening_nodes: &[NodeIndex],
    input_buffer: &[f64],
    output_buffer: &mut [f64],
) {
    flow.iter()
        .for_each(|&idx| call_and_output(graph, idx, input_buffer));
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
