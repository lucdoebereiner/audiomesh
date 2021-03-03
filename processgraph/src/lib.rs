mod filters;
pub mod lag;
//use petgraph::stable_graph::StableGraph;
//use petgraph::graph::Graph;
use petgraph::stable_graph::*;
use petgraph::visit::IntoNodeReferences;
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
const SR: f64 = 44100f64;
static FREQ_FAC: f64 = 2.0 * PI / SR;
const DEBUG: bool = false;

// TODO
// refactor to multiple modules
// move debug flag to main
// softclip on filter input optional
// variable delay times
// input indices
// kuramoto
// zip

// DONE
// lag
// not necessary
// remember node index in ugen
// outside input
// process with inner chained processes (rms, filterbank etc)

#[derive(Serialize, Deserialize, Debug)]
pub enum Process {
    Sin {
        #[serde(skip)]
        input: f64,
        mul: lag::Lag,
    },
    SinOsc {
        #[serde(skip)]
        input: f64,
        freq: lag::Lag,
        freq_mul: lag::Lag,
        #[serde(skip)]
        phase: f64,
    },
    Mul {
        #[serde(skip)]
        inputs: Vec<f64>,
    },
    Ring {
        #[serde(skip)]
        inputs: Vec<Process>,
        #[serde(skip_serializing)]
        #[serde(default)]
        input_counter: usize,
    },
    Add {
        #[serde(skip)]
        inputs: Vec<f64>,
    },
    Mem {
        #[serde(skip)]
        input: f64,
        last_value: f64,
    },
    Compressor {
        #[serde(skip)]
        input_level: Vec<Process>,
        #[serde(skip)]
        input: f64,
        threshold: lag::Lag,
        ratio: lag::Lag,
        makeup: lag::Lag,
    },
    Spike {
        #[serde(skip)]
        input: f64,
        #[serde(skip)]
        v: f64,
        #[serde(skip)]
        last_v: f64,
        threshold: lag::Lag,
        t_const: lag::Lag,
        r: lag::Lag,
        t_rest: usize,
        #[serde(skip)]
        t_this_rest: usize,
        #[serde(skip)]
        t_rest_counter: usize,
    },
    Square {
        #[serde(skip)]
        input: f64,
    },
    Sqrt {
        #[serde(skip)]
        input: f64,
    },
    SoundIn {
        #[serde(skip)]
        input: f64,
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
        #[serde(skip)]
        input: f64,
        #[serde(flatten)]
        filter: filters::Biquad,
    },
    // Noise {
    //     rng: SmallRng,
    // },
    Wrap {
        #[serde(skip)]
        input: f64,
        lo: f64,
        hi: f64,
    },
    Softclip {
        #[serde(skip)]
        input: f64,
    },
    Delay {
        #[serde(serialize_with = "vector_serialize")]
        #[serde(deserialize_with = "vector_deserialize")]
        input: Vec<f64>,
        #[serde(skip)]
        rec_idx: usize,
    },
    BitNeg {
        #[serde(skip)]
        input: f64,
    },
    BitOr {
        #[serde(skip)]
        input1: f64,
        #[serde(skip)]
        input2: f64,
    },
    BitXOr {
        #[serde(skip)]
        input1: f64,
        #[serde(skip)]
        input2: f64,
    },
    BitAnd {
        #[serde(skip)]
        input1: f64,
        #[serde(skip)]
        input2: f64,
    },
    CurveLin {
        #[serde(skip)]
        input: f64,
        in_min: f64,
        in_max: f64,
        out_min: f64,
        out_max: f64,
        curve: f64,
    },
    Gauss {
        #[serde(skip)]
        input: f64,
    },
    LPF1 {
        #[serde(skip)]
        input: f64,
        freq: lag::Lag,
        #[serde(skip)]
        p: f64,
        #[serde(skip)]
        last_out: f64,
    },
    RMS {
        #[serde(skip)]
        input: f64,
        #[serde(skip_serializing)]
        #[serde(default = "rms_chain")]
        chain: Vec<Process>,
    },
    LinCon {
        #[serde(skip)]
        input: f64,
        #[serde(skip_serializing)]
        #[serde(default = "rms_chain")]
        input_proc: Vec<Process>,
        lincon_a: lag::Lag,
        lincon_b: lag::Lag,
        #[serde(skip)]
        last_out: f64,
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

pub fn init_after_deserialize(process: &mut Process) {
    match process {
        Process::Filter { ref mut filter, .. } => filter.init(),
        _ => (),
    }
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

fn process(proc: &mut Process, external_input: &[f64]) -> f64 {
    match proc {
        Process::SoundIn { input, index } => {
            (*input * 0.5) + *external_input.get(*index).unwrap_or(&0.0)
        }
        Process::Sin { input, mul } => (*input * mul.tick()).sin(),
        Process::SinOsc {
            input,
            freq,
            ref mut phase,
            freq_mul,
        } => {
            //            let freqIn = numerical::curvelin(*input, -1.0, 1.0, 1.0, 10000.0, 2.0);
            let output = phase.sin();
            *phase += (freq.tick() + (*input * freq_mul.tick())) * FREQ_FAC;
            output
        }
        Process::Mul { inputs, .. } => inputs.iter().product(),
        Process::Add { inputs } => inputs.iter().sum(),
        Process::Square { input } => input.powf(2.0),
        Process::Sqrt { input } => input.sqrt(),
        Process::Mem { input, .. } => *input,
        //        Process::Constant { ref mut value } => value.tick(),
        Process::Constant { value } => *value,
        //        Process::Map { input, func } => (func)(*input),
        Process::Filter {
            input,
            ref mut filter,
        } => filter.process((*input).tanh()),
        // TODO debug the 0.0s in here
        Process::Ring { ref mut inputs, .. } => inputs
            .iter_mut()
            .map(|p| process(p, external_input))
            .product(),
        Process::Compressor {
            ref mut input_level,
            input,
            threshold,
            ratio,
            makeup,
        } => {
            let level: f64 = input_level
                .iter_mut()
                .map(|p| process(p, external_input))
                .sum();
            let current_threshold = threshold.tick();
            if level > current_threshold {
                let current_ratio = ratio.tick();
                let fac = (current_threshold.powf(1.0 - 1.0 / current_ratio)
                    * level.powf(1.0 / current_ratio))
                    / level;
                let output = *input * fac * makeup.tick();
                // println!(
                //     "level {}, ratio {}, fac {}, output {}",
                //     level, current_ratio, fac, output
                // );
                output
            } else {
                let output = *input * makeup.tick();
                //                println!("level {}, output {}", level, output);
                output
            }
        }
        // leaky integrate and fire
        Process::Spike {
            input,
            ref mut v,
            ref mut last_v,
            ref mut threshold,
            ref mut t_const,
            ref mut r,
            t_rest,
            ref mut t_this_rest,
            ref mut t_rest_counter,
        } => {
            let mut output = 0.0;
            if *t_rest_counter > 0 {
                let pulse_width = *t_this_rest as f64 / 8.0;
                let offset = (*t_this_rest - 1) as f64 / 2.0;
                output = (((*t_rest_counter as f64 - offset) / pulse_width).powf(2.0) * -1.0).exp()
                    * 0.7; // hardcoded amplitude
                *t_rest_counter = *t_rest_counter - 1;
                *last_v = *v;
                *v = 0.0;
                threshold.tick();
                t_const.tick();
            //                println!("output {}", output);
            } else {
                let this_const = t_const.tick();
                *v = *last_v + (((*last_v * -1.0) + (input.abs() * r.tick())) * this_const);
                *last_v = *v;
                // println!(
                //     "after v {}, input {} , const {}, last {}",
                //     v, *input, this_const, last_v
                // );
                if *v > threshold.tick() {
                    output = 1.0;
                    *t_this_rest = *t_rest;
                    *t_rest_counter = *t_rest;
                }
            }
            output
        }
        //        Process::Noise { ref mut rng } => rng.gen_range(-1.0, 1.0),
        Process::Wrap { input, lo, hi } => numerical::wrap(*input, *lo, *hi),
        Process::Softclip { input } => input.tanh(),
        Process::Delay {
            input,
            ref mut rec_idx,
        } => {
            let prev_idx = ((*rec_idx + 1) + input.len()) % input.len();
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
            freq,
            p,
            ref mut last_out,
        } => {
            if !freq.is_done() {
                *p = lpf1_calc_p(freq.current)
            }
            freq.tick();
            let output = ((1. - *p) * *input) + (*p * *last_out);
            *last_out = output;
            output
        }
        Process::RMS {
            input,
            ref mut chain,
        } => process_chain(chain, *input, external_input),
        Process::LinCon {
            input,
            input_proc,
            lincon_a,
            lincon_b,
            last_out,
        } => {
            let rms_input = process_chain(input_proc, *input, external_input);
            let output = (*last_out * (rms_input * lincon_a.tick()) + lincon_b.tick()) % 2.0 - 1.0;
            *last_out = output;
            output
        }
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
    //    println!("setting input {:?} to {:?}", idx, input_value);
    if let Some(node) = graph.graph.node_weight_mut(node_idx) {
        //  println!("found node");
        set_input(&mut node.process, idx as u32, input_value, false)
    }
}

fn set_input(proc: &mut Process, idx: u32, input_value: f64, add: bool) {
    //println!("setting input {:?} to {:?}", idx, input_value);
    match proc {
        Process::Gauss { ref mut input }
        | Process::Softclip { ref mut input }
        | Process::Square { ref mut input }
        | Process::Sqrt { ref mut input }
        | Process::Mem { ref mut input, .. }
        | Process::RMS { ref mut input, .. }
        | Process::BitNeg { ref mut input } => set_or_add(input, input_value, add),
        Process::SinOsc {
            ref mut input,
            ref mut freq,
            ref mut freq_mul,
            ..
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => freq.set_target(input_value),
            2 => freq_mul.set_target(input_value),
            _ => panic!("wrong index into {}: {}", name(proc), idx),
        },
        Process::Add { ref mut inputs } | Process::Mul { ref mut inputs } => {
            inputs.push(input_value)
        }
        Process::Ring {
            ref mut inputs,
            ref mut input_counter,
        } => {
            *input_counter = *input_counter + 1;
            if inputs.len() < *input_counter {
                let mut fil = lpf2000();
                set_input(&mut fil, 0, input_value, false);
                inputs.push(fil);
            //                println!("c {:?}, ring inputs: {:?}", input_counter, inputs)
            } else {
                set_input(&mut inputs[*input_counter - 1], 0, input_value, false);
                //  println!("inputs {:?}, ring inputs: {:?}", input_value, inputs)
            }
        }
        Process::Spike {
            ref mut input,
            ref mut threshold,
            ref mut t_const,
            ref mut r,
            ref mut t_rest,
            ..
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => threshold.set_target(input_value),
            2 => t_const.set_target(input_value),
            3 => r.set_target(input_value),
            4 => *t_rest = input_value.round() as usize,
            _ => panic!("wrong index into {}: {}", name(proc), idx),
        },
        Process::Compressor {
            ref mut input,
            ref mut input_level,
            ref mut threshold,
            ref mut ratio,
            ref mut makeup,
        } => match idx {
            0 => {
                if input_level.len() < 1 {
                    input_level.push(rms_proc());
                }
                set_or_add(input, input_value, add);
                set_input(&mut input_level[0], 0, input_value, false);
            }
            1 => threshold.set_target(input_value),
            2 => ratio.set_target(input_value),
            3 => makeup.set_target(input_value),
            _ => panic!("wrong index into {}: {}", name(proc), idx),
        },
        Process::Constant { .. } => (),
        //        Process::Map { ref mut input, .. } => set_or_add(input, input_value, add),
        Process::SoundIn { .. } => (), // match idx {
        //     0 => set_or_add(input, input_value, add),
        //     //	1 => *index = input_value as usize,
        //     _ => panic!("wrong index into {}: {}", name(proc), idx),
        // },
        Process::Filter {
            ref mut input,
            ref mut filter,
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => filter.set_parameters(input_value, filter.q.current),
            2 => filter.set_parameters(filter.freq.current, input_value),
            _ => panic!("wrong index into {}: {}", name(proc), idx),
        },
        Process::Sin {
            ref mut input,
            ref mut mul,
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => mul.set_target(input_value),
            _ => panic!("wrong index into {}: {}", name(proc), idx),
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
            _ => panic!("wrong index into {}: {}", name(proc), idx),
        },

        Process::LinCon {
            ref mut input,
            ref mut lincon_a,
            ref mut lincon_b,
            ..
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => lincon_a.set_target(input_value),
            2 => lincon_b.set_target(input_value),
            _ => panic!("wrong index into {}: {}", name(proc), idx),
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
            _ => panic!("wrong index into {}: {}", name(proc), idx),
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
            _ => panic!("wrong index into {}: {}", name(proc), idx),
        },
        Process::LPF1 {
            ref mut input,
            ref mut freq,
            ..
        } => match idx {
            0 => set_or_add(input, input_value, add),
            1 => freq.set_target(input_value),
            //            1 => *p = lpf1_calc_p(input_value), // freq input
            _ => panic!("wrong index into {}: {}", name(proc), idx),
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
    Factor,
    Threshold,
    Parameter,
    Amplitude,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum ProcessType {
    NoInputGenerator,
    Processor,
    NeedsMultipleInputs,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProcessSpec {
    name: String,
    process_type: ProcessType,
    inputs: Vec<InputType>,
}

fn procspec(name: &'static str, process_type: ProcessType, inputs: Vec<InputType>) -> ProcessSpec {
    ProcessSpec {
        name: name.to_string(),
        process_type,
        inputs,
    }
}

fn name(process: &Process) -> String {
    spec(process).name
}

fn spec(process: &Process) -> ProcessSpec {
    match process {
        Process::Spike { .. } => procspec(
            "spike",
            ProcessType::Processor,
            vec![
                InputType::Any,
                InputType::Threshold,
                InputType::Parameter,
                InputType::Parameter,
            ],
        ),
        Process::SoundIn { .. } => procspec(
            "soundin",
            ProcessType::NoInputGenerator,
            vec![InputType::Any],
        ),
        Process::Sin { .. } => procspec("sin", ProcessType::Processor, vec![InputType::Phase]),
        Process::SinOsc { .. } => procspec(
            "sinosc",
            ProcessType::Processor,
            vec![InputType::Frequency, InputType::Factor],
        ),
        Process::Mul { .. } => procspec(
            "mul",
            ProcessType::NeedsMultipleInputs,
            vec![InputType::Any],
        ),
        Process::Ring { .. } => procspec(
            "ring",
            ProcessType::NeedsMultipleInputs,
            vec![InputType::Audio],
        ),
        Process::Add { .. } => procspec("add", ProcessType::Processor, vec![InputType::Any]),
        Process::Mem { .. } => procspec("mem", ProcessType::Processor, vec![InputType::Any]),
        Process::Constant { .. } => procspec("constant", ProcessType::NoInputGenerator, vec![]),
        //        Process::Map { .. } => procspec("map", vec![InputType::Any]),
        Process::Filter { .. } => procspec(
            "filter",
            ProcessType::Processor,
            vec![InputType::Audio, InputType::Frequency, InputType::Q],
        ),

        Process::Compressor { .. } => procspec(
            "compressor",
            ProcessType::Processor,
            vec![
                InputType::Audio,
                InputType::Threshold,
                InputType::Parameter,
                InputType::Amplitude,
            ],
        ),

        //        Process::Noise { .. } => procspec("noise", vec![]),
        Process::Wrap { .. } => procspec("wrap", ProcessType::Processor, vec![InputType::Any; 2]),
        Process::Softclip { .. } => {
            procspec("softclip", ProcessType::Processor, vec![InputType::Any])
        }
        Process::Square { .. } => procspec("square", ProcessType::Processor, vec![InputType::Any]),
        Process::Sqrt { .. } => procspec("sqrt", ProcessType::Processor, vec![InputType::Any]),
        Process::Delay { .. } => procspec("delay", ProcessType::Processor, vec![InputType::Any]),
        Process::BitNeg { .. } => procspec("bitneg", ProcessType::Processor, vec![InputType::Any]),
        Process::BitOr { .. } => procspec(
            "bitor",
            ProcessType::NeedsMultipleInputs,
            vec![InputType::Any; 2],
        ),
        Process::BitXOr { .. } => procspec(
            "bitxor",
            ProcessType::NeedsMultipleInputs,
            vec![InputType::Any; 2],
        ),
        Process::BitAnd { .. } => procspec(
            "bitand",
            ProcessType::NeedsMultipleInputs,
            vec![InputType::Any; 2],
        ),
        Process::CurveLin { .. } => {
            procspec("curvelin", ProcessType::Processor, vec![InputType::Any; 6])
        }
        Process::Gauss { .. } => procspec("gauss", ProcessType::Processor, vec![InputType::Audio]),
        Process::RMS { .. } => procspec("rms", ProcessType::Processor, vec![InputType::Audio]),
        Process::LPF1 { .. } => procspec(
            "lpf1",
            ProcessType::Processor,
            vec![InputType::Audio, InputType::Frequency],
        ),
        Process::LinCon { .. } => procspec(
            "lincon",
            ProcessType::Processor,
            vec![InputType::Any, InputType::Factor, InputType::Factor],
        ),
    }
}

fn clear_inputs(process: &mut Process) {
    match process {
        Process::Wrap { ref mut input, .. }
        | Process::Filter { ref mut input, .. }
//        | Process::Map { ref mut input, .. }
        | Process::CurveLin { ref mut input, .. }
	| Process::Square { ref mut input }
	| Process::Sqrt { ref mut input }
        | Process::Gauss { ref mut input }
        | Process::Softclip { ref mut input }
        | Process::Mem { ref mut input, .. }
	| Process::RMS { ref mut input, .. }
        | Process::LPF1 { ref mut input, .. }
	| Process::LinCon { ref mut input, .. }
	| Process::Compressor { ref mut input, .. }
	| Process::Spike { ref mut input, .. }
        | Process::BitNeg { ref mut input } => *input = 0.0,
        Process::SinOsc { ref mut input, .. } => *input = 0.0,
        Process::Mul { ref mut inputs } | Process::Add { ref mut inputs } => inputs.clear(),
	| Process::Ring { ref mut inputs, ref mut input_counter } => {
	    *input_counter = 0;
	    for i in inputs.iter_mut() {
		clear_inputs(i);
	    }
	}
	Process::Sin { ref mut input, .. } => *input = 0.0,
        Process::Constant { .. }  => (),
	| Process::SoundIn { ref mut input, .. } => *input = 0.0,
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
//pub type Connection = (u32, f64);
pub type Connection = (u32, lag::Lag);

#[derive(Serialize, Deserialize)]
pub struct UGen {
    feedback_delay: bool,
    process: Process,
    value: Option<f64>,
    #[serde(skip)]
    pub last_value: f64,
    sum_inputs: bool,
    clip: ClipType,
    output_sends: Vec<(usize, lag::Lag)>,
}

impl UGen {
    pub fn new(process: Process) -> Self {
        UGen {
            feedback_delay: false,
            process,
            value: None,
            sum_inputs: true,
            last_value: 0.0,
            clip: ClipType::None,
            output_sends: vec![],
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
                let v = process(&mut self.process, input);
                let output = match self.clip {
                    ClipType::None => v,
                    ClipType::SoftClip => v.tanh(),
                    ClipType::Wrap => numerical::wrap(v, -1., 1.),
                };
                if DEBUG {
                    println!("processing: [{:?}] with result {}", self, output)
                }
                self.last_value = output;
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

    fn set_input_with_connection(&mut self, connection: (u32, f64), value: f64) {
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
    UGen::new(Process::SoundIn { input: 0.0, index })
}

pub fn sqrt() -> UGen {
    UGen::new(Process::Sqrt { input: 0.0 })
}

fn lpf2000() -> Process {
    filter(filters::FilterType::BLPF, 2000.0, 6.0)
}

fn lp2000() -> Process {
    Process::LPF1 {
        input: 0.0,
        freq: lag::lag(2000.0),
        p: lpf1_calc_p(2000.),
        last_out: 0.0,
    }
}

fn rms_chain() -> Vec<Process> {
    vec![
        Process::Square { input: 0.0 },
        Process::LPF1 {
            input: 0.0,
            freq: lag::lag(10.0),
            p: lpf1_calc_p(10.),
            last_out: 0.0,
        },
        Process::Sqrt { input: 0.0 },
    ]
}

fn rms_proc() -> Process {
    Process::RMS {
        input: 0.0,
        chain: rms_chain(),
    }
}

pub fn rms() -> UGen {
    UGen::new(rms_proc())
}

pub fn spike(threshold: f64, t_const: f64, r: f64, t_rest: usize) -> UGen {
    UGen::new(Process::Spike {
        input: 0.0,
        v: 0.0,
        last_v: 0.0,
        threshold: lag::lag(threshold),
        t_const: lag::lag(t_const),
        r: lag::lag(r),
        t_rest,
        t_this_rest: t_rest,
        t_rest_counter: 0,
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
    UGen::new(Process::Sin {
        input: 0.0,
        mul: lag::lag(1.0),
    })
}

pub fn sin_osc(freq: f64, freq_mul: f64) -> UGen {
    UGen::new(Process::SinOsc {
        input: 0.0,
        freq: lag::lag(freq),
        freq_mul: lag::lag(freq_mul),
        phase: 0.0,
    })
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

pub fn ring() -> UGen {
    UGen::new(Process::Ring {
        inputs: Vec::new(),
        input_counter: 0,
    })
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

pub fn wrap(lo: f64, hi: f64) -> UGen {
    UGen::new(Process::Wrap { input: 0.0, lo, hi })
}

pub fn softclip() -> UGen {
    UGen::new(Process::Softclip { input: 0.0 })
}

pub fn compressor(threshold: f64, ratio: f64, makeup: f64) -> UGen {
    UGen::new(Process::Compressor {
        input: 0.0,
        input_level: vec![],
        threshold: lag::lag(threshold),
        ratio: lag::lag(ratio),
        makeup: lag::lag(makeup),
    })
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
        freq: lag::lag(freq),
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
        last_value: 0.0,
        clip: ClipType::None,
        sum_inputs: true,
        output_sends: vec![(0, lag::lag(1.0))],
    }
}

pub type UGenGraphStructure = StableGraph<UGen, Connection, Directed, DefaultIx>;

pub struct UGenGraph {
    pub graph: UGenGraphStructure,
    current_listening_nodes: Vec<NodeIndex>,
}

pub fn new_graph() -> UGenGraph {
    UGenGraph {
        graph: StableGraph::with_capacity(100, 100),
        current_listening_nodes: vec![],
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct OutputSpec {
    node: usize,
    output: usize,
    amp: f64,
}

impl UGenGraph {
    pub fn set_output(&mut self, spec: OutputSpec) {
        self.graph[NodeIndex::new(spec.node)].set_output(spec.output, spec.amp)
    }
}

// returns true if it had to update
fn update_listening_nodes(g: &mut UGenGraph) -> bool {
    let mut nodes_to_listen_to = vec![];
    for (idx, ugen) in g.graph.node_references() {
        if ugen.output_sends.iter().any(|(_, w)| w.target > 0.0) {
            nodes_to_listen_to.push(idx);
        }
    }
    if (g.current_listening_nodes.len() == nodes_to_listen_to.len())
        && nodes_to_listen_to
            .iter()
            .all(|n| g.current_listening_nodes.contains(n))
    {
        false
    } else {
        g.current_listening_nodes = nodes_to_listen_to;
        true
    }
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
    let low1 = g.graph.add_node(lpf(f2, q));
    let low2 = g.graph.add_node(lpf(f2, q));
    let high1 = g.graph.add_node(hpf(f1, q));
    let high2 = g.graph.add_node(hpf(f1, q));
    g.graph.add_edge(low1, low2, (0, lag::lag(1.0)));
    g.graph.add_edge(low2, high1, (0, lag::lag(1.0)));
    g.graph.add_edge(high1, high2, (0, lag::lag(1.0)));
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
    let input_sum = g.graph.add_node(add());
    let outputs = filters
        .iter()
        .map(|(input, output)| {
            g.graph.add_edge(input_sum, *input, (0, lag::lag(1.0)));
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
            if let Some(to_node) = choose_with_input(g) {
                edges.push(g.graph.add_edge(idx, to_node, (0, lag::lag(w1))));
            }
            if does_idx_have_input(g, idx) {
                edges.push(g.graph.add_edge(
                    *shuffled.choose(&mut rng).unwrap(),
                    idx,
                    (0, lag::lag(w2)),
                ));
            }
        })
    }
    edges
}

pub fn rnd_circle(g: &mut UGenGraph, nodes: &[NodeIndex], n_connections: u32) -> Vec<EdgeIndex> {
    let mut rng = thread_rng();
    let mut shuffled = nodes.to_vec();
    shuffled.shuffle(&mut rng);
    let mut edges = Vec::new();
    for _i in 0..n_connections {
        let filtered_shuffled: Vec<&NodeIndex> = shuffled
            .iter()
            .filter(|&idx| does_idx_have_input(g, *idx))
            .collect();
        let length = filtered_shuffled.len();
        for (i, &idx) in filtered_shuffled.iter().enumerate() {
            let w1 = rng.gen_range(0.7, 1.0);
            edges.push(g.graph.add_edge(
                *idx,
                *filtered_shuffled[(i + 1) % length],
                (0, lag::lag(w1)),
            ));
        }

        let w2 = rng.gen_range(0.7, 1.0);
        let generators: Vec<&NodeIndex> = shuffled
            .iter()
            .filter(|&idx| !does_idx_have_input(g, *idx))
            .collect();
        for &idx in generators.iter() {
            edges.push(g.graph.add_edge(
                *idx,
                **filtered_shuffled.choose(&mut rng).unwrap(),
                (0, lag::lag(w2)),
            ))
        }
    }
    edges
}

fn does_idx_have_input(g: &UGenGraph, node: NodeIndex) -> bool {
    if let Some(u) = g.graph.node_weight(node) {
        !(spec(&u.process).process_type == ProcessType::NoInputGenerator)
    } else {
        false
    }
}

// guarantees that choosen idx is a process with input
fn choose_with_input(g: &UGenGraph) -> Option<NodeIndex> {
    let mut rng = thread_rng();
    let with_input: Vec<NodeIndex> = g
        .graph
        .node_references()
        .filter_map(|(idx, u)| {
            if !(spec(&u.process).process_type == ProcessType::NoInputGenerator) {
                Some(idx)
            } else {
                None
            }
        })
        .collect();
    with_input.choose(&mut rng).map(|r| *r)
}

fn collect_components(graph: &UGenGraph) -> Vec<Vec<NodeIndex>> {
    let mut sets = Vec::new();

    for node in graph.graph.node_indices() {
        let mut bfs = Bfs::new(&graph.graph, node);

        let mut all_neighbors = Vec::new();
        while let Some(nx) = bfs.next(&graph.graph) {
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
    for node in graph.graph.node_indices() {
        let neighbors = graph.graph.neighbors_undirected(node).collect();
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
            graph
                .graph
                .add_edge(*first, *future_neighbor, (0, lag::lag(w)));
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
            if let Some((e, _)) = graph.graph.find_edge_undirected(*last, *neighbor) {
                graph.graph.remove_edge(e);
            }
        }
    }
}

pub fn ensure_connectivity(graph: &mut UGenGraph) {
    let components = collect_components(graph);
    let mut rng = thread_rng();
    if let Some((first, rest)) = components.split_first() {
        for disconnected_node in rest.iter() {
            let w1 = rng.gen_range(0.7, 1.0);
            let w2 = rng.gen_range(0.7, 1.0);
            graph.graph.add_edge(
                disconnected_node[0],
                *first.choose(&mut rng).unwrap(),
                (0, lag::lag(w1)),
            );
            graph.graph.add_edge(
                *first.choose(&mut rng).unwrap(),
                disconnected_node[0],
                (0, lag::lag(w2)),
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

pub fn establish_flow(graph: &mut UGenGraph) -> Vec<NodeIndex> {
    let _ = update_listening_nodes(graph);
    let mut to_visit = graph.current_listening_nodes.to_owned();
    let mut visited: Vec<NodeIndex> = to_visit.to_owned();

    while !to_visit.is_empty() {
        match to_visit.pop() {
            Some(current_node) => graph
                .graph
                .neighbors_directed(current_node, Incoming)
                .for_each(|neighbor| {
                    if !visited.contains(&neighbor) {
                        visited.push(neighbor);
                        match graph.graph.node_weight(neighbor) {
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

pub fn update_connections_and_flow(graph: &mut UGenGraph, flow: &mut Vec<NodeIndex>) {
    ensure_connectivity(graph);
    *flow = establish_flow(graph);
}

/// Calls a ugen and sends result to connected ugens
pub fn call_and_output(graph: &mut UGenGraph, idx: NodeIndex, input: &[f64]) {
    match graph.graph.node_weight_mut(idx) {
        Some(ugen) => {
            let output = ugen.process(input);
            let mut neighbors = graph.graph.neighbors_directed(idx, Outgoing).detach();
            while let Some(neighbor_idx) = neighbors.next_node(&graph.graph) {
                let edge = graph
                    .graph
                    .find_edge(idx, neighbor_idx)
                    .and_then(|e| graph.graph.edge_weight(e))
                    .map(|e| {
                        let (idx, w) = &*e;
                        (*idx, w.current)
                    }); // deref to stop borrowing
                match edge {
                    Some(connection) => {
                        graph.graph[neighbor_idx].set_input_with_connection(connection, output)
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
    input_buffer: &[f64],
    output_buffer: &mut [f64],
) {
    flow.iter()
        .for_each(|&idx| call_and_output(graph, idx, input_buffer));

    output_buffer.iter_mut().for_each(|x| *x = 0.0);

    for ugen in graph.graph.node_weights_mut() {
        let current_value = ugen.value.unwrap_or(0.0);
        for (out_i, amp_out) in ugen.output_sends.iter_mut() {
            output_buffer[*out_i % output_buffer.len()] += current_value * amp_out.tick();
        }
        // let output = match graph.node_weight(listening_node) {
        //     Some(node) =>
        //     None => 0.0,
        // };
        // output_buffer[i] = output;
    }
    for &i in flow {
        match graph.graph.node_weight_mut(i) {
            Some(n) => {
                n.reset();
            }
            None => (),
        }
    }
    for e in graph.graph.edge_weights_mut() {
        let (_, w) = e;
        w.tick();
        ()
    }
}
