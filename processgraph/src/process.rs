use crate::compenv::*;
use crate::filters;
use crate::lag;
use crate::numerical;
use crate::tapdelay;
use serde::{Deserialize, Serialize};
use serde::{Deserializer, Serializer};
use std::f64;
const PI: f64 = f64::consts::PI;

use crate::numerical::{zapgremlins, TWOPI};

static mut SR: f64 = 44100f64;
static mut FREQ_FAC: f64 = unsafe { 2.0 * PI / SR };

pub fn set_sr(sr: f64) {
    unsafe {
        SR = sr;
        FREQ_FAC = 2.0 * PI / SR;
    }
}

pub fn get_sr() -> f64 {
    unsafe { SR }
}

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
    Env {
        #[serde(skip)]
        input: f64,
        #[serde(skip)]
        input_level: Vec<Process>,
        #[serde(flatten)]
        //        #[serde(default = "comp_env_default")]
        comp_env: CompEnv,
    },
    VanDerPol {
        #[serde(skip)]
        input: f64,
        #[serde(skip)]
        x: f64,
        #[serde(skip)]
        y: f64,
        frac: lag::Lag,
        e: lag::Lag,
        a: lag::Lag,
        #[serde(skip_serializing)]
        #[serde(default = "dc_remove_filter")]
        output: filters::OnePoleHP,
    },
    Ducking {
        #[serde(skip)]
        input_level: Vec<Process>,
        #[serde(skip)]
        input: f64,
        #[serde(skip_serializing)]
        #[serde(default = "ducking_lag")]
        output_factor: lag::Lag,
    },
    EnvFollow {
        #[serde(skip)]
        input_level: Vec<Process>,
        #[serde(skip)]
        input: f64,
    },
    PLL {
        #[serde(skip_serializing)]
        #[serde(default = "dc_remove_filter")]
        input: filters::OnePoleHP,
        factor: lag::Lag,
        #[serde(skip)]
        phase: f64,
        #[serde(skip_serializing)]
        #[serde(default = "pll_error_lag")]
        error: lag::Lag,
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
        factor: lag::Lag,
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
    Resonator {
        #[serde(skip)]
        input: f64,
        #[serde(skip)]
        freq_mod: f64,
        freq_factor: lag::Lag,
        freq_center: lag::Lag,
        #[serde(flatten)]
        resonator: filters::ComplexRes,
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
    VarDelay {
        #[serde(skip)]
        input: f64,
        #[serde(flatten)]
        delay: tapdelay::TapDelay,
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
        Process::Filter { ref mut filter, .. } => filter.init(get_sr()),
        Process::Resonator {
            ref mut resonator, ..
        } => resonator.init(get_sr()),

        _ => (),
    }
}

fn set_or_add(field: &mut f64, value: f64, add: bool) {
    if add {
        *field += value
    } else {
        *field = value
    }
}

fn lpf1_calc_p(freq: f64) -> f64 {
    1. - (2. * (freq / unsafe { SR }).tan())
}

fn rms_proc() -> Process {
    Process::RMS {
        input: 0.0,
        chain: rms_chain(),
    }
}

// fn comp_env_default() -> CompEnv {
//     CompEnv::new(0.6, 0.001, get_sr() as usize * 4)
// }

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
    Seconds,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum ProcessType {
    NoInputGenerator,
    TransparentProcessor,
    OpaqueProcessor,
    SidechainEnv,
    TwoInputs,
    MultipleInputs,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProcessSpec {
    pub name: String,
    pub process_type: ProcessType,
    pub inputs: Vec<InputType>,
}

fn procspec(name: &'static str, process_type: ProcessType, inputs: Vec<InputType>) -> ProcessSpec {
    ProcessSpec {
        name: name.to_string(),
        process_type,
        inputs,
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

fn ducking_lag() -> lag::Lag {
    let mut dlag = lag::lag(0.0);
    dlag.set_duration_ud(1.0, 0.15, get_sr());
    dlag
}

fn pll_error_lag() -> lag::Lag {
    let mut dlag = lag::lag(0.0);
    dlag.set_factor(0.4);
    dlag
}

fn dc_remove_filter() -> filters::OnePoleHP {
    filters::OnePoleHP::new(0.94)
}

fn filter(filter_type: filters::FilterType, freq: f64, q: f64) -> Process {
    unsafe {
        Process::Filter {
            input: 0.0,
            filter: filters::Biquad::new(filter_type, freq, q, SR),
        }
    }
}

fn lpf2000() -> Process {
    filter(filters::FilterType::BLPF, 2000.0, 6.0)
}

// index always 0, weight always 1
fn process_chain(chain: &mut [Process], input_value: f64, input: &[f64]) -> f64 {
    let mut prev_output = input_value;
    for p in chain {
        p.set_input(0, prev_output, false);
        prev_output = p.process(input);
    }
    prev_output
}

fn clear_chain(chain: &mut [Process]) {
    for p in chain {
        p.clear_inputs();
    }
}

impl Process {
    pub fn process(&mut self, external_input: &[f64]) -> f64 {
        match self {
            Process::SoundIn {
                input,
                index,
                ref mut factor,
            } => (*input + (*external_input.get(*index).unwrap_or(&0.0) * factor.tick())),
            Process::Sin { input, mul } => (*input * mul.tick()).sin(),
            Process::SinOsc {
                input,
                freq,
                ref mut phase,
                freq_mul,
            } => {
                //            let freq_in = numerical::curvelin(input.abs(), 0.0, 1.0, 1.0, freq_mul.tick(), 2.0);
                let output = phase.sin();
                *phase += (freq.tick() + (*input * freq_mul.tick())) * unsafe { FREQ_FAC };
                output
            }
            Process::VanDerPol {
                input,
                ref mut x,
                ref mut y,
                ref mut frac,
                ref mut e,
                ref mut a,
                ref mut output,
            } => {
                let this_e = e.tick();
                let f = frac.tick();
                let this_a = a.tick();

                let mut d_x_n = *y;
                let mut d_y_n = (this_e * *y * (1.0 - (*x).powi(2))) - *x + (*input * this_a);

                d_x_n = zapgremlins(d_x_n);
                d_y_n = zapgremlins(d_y_n);

                let x_1 = *x + (d_x_n * f);
                let y_1 = *y + (d_y_n * f);

                let mut d_x_1 = y_1;
                let mut d_y_1 = (this_e * y_1 * (1.0 - (x_1).powi(2))) - x_1 + (*input * this_a);

                d_x_1 = zapgremlins(d_x_1);
                d_y_1 = zapgremlins(d_y_1);

                let mut d_x = ((d_x_n + d_x_1) / 2.0) * f;
                let mut d_y = ((d_y_n + d_y_1) / 2.0) * f;

                //hard reset
                if d_x.abs() > 300.0 || d_y.abs() > 300.0 {
                    //                    println!("reset vdp x{} y{}", d_x, d_y);
                    d_x = 0.0;
                    d_y = 0.0;
                    *x = 0.0;
                    *y = 0.0;
                }

                *x = *x + d_x;
                *y = *y + d_y;
                output.input = *x * 0.5;
                output.process()
            }
            Process::PLL {
                input,
                ref mut factor,
                ref mut phase,
                ref mut error,
            } => {
                let output = phase.sin();
                *phase = numerical::fmod(*phase + (error.tick() * factor.tick()), TWOPI);
                let signums = output.signum() * input.process().signum();
                if signums > 0.0 {
                    error.set_target(0.0);
                } else {
                    error.set_target(1.0);
                }
                output * 0.7
            }
            Process::Env {
                input,
                ref mut input_level,
                ref mut comp_env,
            } => {
                let level: f64 = input_level
                    .iter_mut()
                    .map(|p| p.process(external_input))
                    .sum();
                comp_env.process(level / 0.707);
                *input * comp_env.fac
            }
            Process::Mul { inputs, .. } => inputs.iter().product(),
            Process::Add { inputs } => inputs.iter().sum(),
            Process::Square { input } => input.powf(2.0),
            Process::Sqrt { input } => input.sqrt(),
            Process::Mem { input, .. } => *input,
            //        Process::Constant { ref mut value } => value.tick(),
            Process::Constant { value } => *value,
            //        Process::Map { input, func } => (func)(*input),
            Process::Resonator {
                input,
                freq_mod,
                ref mut resonator,
                ref mut freq_factor,
                ref mut freq_center,
            } => {
                resonator.freq = freq_center.tick() + (freq_factor.tick() * *freq_mod);
                resonator.process(*input)
            }
            Process::Filter {
                input,
                ref mut filter,
            } => filter.process(*input),
            // TODO debug the 0.0s in here
            Process::Ring { ref mut inputs, .. } => inputs
                .iter_mut()
                .map(|p| p.process(external_input))
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
                    .map(|p| p.process(external_input))
                    .sum();
                let current_threshold = threshold.tick();
                if level > current_threshold {
                    let current_ratio = ratio.tick();
                    let fac = (current_threshold.powf(1.0 - 1.0 / current_ratio)
                        * level.powf(1.0 / current_ratio))
                        / level;
                    let output = *input * fac * makeup.tick();
                    output
                } else {
                    let output = *input * makeup.tick();
                    output
                }
            }
            Process::Ducking {
                ref mut input_level,
                input,
                output_factor,
            } => {
                let level: f64 = input_level
                    .iter_mut()
                    .map(|p| p.process(external_input))
                    .sum();
                let mut out_target = (1.0 - (level * 12.0).powi(2)).max(0.0);
                out_target = out_target.min(1.0).max(0.0);
                output_factor.set_target(out_target);
                *input * output_factor.tick()
            }
            Process::EnvFollow {
                ref mut input_level,
                input,
            } => {
                let level: f64 = input_level
                    .iter_mut()
                    .map(|p| p.process(external_input))
                    .sum();
                *input * (level / 0.707)
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
                    output = (((*t_rest_counter as f64 - offset) / pulse_width).powf(2.0) * -1.0)
                        .exp()
                        * 0.7; // hardcoded amplitude
                    *t_rest_counter = *t_rest_counter - 1;
                    *last_v = *v;
                    *v = 0.0;
                    threshold.tick();
                    t_const.tick();
                } else {
                    let this_const = t_const.tick();
                    *v = *last_v + (((*last_v * -1.0) + (input.abs() * r.tick())) * this_const);
                    *last_v = *v;
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
            Process::VarDelay {
                input,
                ref mut delay,
            } => delay.process(*input),
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
                let output =
                    (*last_out * (rms_input * lincon_a.tick()) + lincon_b.tick()) % 2.0 - 1.0;
                *last_out = output;
                output
            }
        }
    }

    pub fn set_input(&mut self, idx: u32, input_value: f64, add: bool) {
        //println!("setting input {:?} to {:?}", idx, input_value);
        match self {
            Process::Gauss { ref mut input }
            | Process::Softclip { ref mut input }
            | Process::Square { ref mut input }
            | Process::Sqrt { ref mut input }
            | Process::Mem { ref mut input, .. }
            | Process::RMS { ref mut input, .. }
            | Process::BitNeg { ref mut input } => set_or_add(input, input_value, add),
            Process::PLL {
                ref mut input,
                ref mut factor,
                ..
            } => match idx {
                0 => set_or_add(&mut input.input, input_value, add),
                1 => factor.set_target(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
            Process::VarDelay {
                ref mut input,
                ref mut delay,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => delay.set_delay(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
            Process::VanDerPol {
                ref mut input,
                ref mut e,
                ref mut frac,
                ref mut a,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => e.set_target(input_value),
                2 => frac.set_target(input_value),
                3 => a.set_target(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
            Process::SinOsc {
                ref mut input,
                ref mut freq,
                ref mut freq_mul,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => freq.set_target(input_value),
                2 => freq_mul.set_target(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
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
                    fil.set_input(0, input_value, false);
                    inputs.push(fil);
                //                println!("c {:?}, ring inputs: {:?}", input_counter, inputs)
                } else {
                    inputs[*input_counter - 1].set_input(0, input_value, false);
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
                _ => panic!("wrong index into {}: {}", self.name(), idx),
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
                    input_level[0].set_input(0, input_value, add);
                }
                1 => threshold.set_target(input_value),
                2 => ratio.set_target(input_value),
                3 => makeup.set_target(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
            Process::Env {
                ref mut input,
                ref mut input_level,
                ref mut comp_env,
                ..
            } => match idx {
                0 => {
                    if input_level.len() < 1 {
                        input_level.push(rms_proc());
                    }
                    set_or_add(input, input_value, add);
                    input_level[0].set_input(0, input_value, add);
                }
                1 => comp_env.min_target = input_value,
                2 => comp_env.max_target = input_value,
                3 => comp_env.max_n = input_value,
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
            Process::Ducking {
                ref mut input,
                ref mut input_level,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => {
                    if input_level.len() < 1 {
                        input_level.push(rms_proc());
                    }
                    input_level[0].set_input(0, input_value, add);
                }
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },

            Process::EnvFollow {
                ref mut input,
                ref mut input_level,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => {
                    if input_level.len() < 1 {
                        input_level.push(rms_proc());
                    }
                    input_level[0].set_input(0, input_value, add);
                }
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },

            Process::Constant { .. } => (),
            //        Process::Map { ref mut input, .. } => set_or_add(input, input_value, add),
            Process::SoundIn { ref mut factor, .. } => factor.set_target(input_value), // match idx {
            //     0 => set_or_add(input, input_value, add),
            //     //	1 => *index = input_value as usize,
            //     _ => panic!("wrong index into {}: {}", self.name(), idx),
            // },
            Process::Filter {
                ref mut input,
                ref mut filter,
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => filter.set_parameters(input_value, filter.q.current),
                2 => filter.set_parameters(filter.freq.current, input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
            Process::Resonator {
                ref mut input,
                ref mut freq_mod,
                ref mut freq_factor,
                ref mut freq_center,
                ref mut resonator,
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => set_or_add(freq_mod, input_value, add),
                2 => freq_center.set_target(input_value),
                3 => freq_factor.set_target(input_value),
                4 => resonator.set_parameters(resonator.freq, input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },

            Process::Sin {
                ref mut input,
                ref mut mul,
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => mul.set_target(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
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
                _ => panic!("wrong index into {}: {}", self.name(), idx),
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
                _ => panic!("wrong index into {}: {}", self.name(), idx),
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
                _ => panic!("wrong index into {}: {}", self.name(), idx),
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
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
            Process::LPF1 {
                ref mut input,
                ref mut freq,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => freq.set_target(input_value),
                //            1 => *p = lpf1_calc_p(input_value), // freq input
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
        }
    }

    pub fn spec(&self) -> ProcessSpec {
        match self {
            Process::Spike { .. } => procspec(
                "spike",
                ProcessType::OpaqueProcessor,
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
                vec![InputType::Index, InputType::Factor],
            ),
            Process::VanDerPol { .. } => procspec(
                "vanderpool",
                ProcessType::OpaqueProcessor,
                vec![
                    InputType::Audio,
                    InputType::Factor,
                    InputType::Factor,
                    InputType::Amplitude,
                ],
            ),
            Process::Sin { .. } => {
                procspec("sin", ProcessType::OpaqueProcessor, vec![InputType::Phase])
            }
            Process::Env { .. } => procspec(
                "sin",
                ProcessType::TransparentProcessor,
                vec![InputType::Audio],
            ),
            Process::SinOsc { .. } => procspec(
                "sinosc",
                ProcessType::OpaqueProcessor,
                vec![InputType::Frequency, InputType::Factor],
            ),
            Process::PLL { .. } => procspec(
                "pll",
                ProcessType::OpaqueProcessor,
                vec![InputType::Any, InputType::Factor],
            ),

            Process::Mul { .. } => {
                procspec("mul", ProcessType::MultipleInputs, vec![InputType::Any])
            }
            Process::Ring { .. } => {
                procspec("ring", ProcessType::MultipleInputs, vec![InputType::Audio])
            }
            Process::Add { .. } => {
                procspec("add", ProcessType::MultipleInputs, vec![InputType::Any])
            }
            Process::Mem { .. } => procspec(
                "mem",
                ProcessType::TransparentProcessor,
                vec![InputType::Any],
            ),
            Process::Constant { .. } => procspec("constant", ProcessType::NoInputGenerator, vec![]),
            //        Process::Map { .. } => procspec("map", vec![InputType::Any]),
            Process::Filter { .. } => procspec(
                "filter",
                ProcessType::TransparentProcessor,
                vec![InputType::Audio, InputType::Frequency, InputType::Q],
            ),
            Process::Resonator { .. } => procspec(
                "resonator",
                ProcessType::MultipleInputs,
                vec![
                    InputType::Audio,
                    InputType::Any,
                    InputType::Frequency,
                    InputType::Factor,
                    InputType::Seconds,
                ],
            ),

            Process::Compressor { .. } => procspec(
                "compressor",
                ProcessType::TransparentProcessor,
                vec![
                    InputType::Audio,
                    InputType::Threshold,
                    InputType::Parameter,
                    InputType::Amplitude,
                ],
            ),
            Process::Ducking { .. } => procspec(
                "ducking",
                ProcessType::SidechainEnv,
                vec![InputType::Any; 2],
            ),
            Process::EnvFollow { .. } => procspec(
                "envfollow",
                ProcessType::SidechainEnv,
                vec![InputType::Any; 2],
            ),

            //        Process::Noise { .. } => procspec("noise", vec![]),
            Process::Wrap { .. } => procspec(
                "wrap",
                ProcessType::OpaqueProcessor,
                vec![InputType::Any; 2],
            ),
            Process::Softclip { .. } => procspec(
                "softclip",
                ProcessType::TransparentProcessor,
                vec![InputType::Any],
            ),
            Process::Square { .. } => procspec(
                "square",
                ProcessType::TransparentProcessor,
                vec![InputType::Any],
            ),
            Process::Sqrt { .. } => procspec(
                "sqrt",
                ProcessType::TransparentProcessor,
                vec![InputType::Any],
            ),
            Process::Delay { .. } => procspec(
                "delay",
                ProcessType::TransparentProcessor,
                vec![InputType::Any],
            ),
            Process::VarDelay { .. } => procspec(
                "vardelay",
                ProcessType::TransparentProcessor,
                vec![InputType::Any, InputType::Seconds],
            ),
            Process::BitNeg { .. } => {
                procspec("bitneg", ProcessType::OpaqueProcessor, vec![InputType::Any])
            }
            Process::BitOr { .. } => {
                procspec("bitor", ProcessType::TwoInputs, vec![InputType::Any; 2])
            }
            Process::BitXOr { .. } => {
                procspec("bitxor", ProcessType::TwoInputs, vec![InputType::Any; 2])
            }
            Process::BitAnd { .. } => {
                procspec("bitand", ProcessType::TwoInputs, vec![InputType::Any; 2])
            }
            Process::CurveLin { .. } => procspec(
                "curvelin",
                ProcessType::TransparentProcessor,
                vec![InputType::Any; 6],
            ),
            Process::Gauss { .. } => procspec(
                "gauss",
                ProcessType::TransparentProcessor,
                vec![InputType::Audio],
            ),
            Process::RMS { .. } => {
                procspec("rms", ProcessType::OpaqueProcessor, vec![InputType::Audio])
            }
            Process::LPF1 { .. } => procspec(
                "lpf1",
                ProcessType::TransparentProcessor,
                vec![InputType::Audio, InputType::Frequency],
            ),
            Process::LinCon { .. } => procspec(
                "lincon",
                ProcessType::OpaqueProcessor,
                vec![InputType::Any, InputType::Factor, InputType::Factor],
            ),
        }
    }

    pub fn name(&self) -> String {
        self.spec().name
    }

    pub fn clear_inputs(&mut self) {
        match self {
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
	| Process::Spike { ref mut input, .. }
	| Process::VarDelay { ref mut input, .. }
	| Process::VanDerPol { ref mut input, .. }
	| Process::BitNeg { ref mut input } => *input = 0.0,
        Process::SinOsc { ref mut input, .. } => *input = 0.0,
	Process::Compressor { ref mut input, ref mut input_level, .. }
	| Process::Env { ref mut input, ref mut input_level, .. }
	| Process::EnvFollow { ref mut input, ref mut input_level, .. }
	| Process::Ducking { ref mut input, ref mut input_level, .. } => {
	    *input = 0.0;
	    clear_chain(input_level);
	    //input_level.iter_mut().for_each(|i| clear_inputs(i));
	},
	Process::Resonator { ref mut input, ref mut freq_mod, .. } => {
		*input = 0.0;
		*freq_mod = 0.0;
	},
	Process::PLL { ref mut input, .. } => input.input = 0.0,
	Process::Mul { ref mut inputs } | Process::Add { ref mut inputs } => inputs.clear(),
	| Process::Ring { ref mut inputs, ref mut input_counter } => {
	    *input_counter = 0;
	    for i in inputs.iter_mut() {
		i.clear_inputs();
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
}
