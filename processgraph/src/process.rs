use crate::compenv::*;
use crate::filters;
use crate::integrator::{runge_kutta_4, runge_kutta_5};
use crate::lag;
use crate::processspec::*;
use crate::numerical;
use crate::tapdelay;
use serde::{Deserialize, Serialize};
use serde::{Deserializer, Serializer};
use std::f64;
use strum_macros;
use strum::EnumProperty;
const PI: f64 = f64::consts::PI;

use crate::numerical::TWOPI;

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

#[derive(Serialize, Deserialize, Debug, strum_macros::EnumProperty)]
#[allow(dead_code)]
pub enum Process {
    #[strum(props(Name="Sin"))]
    Sin {
        #[serde(skip)]
        input: f64,
        mul: lag::Lag,
    },
    #[strum(props(Name="SinOsc"))]
    SinOsc {
        #[serde(skip)]
        input: f64,
        freq: lag::Lag,
        freq_mul: lag::Lag,
        #[serde(skip)]
        phase: f64,
    },
    #[strum(props(Name="Mul"))]
    Mul {
        #[serde(skip)]
        inputs: Vec<f64>,
    },
    Fold {
        #[serde(skip)]
        input: f64,
        threshold: lag::Lag,
        mul: lag::Lag,
        add: lag::Lag,
    },
    GateIfGreater {
        #[serde(skip)]
        input1: f64,
        #[serde(skip)]
        input2: f64,
    },
    Kaneko {
        #[serde(skip)]
        input1: f64,
        #[serde(skip)]
        input2: f64,
        #[serde(skip)]
        last_output: f64,
        e: lag::Lag,
        a: lag::Lag,
    },
    KanekoChain {
        #[serde(skip)]
        input1: f64,
        #[serde(skip)]
        input2: f64,
        #[serde(serialize_with = "vector_serialize")]
        #[serde(deserialize_with = "vector_deserialize")]
        last_outputs: Vec<f64>,
        e: lag::Lag,
        a: lag::Lag,
        #[serde(skip)]
        i: usize,
    },
    GateDecision {
        #[serde(skip)]
        input: f64,
        #[serde(skip)]
        other_level: Vec<Process>,
        #[serde(skip)]
        playing: bool,
        #[serde(skip)]
        counter: i32,
        min_dur_on: f64,
        max_dur_on: f64,
        min_dur_off: f64,
        max_dur_off: f64,
        #[serde(skip_serializing)]
        #[serde(default = "out_gate_lag")]
        out_lag: lag::Lag,
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
        #[serde(skip_serializing)]
        #[serde(default = "vdp_state")]
        state: Vec<f64>,
        frac: lag::Lag,
        e: lag::Lag,
        a: lag::Lag,
    },
    NoseHoover {
        #[serde(skip)]
        input: f64,
        #[serde(skip_serializing)]
        #[serde(default = "chua_state")]
        state: Vec<f64>,
        frac: lag::Lag,
        a: lag::Lag,
        coupling: lag::Lag,
    },
    FitzHughNagumo {
        #[serde(skip)]
        input: f64,
        #[serde(skip_serializing)]
        #[serde(default = "fitz_state")]
        state: Vec<f64>,
        frac: lag::Lag,
        a: lag::Lag,
        b: lag::Lag,
        c: lag::Lag,
        coupling: lag::Lag,
    },
    Chua {
        #[serde(skip)]
        input: f64,
        #[serde(skip_serializing)]
        #[serde(default = "chua_state")]
        state: Vec<f64>,
        frac: lag::Lag,
        coupling: lag::Lag,
        a: lag::Lag,
        b: lag::Lag,
        c: lag::Lag,
        bp: lag::Lag,
        m0: lag::Lag,
        m1: lag::Lag,
    },
    Duffing {
        #[serde(skip)]
        input: f64,
        #[serde(skip_serializing)]
        #[serde(default = "vdp_state")]
        state: Vec<f64>,
        frac: lag::Lag,
        e: lag::Lag,
        a: lag::Lag,
        b: lag::Lag,
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
    Perceptron {
        #[serde(skip)]
        input: f64,
        bias: lag::Lag,
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

pub fn lpf1_calc_p(freq: f64) -> f64 {
    1. - (2. * (freq / unsafe { SR }).tan())
}

pub fn lpf1_calc_freq(p: f64) -> f64 {
    ((1.0 - p).atan() * get_sr()) / 2.0
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

// fn dynsys_compressor() -> Box<Process> {
//     Box::new(Process::Compressor {
//         input_level: vec![],
//         input: 0.0,
//         threshold: lag::lag(0.25),
//         ratio: lag::lag(6.0),
//         makeup: lag::lag(1.0),
//     })
// }

pub fn vanderpol(e: f64, a: f64, frac: f64) -> Process {
    Process::VanDerPol {
        input: 0.0,
        state: vdp_state(),
        // x: 0.0,
        // y: 0.0,
        frac: lag::lag(frac),
        e: lag::lag(e),
        a: lag::lag(a),
    }
}

fn vdp_state() -> Vec<f64> {
    vec![0.1, 0.2]
}

fn chua_state() -> Vec<f64> {
    vec![0.001, 0.01, 0.001]
}

fn fitz_state() -> Vec<f64> {
    vec![0.01, 0.01]
}


fn ducking_lag() -> lag::Lag {
    let mut dlag = lag::lag(0.0);
    dlag.set_duration_ud(0.1, 0.1);
    dlag
}

fn out_gate_lag() -> lag::Lag {
    let mut dlag = lag::lag(0.0);
    dlag.set_duration_ud(0.05, 0.07);
    dlag
}

fn pll_error_lag() -> lag::Lag {
    let mut dlag = lag::lag(0.0);
    dlag.set_factor(0.4);
    dlag
}

pub fn dc_remove_filter() -> filters::OnePoleHP {
    filters::OnePoleHP::new(0.98)
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

#[inline]
fn kaneko_logisitc(x: f64, a: f64) -> f64 {
    1.0 - a * numerical::wrap(x, -1.0, 1.0).powi(2)
}

#[inline]
fn attrition(val: f64) -> f64 {
    1.0f64.min(0.1 * val.abs().powf(1.08) * val)
}

struct NoseHooverAdditionalVars {
    a: f64,
    coupling: f64,
    input: f64,
    f: f64,
}


struct ChuaAdditionalVars {
    a: f64,
    b: f64,
    c: f64,
    bp: f64,
    coupling: f64,
    input: f64,
    f: f64,
    m0: f64,
    m1: f64,
}

// #[inline]
// fn chua_diode(x: f64) -> f64 {
//     let m0 = -1.0 * (1.0/7.0);
//     let m1 = 2.0/7.0;
//     (m1 * x) + ( 0.5 *  (m0 - m1) * ((x+1.0).abs() - (x-1.0).abs()))
// }

// fn chua_diode(x: f64, bp: f64, m0: f64, m1: f64) -> f64 {
// //    let m0 = -4.0/7.0;
// //    let m1 = -2.0/7.0;
//     let out = (m1 * x) + ( 0.5 *  (m0 - m1) * ((x+bp).abs() - (x-bp).abs()));
//     out
// //    out.tanh()
// //    (m1 * x) + ( 0.5 *  (m0 - m1) * ((x+bp).abs() - (x-bp).abs()))
// }

fn chua_diode(x: f64, bp: f64, m0: f64, m1: f64) -> f64 {
    if (x >= -bp) && (x <= bp) {
        m0 * x
    } else if x >= bp {
        m1 * x + (m0 - m1)
    } else {
        m1 * x + (m1 - m0)
    }
}

#[inline]
fn chua_calc_vec(state: &[f64], additional_vars: &ChuaAdditionalVars) -> Vec<f64> {
    let x = state[0];
    let y = state[1];
    let z = state[2];

    let a = additional_vars.a;
    let b = additional_vars.b;
    let c = additional_vars.c;
    let bp = additional_vars.bp;
    let m0 = additional_vars.m0;
    let m1 = additional_vars.m1;
    // cubic
//    let mut d_x =
  //      a * (y - x.powi(3) - (c * x)) + (additional_vars.coupling * additional_vars.input);

    let d_x = a * (y - x - chua_diode(x, bp, m0, m1)) + (additional_vars.coupling * additional_vars.input);
    let d_y = x - y + z;
    let d_z = -b * y - (c * z); 

    // d_x = (d_x / 2.0).tanh() * 2.0;
    // d_y = (d_y / 2.0).tanh() * 2.0;
    // d_z = (d_z / 2.0).tanh() * 2.0;
    
    // let mut d_x = a * (y - x - chua_diode(x, bp, m0, m1)) + (additional_vars.coupling * additional_vars.input);
    // let mut d_y = x - y + z;
    // let mut d_z = -b * y - (c * z);
    
    // let mut d_x = additional_vars.a * (y - x - chua_h(x))
    //     + (additional_vars.coupling * additional_vars.input);
    // let mut d_x = additional_vars.a * y - x + (x + 1.0).abs() - (x - 1.0).abs()
    //     + (additional_vars.coupling * additional_vars.input);
    //  d_x = ((d_x / tan_fac).tanh() * tan_fac); // - attrition(x);
    
    //    d_y = ((d_y / tan_fac).tanh() * tan_fac); // - attrition(y);

    //  d_z = ((d_z / tan_fac).tanh() * tan_fac); // - attrition(z);

    vec![
        d_x * additional_vars.f,
        d_y * additional_vars.f,
        d_z * additional_vars.f,
    ]
}


#[inline]
fn nose_hoover_calc_vec(state: &[f64], additional_vars: &NoseHooverAdditionalVars) -> Vec<f64> {
    let x = state[0];
    let y = state[1];
    let z = state[2];

    let d_x = additional_vars.a * y  + (additional_vars.input * additional_vars.coupling);
    let d_y = -x + (y * z);
    let d_z = 1.0 - y.powi(2);

    vec![d_x * additional_vars.f, d_y * additional_vars.f, d_z * additional_vars.f]
}


#[inline]
fn vdp_calc_vec(state: &[f64], additional_vars: &VDPAdditionalVars) -> Vec<f64> {
    let x = state[0];
    let y = state[1];
    let tan_fac = 10.0;
    let mut d_x = y;
    d_x = ((d_x / tan_fac).tanh() * tan_fac) - attrition(x);
    let mut d_y = ((additional_vars.e * (1.0 - x.powi(2)) * y) - x)
        + (additional_vars.input * additional_vars.a);
    d_y = ((d_y / tan_fac).tanh() * tan_fac) - attrition(y);
    vec![d_x * additional_vars.f, d_y * additional_vars.f]
}

struct VDPAdditionalVars {
    e: f64,
    a: f64,
    input: f64,
    f: f64,
}

struct DuffingAdditionalVars {
    e: f64,
    a: f64,
    b: f64,
    input: f64,
    f: f64,
}

struct FitzAdditionalVars {
    a: f64,
    b: f64,
    c: f64,
    input: f64,
    f: f64,
    coupling: f64,
}


#[inline]
fn fitz_calc_vec(state: &[f64], additional_vars: &FitzAdditionalVars) -> Vec<f64> {
    let x = state[0];
    let y = state[1];

    let d_x = x - x.powi(3) - y + (additional_vars.input * additional_vars.coupling);
    let d_y = additional_vars.a + (additional_vars.b * x) - (additional_vars.c * y);
        
    vec![d_x * additional_vars.f, d_y * additional_vars.f]
}


#[inline]
fn duffing_calc_vec(state: &[f64], additional_vars: &DuffingAdditionalVars) -> Vec<f64> {
    let x = state[0];
    let y = state[1];
//    let tan_fac = 10.0;
    let mut d_x = y;
    d_x = d_x - attrition(x);
    //    d_x = ((d_x / tan_fac).tanh() * tan_fac) - attrition(x);
    let mut d_y = x - (additional_vars.b * x.powi(3)) - (additional_vars.e * y)
        + (additional_vars.input * additional_vars.a);
    d_y = d_y - attrition(y);
    //  d_y = ((d_y / tan_fac).tanh() * tan_fac) - attrition(y);
    vec![d_x * additional_vars.f, d_y * additional_vars.f]
}

// let mut d_x_n = *y;
// let mut d_y_n = *x - (*x).powi(3) - this_e * *y + (*input * this_a);

impl Process {
    pub fn process(&mut self, external_input: &[f64]) -> f64 {
        match self {
            Process::GateIfGreater { input1, input2 } => {
                if input1.abs() > input2.abs() {
                    *input1
                } else {
                    0.0
                }
            }
            Process::SoundIn {
                input,
                index,
                ref mut factor,
            } => (*input + (*external_input.get(*index).unwrap_or(&0.0) * factor.tick())),
            Process::Sin { input, mul } => (*input * mul.tick()).sin(),
            Process::Fold {
                input,
                ref mut threshold,
                ref mut mul,
                ref mut add,
            } => (numerical::fold(*input, threshold.tick(), mul.tick(), add.tick())),
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
                ref mut state,
                ref mut frac,
                ref mut e,
                ref mut a,
            } => {
                let this_e = e.tick();
                let f = frac.tick();
                let this_a = a.tick();

                let additional_vars = VDPAdditionalVars {
                    e: this_e,
                    input: *input,
                    a: this_a,
                    f: f,
                };
                let new_state =
                    runge_kutta_5(&vdp_calc_vec, &state, &additional_vars, 6.0 / get_sr());
                //                    runge_kutta_6(&vdp_calc_vec, &state, &additional_vars, 6.0 / get_sr());

                state[0] = new_state[0];
                state[1] = new_state[1];
                if (state[0].abs() > 10.0) || (state[1].abs() > 10.0) {
                    state[0] = state[0] - (state[0] * 0.1);
                    state[1] = state[1] - (state[1] * 0.1);
                }
                state[0] * (((50.0 - this_e) / 50.0) + 0.1).min(1.0)
            }
            Process::Chua {
                input,
                ref mut state,
                ref mut frac,
                ref mut a,
                ref mut b,
                ref mut c,
                ref mut bp,
                ref mut coupling,
                ref mut m0,
                ref mut m1,
            } => {
                let this_coupling = coupling.tick();
                let f = frac.tick();
                let this_c = c.tick();
                let this_a = a.tick();
                let this_b = b.tick();
                let this_bp = bp.tick();
                let this_m0 = m0.tick();
                let this_m1 = m1.tick();

                let additional_vars = ChuaAdditionalVars {
                    input: *input,
                    a: this_a,
                    b: this_b,
                    c: this_c,
                    bp: this_bp,
                    f,
                    coupling: this_coupling,
                    m0: this_m0,
                    m1: this_m1,
                };
                let new_state =
                    runge_kutta_5(&chua_calc_vec, &state, &additional_vars, 6.0 / get_sr());

                // state[0] = new_state[0];
                // state[1] = new_state[1];
                // state[2] = new_state[2];
                
                state[0] = (new_state[0] / 100.0).tanh() * 100.0;
                state[1] = (new_state[1] / 100.0).tanh() * 100.0;
                state[2] = (new_state[2] / 100.0).tanh() * 100.0;
                // if (state[0].abs() > 30.0) || (state[1].abs() > 30.0) || (state[2].abs() > 30.0) {
                //     state[0] = state[0] - (state[0] * 0.1);
                //     state[1] = state[1] - (state[1] * 0.1);
                //     state[2] = state[2] - (state[2] * 0.1);
                // }
                state[0]
            }
            Process::NoseHoover {
                input,
                ref mut state,
                ref mut frac,
                ref mut coupling,
                ref mut a,
            } => {
                let this_coupling = coupling.tick();
                let f = frac.tick();
                let this_a = a.tick();

                let additional_vars = NoseHooverAdditionalVars {
                    input: *input,
                    a: this_a,
                    f: f,
                    coupling: this_coupling,
                };
                let new_state =
                    runge_kutta_5(&nose_hoover_calc_vec, &state, &additional_vars, 6.0 / get_sr());

                state[0] = (new_state[0] / 100.0).tanh() * 100.0;
                state[1] = (new_state[1] / 100.0).tanh() * 100.0;
                state[2] = (new_state[2] / 100.0).tanh() * 100.0;
                
                state[0] 
            }
            Process::FitzHughNagumo {
                input,
                ref mut state,
                ref mut frac,
                ref mut coupling,
                ref mut a,
                ref mut b,
                ref mut c,
            } => {
                let this_coupling = coupling.tick();
                let f = frac.tick();
                let this_a = a.tick();
                let this_b = b.tick();
                let this_c = c.tick();

                let additional_vars = FitzAdditionalVars {
                    input: *input,
                    a: this_a,
                    b: this_b,
                    c: this_c,
                    f: f,
                    coupling: this_coupling,
                };
                let new_state =
                    runge_kutta_5(&fitz_calc_vec, &state, &additional_vars, 6.0 / get_sr());

                state[0] = (new_state[0] / 100.0).tanh() * 100.0;
                state[1] = (new_state[1] / 100.0).tanh() * 100.0;
                
                state[0] 
            }                     
            Process::Duffing {
                input,
                ref mut state,
                ref mut frac,
                ref mut e,
                ref mut a,
                ref mut b,
            } => {
                let this_e = e.tick();
                let f = frac.tick();
                let this_a = a.tick();
                let this_b = b.tick();

                let additional_vars = DuffingAdditionalVars {
                    e: this_e,
                    input: *input,
                    a: this_a,
                    b: this_b,
                    f: f,
                };
                let new_state =
                    runge_kutta_4(&duffing_calc_vec, &state, &additional_vars, 6.0 / get_sr());

                state[0] = new_state[0];
                state[1] = new_state[1];
                if (state[0].abs() > 10.0) || (state[1].abs() > 10.0) {
                    state[0] = state[0] - (state[0] * 0.1);
                    state[1] = state[1] - (state[1] * 0.1);
                }
                state[0]
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
            Process::Kaneko {
                input1,
                input2,
                ref mut last_output,
                ref mut e,
                ref mut a,
            } => {
                let this_e = e.tick();
                let this_a = a.tick();
                *last_output = (1.0 - this_e) * kaneko_logisitc(*last_output, this_a)
                    + ((this_e / 2.0)
                        * (kaneko_logisitc(*input1, this_a) + kaneko_logisitc(*input2, this_a)));
                *last_output
            }

            Process::KanekoChain {
                input1,
                input2,
                ref mut last_outputs,
                ref mut e,
                ref mut a,
                ref mut i,
            } => {
                let this_e = e.tick();
                let this_a = a.tick();

                if *i == 0 {
                    for k in 0..last_outputs.len() {
                        let prev = if k == 0 { *input1 } else { last_outputs[k - 1] };
                        let next = if k == (last_outputs.len() - 1) {
                            *input2
                        } else {
                            last_outputs[k + 1]
                        };

                        last_outputs[k] = (1.0 - this_e) * kaneko_logisitc(last_outputs[k], this_a)
                            + ((this_e / 2.0)
                                * (kaneko_logisitc(prev, this_a) + kaneko_logisitc(next, this_a)));
                    }
                }

                let out = last_outputs[*i];
                *i = (*i + 1) % last_outputs.len();
                out
            }

            Process::GateDecision {
                input,
                ref mut other_level,
                ref mut playing,
                ref mut counter,
                min_dur_on,
                max_dur_on,
                min_dur_off,
                max_dur_off,
                ref mut out_lag,
            } => {
                let level: f64 = other_level
                    .iter_mut()
                    .map(|p| p.process(external_input))
                    .sum();

                if *playing {
                    if *counter > ((*max_dur_on * get_sr()) as i32) {
                        *counter = 0;
                        *playing = false;
                    } else {
                        if *counter > ((*min_dur_on * get_sr()) as i32) {
                            if level > 0.1 {
                                *playing = false;
                                *counter = 0;
                            }
                        }
                    }
                } else {
                    if *counter > ((*max_dur_off * get_sr()) as i32) {
                        *counter = 0;
                        *playing = true;
                    } else {
                        if *counter > ((*min_dur_off * get_sr()) as i32) {
                            if level < 0.05 {
                                *playing = true;
                                *counter = 0;
                            }
                        }
                    }
                }

                *counter += 1;
                if *playing {
                    out_lag.set_target(1.0);
                } else {
                    out_lag.set_target(0.0);
                }
                *input * out_lag.tick()
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
                    let pulse_width = *t_this_rest as f64 / 20.0;
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
                    *v = *last_v
                        + (((*last_v * -1.0 * 0.01) + (input.abs() * r.tick())) * this_const);
                    *last_v = *v;
                    if *v > threshold.tick() {
                        //output = 1.0;
                        *t_this_rest = *t_rest;
                        *t_rest_counter = *t_rest;
                    }
                }
                output
            }
            Process::Wrap { input, lo, hi } => numerical::wrap(*input, *lo, *hi),
            Process::Softclip { input } => input.tanh(),
            Process::Perceptron { input, bias } => {
                bias.tick();
                (*input + bias.current).tanh()
            },
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

    pub fn is_output(&self) -> bool {
        match self {
            Process::Mem { .. }
            | Process::SoundIn { .. }
            | Process::Delay { .. }
            | Process::RMS { .. } => false,
            _ => true,
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
            Process::Perceptron {
                ref mut input,
                ref mut bias,
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => bias.set_target(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
            Process::Chua {
                ref mut input,
                ref mut a,
                ref mut b,
                ref mut c,
                ref mut bp,
                ref mut coupling,
                ref mut frac,
                ref mut m0,
                ref mut m1,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => a.set_target(input_value),
                2 => b.set_target(input_value),
                3 => c.set_target(input_value),
                4 => frac.set_target(input_value),
                5 => coupling.set_target(input_value),
                6 => bp.set_target(input_value),
                7 => m0.set_target(input_value),
                8 => m1.set_target(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },
            Process::Fold {
                ref mut input,
                ref mut threshold,
                ref mut mul,
                add: ref mut fold_add,
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => threshold.set_target(input_value),
                2 => mul.set_target(input_value),
                3 => fold_add.set_target(input_value),
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

            Process::NoseHoover {
                ref mut input,
                ref mut coupling,
                ref mut frac,
                ref mut a,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => a.set_target(input_value),
                2 => frac.set_target(input_value),
                3 => coupling.set_target(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },

            Process::FitzHughNagumo {
                ref mut input,
                ref mut coupling,
                ref mut frac,
                ref mut a,
                ref mut b,
                ref mut c,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => a.set_target(input_value),
                2 => b.set_target(input_value),
                3 => c.set_target(input_value),
                4 => frac.set_target(input_value),
                5 => coupling.set_target(input_value),
                _ => panic!("wrong index into {}: {}", self.name(), idx),
            },

            Process::Duffing {
                ref mut input,
                ref mut e,
                ref mut frac,
                ref mut a,
                ref mut b,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => e.set_target(input_value),
                2 => frac.set_target(input_value),
                3 => a.set_target(input_value),
                4 => b.set_target(input_value),
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
            Process::Kaneko {
                ref mut input1,
                ref mut input2,
                ref mut e,
                ref mut a,
                ..
            }
            | Process::KanekoChain {
                ref mut input1,
                ref mut input2,
                ref mut e,
                ref mut a,
                ..
            } => match idx {
                0 => set_or_add(input1, input_value, add),
                1 => set_or_add(input2, input_value, add),
                2 => e.set_target(input_value),
                3 => a.set_target(input_value),
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
                4 => comp_env.sustain_fac = input_value,
                5 => comp_env.rest_fac = input_value,
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

            Process::GateDecision {
                ref mut input,
                ref mut other_level,
                ref mut min_dur_on,
                ref mut max_dur_on,
                ref mut min_dur_off,
                ref mut max_dur_off,
                ..
            } => match idx {
                0 => set_or_add(input, input_value, add),
                1 => {
                    if other_level.len() < 1 {
                        other_level.push(rms_proc());
                    }
                    other_level[0].set_input(0, input_value, add);
                }
                2 => *min_dur_on = input_value,
                3 => *max_dur_on = input_value,
                4 => *min_dur_off = input_value,
                5 => *max_dur_off = input_value,
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
            | Process::GateIfGreater {
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

    pub fn name(&self) -> String {
        self.get_str("Name").unwrap().to_string()
    }


    pub fn spec(&self) -> &ProcessSpec {
        SPECS.get(&self.name()).unwrap()
    }

    pub fn is_input(&self) -> bool {
        match self {
            Process::SoundIn { .. } => true,
            _ => false,
        }
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
            | Process::Perceptron { ref mut input, .. }
            | Process::Mem { ref mut input, .. }
	    | Process::RMS { ref mut input, .. }
            | Process::LPF1 { ref mut input, .. }
	    | Process::LinCon { ref mut input, .. }
	    | Process::Spike { ref mut input, .. }
	    | Process::VarDelay { ref mut input, .. }
	    | Process::Duffing { ref mut input, .. }
            | Process::Chua { ref mut input, .. }
	    | Process::Fold { ref mut input, .. }
            | Process::NoseHoover { ref mut input, .. }
            | Process::FitzHughNagumo { ref mut input, .. }
	    | Process::BitNeg { ref mut input } => *input = 0.0,
            Process::SinOsc { ref mut input, .. } => *input = 0.0,
	    Process::Compressor { ref mut input, ref mut input_level, .. }
	    | Process::Env { ref mut input, ref mut input_level, .. }
	    | Process::EnvFollow { ref mut input, ref mut input_level, .. }
	    | Process::Ducking { ref mut input, ref mut input_level, .. } => {
	        *input = 0.0;
	        clear_chain(input_level);
	    },

            | Process::VanDerPol { ref mut input, .. } => {
	        *input = 0.0;
            }

	    Process::GateDecision { ref mut input, ref mut other_level, .. } => {
	        *input = 0.0;
	        clear_chain(other_level);
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
            Process::Delay {
                ref mut input,
                rec_idx,
            } => input[*rec_idx] = 0.0,
            Process::BitOr {
                ref mut input1,
                ref mut input2,
            }
	    | Process::GateIfGreater {
                ref mut input1,
                ref mut input2,
            }

            | Process::BitXOr {
                ref mut input1,
                ref mut input2,
            }

            | Process::Kaneko {
                ref mut input1,
                ref mut input2,
	        ..
            }
            | Process::KanekoChain {
                ref mut input1,
                ref mut input2,
	        ..
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
