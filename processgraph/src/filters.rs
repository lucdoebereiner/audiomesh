use serde::{Deserialize, Serialize};
use std::f64;

use crate::lag::*;
use crate::numerical::{zapgremlins, TWOPI};

#[derive(Serialize, Deserialize, Debug, Default)]
struct BiquadCoefficients {
    a0: f64,
    a1: f64,
    a2: f64,
    b0: f64,
    b1: f64,
    b2: f64,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum FilterType {
    BLPF,
    BHPF,
    BBPF,
}

#[derive(Serialize, Deserialize, Debug, Default)]
struct BiquadState {
    x1: f64,
    x2: f64,
    y1: f64,
    y2: f64,
}

impl BiquadState {
    fn new() -> Self {
        BiquadState {
            x1: 0.,
            x2: 0.,
            y1: 0.,
            y2: 0.,
        }
    }
}

impl BiquadCoefficients {
    fn new() -> Self {
        BiquadCoefficients {
            a0: 0.,
            a1: 0.,
            a2: 0.,
            b0: 0.,
            b1: 0.,
            b2: 0.,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Biquad {
    #[serde(skip)]
    coefficients: BiquadCoefficients,
    #[serde(skip)]
    state: BiquadState,
    filter_type: FilterType,
    #[serde(skip_serializing)]
    #[serde(default = "sr_default")]
    sample_rate: f64,
    pub freq: Lag,
    pub q: Lag,
}

pub fn sr_default() -> f64 {
    44100f64
}

impl Biquad {
    pub fn new(filter_type: FilterType, freq: f64, q: f64, sample_rate: f64) -> Self {
        let mut filter = Biquad {
            coefficients: BiquadCoefficients::new(),
            state: BiquadState::new(),
            filter_type,
            sample_rate,
            freq: lag(freq),
            q: lag(q),
        };
        filter.init(sample_rate);
        filter
    }

    pub fn init(&mut self, sr: f64) {
        self.sample_rate = sr;
        self.update_coefficients(true);
    }

    pub fn set_parameters(&mut self, freq: f64, q: f64) {
        self.freq.set_target(freq);
        self.q.set_target(q);
    }

    pub fn process(&mut self, input: f64) -> f64 {
        self.freq.tick();
        self.q.tick();
        self.update_coefficients(false);
        let y = (input * self.coefficients.b0)
            + (self.state.x1 * self.coefficients.b1)
            + (self.state.x2 * self.coefficients.b2)
            - (self.state.y1 * self.coefficients.a1)
            - (self.state.y2 * self.coefficients.a2);
        self.state.x2 = self.state.x1;
        self.state.x1 = input;
        self.state.y2 = self.state.y1;
        self.state.y1 = y;
        y
    }

    pub fn update_coefficients(&mut self, force: bool) {
        if !self.freq.is_done() || !self.q.is_done() || force {
            let sample_rate = self.sample_rate;
            let freq = self.freq.current;
            let q = self.q.current;
            let w = f64::consts::PI * 2. * (freq / sample_rate);
            let a = w.sin() / (q * 2.);
            let cosw = w.cos();
            let a0 = 1. + a;
            self.coefficients.a0 = a0;
            self.coefficients.a1 = (-2. * cosw) / a0;
            self.coefficients.a2 = (1. - a) / a0;
            match self.filter_type {
                FilterType::BLPF => {
                    self.coefficients.b0 = ((1. - cosw) / 2.) / a0;
                    self.coefficients.b1 = (1. - cosw) / a0;
                    self.coefficients.b2 = self.coefficients.b0;
                }
                FilterType::BHPF => {
                    self.coefficients.b0 = ((1. + cosw) / 2.) / a0;
                    self.coefficients.b1 = ((1. + cosw) * -1.) / a0;
                    self.coefficients.b2 = self.coefficients.b0;
                }
                FilterType::BBPF => {
                    self.coefficients.b0 = a / a0;
                    self.coefficients.b1 = 0.;
                    self.coefficients.b2 = (a * -1.) / a0;
                }
            }
        }
    }
}

// ComplexRes

// fn cr_default_freq() -> Lag {
//     let mut l = lag(0.0);
//     l.set_factor(0.2);
//     l
// }

#[derive(Serialize, Deserialize, Debug)]
pub struct ComplexRes {
    //    #[serde(default = "cr_default_freq")]
    #[serde(skip)]
    pub freq: f64,
    #[serde(skip)]
    coeff_x: f64,
    #[serde(skip)]
    coeff_y: f64,
    pub decay: Lag, // Exponential decay time constant in seconds
    #[serde(skip)]
    res: f64, // Filter resonance coefficient (0...1)
    #[serde(skip)]
    x: f64, // First state (real part)
    #[serde(skip)]
    y: f64, // Second state (imaginary part)
    #[serde(skip)]
    norm_coeff: f64, // Normalisation gain
    #[serde(skip)]
    ang: f64,
    #[serde(skip_serializing)]
    #[serde(default = "sr_default")]
    sr: f64,
}

impl ComplexRes {
    pub fn new(freq: f64, decay: f64, sr: f64) -> Self {
        let mut cr = ComplexRes {
            freq: freq,
            coeff_x: 0.0,
            coeff_y: 0.0,
            decay: lag(decay),
            res: 0.0,
            x: 0.0,
            y: 0.0,
            norm_coeff: 0.0,
            ang: 0.0,
            sr: sr,
        };
        //        cr.freq.set_factor(0.5);
        cr.decay.set_factor(0.5);
        cr.update_coeff();
        cr
    }

    pub fn update_coeff(&mut self) {
        //        if !self.freq.is_done() || !self.decay.is_done() || force {
        self.res = (-1.0 / (self.decay.current * self.sr)).exp();
        self.norm_coeff = (1.0 - self.res.powi(2)) / self.res;
        self.coeff_x = self.res * (TWOPI * self.freq / self.sr).cos();
        self.coeff_y = self.res * (TWOPI * self.freq / self.sr).sin();
        self.ang = (self.freq / self.sr) * TWOPI;
        //      }
    }

    pub fn process(&mut self, input: f64) -> f64 {
        //        self.freq.tick();
        self.decay.tick();
        self.update_coeff();
        let x = self.coeff_x * self.x - self.coeff_y * self.y + input;
        let y = self.coeff_y * self.x - self.coeff_x * self.y;
        self.x = zapgremlins(x);
        self.y = zapgremlins(y);
        y * self.norm_coeff
    }

    pub fn init(&mut self, sr: f64) {
        self.sr = sr;
        self.update_coeff();
    }

    pub fn set_parameters(&mut self, freq: f64, decay: f64) {
        //        self.freq.set_target(freq);
        self.freq = freq;
        self.decay.set_target(decay);
    }
}

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct OnePoleHP {
    pub input: f64,
    last_in: f64,
    last_out: f64,
    x: f64,
    w: f64,
}

impl OnePoleHP {
    pub fn new(x: f64) -> Self {
        OnePoleHP {
            input: 0.0,
            last_in: 0.0,
            last_out: 0.0,
            x: x,
            w: 0.0,
        }
    }
    pub fn process(&mut self) -> f64 {
        // https://sam-koblenski.blogspot.com/2015/11/everyday-dsp-for-programmers-dc-and.html
        let w_n = zapgremlins(self.input + (self.x * self.w));
        let output = w_n - self.w;
        self.w = w_n;
        output
        // actual one pole, creating artefacts ??
        // let a0 = ((1.0 + self.x) / 2.0) * self.input;
        // let a1 = ((-1.0 * (1.0 + self.x)) / 2.0) * self.last_in;
        // let b = self.last_out * self.x;
        // let output = a0 + a1 + b;
        // self.last_out = output;
        // self.last_in = self.input;
        // output
    }
}
