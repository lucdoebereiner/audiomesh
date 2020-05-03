use serde::{Deserialize, Serialize};
use std::f64;

#[derive(Serialize, Deserialize, Debug)]
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

#[derive(Serialize, Deserialize, Debug)]
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
    coefficients: BiquadCoefficients,
    state: BiquadState,
    filter_type: FilterType,
    pub freq: f64,
    pub q: f64,
}

impl Biquad {
    pub fn new(filter_type: FilterType, freq: f64, q: f64, sample_rate: f64) -> Self {
        let mut filter = Biquad {
            coefficients: BiquadCoefficients::new(),
            state: BiquadState::new(),
            filter_type,
            freq: std::f64::NAN, // init with NAN, so update is always triggered
            q: std::f64::NAN,
        };
        filter.update_coefficients(freq, q, sample_rate);
        filter
    }

    pub fn process(&mut self, input: f64) -> f64 {
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

    pub fn update_coefficients(&mut self, freq: f64, q: f64, sample_rate: f64) {
        if freq != self.freq || q != self.q {
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
            self.freq = freq;
            self.q = q;
        }
    }
}
