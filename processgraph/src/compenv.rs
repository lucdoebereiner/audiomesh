use crate::process::get_sr;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
enum CompEnvState {
    Attack,
    Sustain,
    Decay,
    Rest,
}

impl Default for CompEnvState {
    fn default() -> CompEnvState {
        CompEnvState::Rest
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CompEnv {
    #[serde(skip)]
    pub fac: f64,
    #[serde(skip)]
    state: CompEnvState,
    #[serde(skip)]
    state_counter: usize,
    #[serde(skip)]
    target: f64,
    #[serde(skip)]
    accum: f64,
    pub max_target: f64,
    pub min_target: f64,
    #[serde(skip)]
    n: usize,
    pub max_n: f64, // seconds
    pub rest_fac: f64,
    pub sustain_fac: f64,
}

impl CompEnv {
    pub fn new(
        max_target: f64,
        min_target: f64,
        n: f64,
        sustain_fac: f64,
        rest_fac: f64,
    ) -> CompEnv {
        CompEnv {
            fac: 1.0,
            state: CompEnvState::Rest,
            state_counter: 0,
            accum: 0.0,
            target: max_target,
            max_target: max_target,
            min_target: min_target,
            n: (n * get_sr()) as usize,
            max_n: n,
            rest_fac: rest_fac,
            sustain_fac: sustain_fac,
        }
    }

    pub fn process(&mut self, input: f64) {
        //        println!("{:?}", self);
        let adjusted_input = input * self.fac;
        let max_samples = (self.max_n * get_sr()) as usize;
        if self.n == 0 {
            self.n = max_samples;
        }
        if self.target == 0.0 {
            self.target = self.max_target;
        }
        if self.fac == 0.0 {
            self.fac = 1.0
        }
        if input > 0.0 {
            match self.state {
                CompEnvState::Rest => {
                    if self.accum > 0.5 {
                        // threshold
                        self.state = CompEnvState::Attack;
                        self.accum = 0.0;
                        self.fac = 0.001;
                    } else {
                        self.fac = 0.0;
                        self.accum += (input * (self.rest_fac / get_sr())
                            - (self.accum * (0.001 / get_sr())))
                        .max(0.0);
                    }
                }
                CompEnvState::Attack => {
                    if adjusted_input >= self.target {
                        self.state = CompEnvState::Sustain;
                        self.n = (self.state_counter % max_samples) + (max_samples / 2);
                        self.state_counter = 0;
                    } else {
                        self.state_counter += 1;
                        //self.n = (self.n - 1).max(1);
                    }
                    self.fac *= (self.target / adjusted_input)
                        .powf(1.0 / ((self.n - self.state_counter) as f64).max(1.0))
                    //                        .powf(1.0 / self.n as f64)
                    //.max(1.0);
                }
                CompEnvState::Sustain => {
                    if self.state_counter >= self.n {
                        self.state = CompEnvState::Decay;
                        self.target = self.min_target;
                        self.state_counter = 0;
                    }
                    self.state_counter += 1;
                    self.fac *= (self.target / adjusted_input)
                        .powf(1.0 / ((self.n as f64 * self.sustain_fac) / 4.0).max(1.0));
                }
                CompEnvState::Decay => {
                    //                    println!("decay input {}", adjusted_input);
                    if adjusted_input <= self.target {
                        self.state = CompEnvState::Rest;
                        self.n = (self.state_counter % max_samples) + (max_samples / 2);
                        self.state_counter = 0;
                        self.target = self.max_target;
                    } else {
                        self.state_counter += 1;
                        //self.n = (self.n - 1).max(1);
                    }
                    self.fac *= (self.target / adjusted_input)
                        .powf(1.0 / ((self.n - self.state_counter) as f64).max(1.0))
                    // .max(1.0);
                }
            }
        }
    }
}
