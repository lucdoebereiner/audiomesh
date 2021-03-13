use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
enum CompEnvState {
    Attack,
    Sustain,
    Decay,
}

impl Default for CompEnvState {
    fn default() -> CompEnvState {
        CompEnvState::Attack
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
    pub max_target: f64,
    pub min_target: f64,
    #[serde(skip)]
    n: usize,
    pub max_n: f64, // seconds
}

impl CompEnv {
    pub fn new(max_target: f64, min_target: f64, n: f64) -> CompEnv {
        CompEnv {
            fac: 1.0,
            state: CompEnvState::Attack,
            state_counter: 0,
            target: max_target,
            max_target: max_target,
            min_target: min_target,
            n: (n * 44100.0) as usize, // todo fix hardcoded sr
            max_n: n,
        }
    }

    pub fn process(&mut self, input: f64) {
        //        println!("{:?}", self);
        let adjusted_input = input * self.fac;
        let max_samples = (self.max_n * 44100.0) as usize; // todo fix hardcoded sr
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
                CompEnvState::Attack => {
                    if adjusted_input >= self.target {
                        self.state = CompEnvState::Sustain;
                        self.n = (self.state_counter % max_samples) + (max_samples / 2);
                        self.state_counter = 0;
                    } else {
                        self.state_counter += 1;
                        //self.n = (self.n - 1).max(1);
                    }
                    self.fac *=
                        (self.target / adjusted_input).powf(1.0 / ((self.n as f64) / 1.5).max(1.0));
                }
                CompEnvState::Sustain => {
                    if self.state_counter >= self.n {
                        self.state = CompEnvState::Decay;
                        self.target = self.min_target;
                        self.state_counter = 0;
                    }
                    self.state_counter += 1;
                    self.fac *=
                        (self.target / adjusted_input).powf(1.0 / ((self.n as f64) / 2.0).max(1.0));
                }
                CompEnvState::Decay => {
                    //                    println!("decay input {}", adjusted_input);
                    if adjusted_input <= self.target {
                        self.state = CompEnvState::Attack;
                        self.n = (self.state_counter % max_samples) + (max_samples / 2);
                        self.state_counter = 0;
                        self.target = self.max_target;
                    } else {
                        self.state_counter += 1;
                        //self.n = (self.n - 1).max(1);
                    }
                    self.fac *=
                        (self.target / adjusted_input).powf(1.0 / ((self.n as f64) / 1.5).max(1.0));
                }
            }
        }
    }
}
