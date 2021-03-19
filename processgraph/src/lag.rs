use serde::{Deserialize, Deserializer, Serialize, Serializer};

const LOG001: f64 = -6.90775527898; //0.001_f64.ln();

#[derive(Debug)]
pub struct Lag {
    pub current: f64,
    pub target: f64,
    factor_up: f64,
    factor_down: f64,
}

impl Serialize for Lag {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_f64(self.current)
    }
}

impl<'de> Deserialize<'de> for Lag {
    fn deserialize<D>(deserializer: D) -> Result<Lag, D::Error>
    where
        D: Deserializer<'de>,
    {
        let current: f64 = <f64 as Deserialize>::deserialize(deserializer)?;
        Ok(lag(current)) // todo rethink this because factors are lost in (de)serializtion
    }
}

pub fn lag(init: f64) -> Lag {
    Lag {
        current: init,
        target: init,
        factor_up: 0.99998,
        factor_down: 0.99998,
    }
}

impl Lag {
    pub fn new(current: f64, duration: f64, sr: f64) -> Self {
        let mut l = lag(current);
        l.set_duration(duration, sr);
        l
    }

    pub fn tick(&mut self) -> f64 {
        if (self.current - self.target).abs() < 0.00001 {
            self.current = self.target
        } else {
            if self.target > self.current {
                self.current = self.target + self.factor_up * (self.current - self.target)
            } else {
                self.current = self.target + self.factor_down * (self.current - self.target)
            }
        }
        self.current
    }

    pub fn is_done(&self) -> bool {
        self.current == self.target
    }

    pub fn set_target(&mut self, new_target: f64) {
        self.target = new_target
    }

    pub fn set_duration(&mut self, duration: f64, sr: f64) {
        if duration == 0.0 {
            self.factor_up = 0.0;
            self.factor_down = 0.0;
        } else {
            self.factor_up = (LOG001 / (duration * sr)).exp();
            self.factor_down = (LOG001 / (duration * sr)).exp();
        }
    }

    pub fn set_factor(&mut self, factor: f64) {
        self.factor_up = factor;
        self.factor_down = factor;
    }

    pub fn set_duration_ud(&mut self, duration_up: f64, duration_down: f64, sr: f64) {
        if duration_up == 0.0 {
            self.factor_up = 0.0;
        } else {
            self.factor_up = (LOG001 / (duration_up * sr)).exp();
        }

        if duration_down == 0.0 {
            self.factor_down = 0.0;
        } else {
            self.factor_down = (LOG001 / (duration_down * sr)).exp();
        }
    }
}
