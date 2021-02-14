//use std::f64;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

pub struct Lag {
    current: f64,
    target: f64,
    factor: f64,
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
        Ok(lag(current))
    }
}

pub fn lag(init: f64) -> Lag {
    Lag {
        current: init,
        target: init,
        factor: 0.9999,
    }
}

impl Lag {
    pub fn tick(&mut self) -> f64 {
        if (self.current - self.target).abs() < 0.00001 {
            self.current = self.target
        } else {
            self.current = self.target + self.factor * (self.current - self.target)
        }
        self.current
    }

    pub fn set_target(&mut self, new_target: f64) {
        self.target = new_target
    }
}