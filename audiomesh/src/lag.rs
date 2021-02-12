//use std::f64;

pub struct Lag {
    current: f64,
    target: f64,
    factor: f64,
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
