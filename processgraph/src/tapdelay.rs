use crate::lag::*;
use crate::process::get_sr;
use serde::{Deserialize, Serialize};
use serde::{Deserializer, Serializer};

#[derive(Serialize, Deserialize, Debug)]
pub struct TapDelay {
    #[serde(rename = "delay")]
    #[serde(serialize_with = "reader_serialize")]
    #[serde(deserialize_with = "reader_deserialize")]
    readers: Vec<(usize, Lag)>,
    #[serde(skip)]
    writer: usize,
    #[serde(rename = "maxdelay")]
    #[serde(serialize_with = "buffer_serialize")]
    #[serde(deserialize_with = "buffer_deserialize")]
    buffer: Vec<f64>,
}

fn buffer_serialize<S, T>(x: &Vec<T>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    s.serialize_f64(x.len() as f64 / get_sr())
}

fn buffer_deserialize<'de, D>(deserializer: D) -> Result<Vec<f64>, D::Error>
where
    D: Deserializer<'de>,
{
    let buffer_secs: f64 = <f64 as Deserialize>::deserialize(deserializer)?;
    let buffer_smpls = (buffer_secs * get_sr()) as usize;
    Ok(vec![0.0f64; buffer_smpls])
}

fn reader_serialize<S>(x: &Vec<(usize, Lag)>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let delay = x
        .iter()
        .find(|(_, l)| l.target == 1.0)
        .map_or(0.0, |(idx, _)| *idx as f64 / get_sr());
    s.serialize_f64(delay)
}

fn reader_deserialize<'de, D>(deserializer: D) -> Result<Vec<(usize, Lag)>, D::Error>
where
    D: Deserializer<'de>,
{
    let delay_secs: f64 = <f64 as Deserialize>::deserialize(deserializer)?;
    let delay_smpls = (delay_secs * get_sr()) as usize;
    let mut l = lag(1.0);
    l.set_duration(0.8);
    Ok(vec![(delay_smpls, l)])
}

impl TapDelay {
    pub fn new(length_sec: f64) -> TapDelay {
        //        println!("tap length {}", length_sec * get_sr());
        TapDelay {
            readers: vec![],
            writer: 0,
            buffer: vec![0.0; (length_sec * get_sr()) as usize],
        }
    }

    fn reader_idx(&self, reader: usize) -> usize {
        let length = self.buffer.len() as i32;
        let difference = self.writer as i32 - reader as i32;
        ((length + difference) % length) as usize
    }

    fn tick(&mut self) -> f64 {
        let mut sum = 0.0;
        for i in 0..self.readers.len() {
            sum += self.buffer[self.reader_idx(self.readers[i].0)] * self.readers[i].1.tick();
        }
        sum
    }

    fn clean_zero_readers(&mut self) {
        self.readers.retain(|(_, l)| l.current != 0.0);
        if self.readers.is_empty() {
            self.set_delay(0.0);
        }
    }

    pub fn set_delay(&mut self, delay_secs: f64) {
        let delay = ((delay_secs * get_sr()) as usize) % self.buffer.len();
        if !self.readers.iter().any(|(idx, _)| *idx == delay) {
            let mut l = lag(1.0);
            l.set_duration(0.8);
            self.readers.push((delay, l));
        }
        for r in self.readers.iter_mut() {
            if r.0 == delay {
                r.1.set_target(1.0);
            } else {
                r.1.set_target(0.0);
            }
        }
    }

    // pub fn set_input(&mut self, input: f64) {
    //     self.buffer[self.writer] = input
    // }

    pub fn process(&mut self, input: f64) -> f64 {
        self.clean_zero_readers();
        self.buffer[self.writer] = input;
        let output = self.tick();
        self.writer = (self.writer + 1) % self.buffer.len();
        output
    }
}
