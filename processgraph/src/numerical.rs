pub fn modulo(input: f64, hi: f64) -> f64 {
    let mut result = input;
    let lo = 0.;
    if result >= hi {
        result -= hi;
        if result < hi {
            return result;
        };
    } else if result < lo {
        result += hi;
        if result >= lo {
            return result;
        };
    } else {
        return result;
    };

    if hi == lo {
        return lo;
    };
    return result - hi * (result / hi).floor();
}

pub fn wrap(input: f64, lo: f64, hi: f64) -> f64 {
    let mut range = 0.;
    let mut input = input;
    // avoid the divide if possible
    if input >= hi {
        range = hi - lo;
        input -= range;
        if input < hi {
            return input;
        }
    } else if input < lo {
        range = hi - lo;
        input += range;
        if input >= lo {
            return input;
        }
    } else {
        return input;
    };

    if hi == lo {
        return lo;
    }
    return input - range * ((input - lo) / range).floor();
}

fn linexp(x: f64, a: f64, b: f64, c: f64, d: f64) -> f64 {
    if x <= a {
        return c;
    };
    if x >= b {
        return d;
    };
    (d / c).powf((x - a) / (b - a)) * c
}

pub fn curvelin(x: f64, in_min: f64, in_max: f64, out_min: f64, out_max: f64, curve: f64) -> f64 {
    if x <= in_min {
        out_max
    } else if x >= in_max {
        out_max
    } else {
        let grow = curve.exp();
        let a = (in_max - in_min) / (1. - grow);
        let b = in_min + a;

        ((b - x) / a).ln() * (out_max - out_min) / curve + out_min
    }
}

pub fn gauss_curve(x: f64) -> f64 {
    let c = 0.5;
    ((x * x) / (-2.0 * (c * c))).exp()
}

pub fn bit_neg(input: f64) -> f64 {
    f64::from_bits(input.to_bits().wrapping_neg()) // / f64::MAX * 2.0 - 1.0
}

pub fn bit_or(input1: f64, input2: f64) -> f64 {
    f64::from_bits(input1.to_bits() | input2.to_bits()) // / f64::MAX * 2.0 - 1.0
}

pub fn bit_xor(input1: f64, input2: f64) -> f64 {
    f64::from_bits(input1.to_bits() ^ input2.to_bits()) // / f64::MAX * 2.0 - 1.0
}

pub fn bit_and(input1: f64, input2: f64) -> f64 {
    f64::from_bits(input1.to_bits() & input2.to_bits()) // / f64::MAX * 2.0 - 1.0
}

// TODO unit tests
