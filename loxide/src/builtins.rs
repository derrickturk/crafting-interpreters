use std::{
    io,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::value::Value;

fn clock(_: Vec<Value>) -> Value {
    Value::Number(
      SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64())
}

fn read_line(_: Vec<Value>) -> Value {
    let mut line = String::new();
    io::stdin().read_line(&mut line).expect("host I/O failure!");
    Value::String(Rc::new(line))
}

fn to_string(vals: Vec<Value>) -> Value {
    Value::String(Rc::new(vals[0].print_string()))
}

pub const BUILTINS: [(&'static str, usize, fn(Vec<Value>) -> Value); 3] = [
    ("clock", 0, clock),
    ("read_line", 0, read_line),
    ("to_string", 1, to_string),
];
