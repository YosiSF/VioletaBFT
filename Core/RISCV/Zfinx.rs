//Core::RISCV::Zfinx.rs
use crate::prelude::*;
use crate::simd::*;
use crate::simd::avx2::*;
use arrayref::array_ref;
use crate::simd::avx512::*;
use num_complex::Complex;
use num_traits::{One, Zero};
use std::fmt;
use std::ops::{Add, Sub, Mul, Div};
use std::path::Path;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Instant;
use std::vec::Vec;
use std::collections::HashMap;
use std::sync::mpsc::{channel, Sender, Receiver};
use std::sync::mpsc::{sync_channel, SyncSender, SyncReceiver};
use std::sync::mpsc::{SendError, RecvError};
use std::sync::mpsc::{SendTimeoutError, RecvTimeoutError};
use std::sync::mpsc::{TrySendError, TryRecvError};
use std::sync::mpsc::{TrySendTimeoutError, TryRecvTimeoutError};
//json
use serde_json::{Value, Error};
use serde_json::{from_str, to_string};
use serde_json::{from_reader, to_writer};
use serde_json::{from_slice, to_vec};


//use std::sync::mpsc::{channel, Sender, Receiver};
//use std::sync::mpsc::{sync_channel, SyncSender, SyncReceiver};
//use std::sync::mpsc::{SendError, RecvError};
//use std::sync::mpsc::{SendTimeoutError, RecvTimeoutError};
//use std::sync::mpsc::{TrySendError, TryRecvError};



#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Zfinx {
    pub data: [f32; 4],

    pub z: f32,

    pub x: f32,

    pub y: f32,


    pub xx: f32,

    pub yy: f32,

    pub zz: f32,


    pub xy: f32,

    pub xz: f32,

    pub yz: f32,

    pub xw: f32,

    pub yw: f32,

    pub zw: f32,

    pub ww: f32,

    pub wx: f32,

    pub wy: f32,

    pub wz: f32,

    pub wxw: f32,

    pub wyw: f32,

    pub wzw: f32,

    pub www: f32,

    pub xxx: f32,

    pub yyy: f32,

    pub zzz: f32,


}




// RiscV is a RISCV processor, we want to adapt it to a generic architecture. Without sacrificing performance, we can use the same architecture for different processors.
//
// The RiscV architecture is a 32-bit RISC architecture with a 32-bit instruction set.
// We can use the same architecture for different processors, by adapting it to a generic architecture. We can hash the architecture to a 32-bit integer, and use this integer to index into a table of architectures.
// We then use the architecture to index into a table of functions, parsing the tokens, and then calling the function with lambda calculus.
//
// The architecture is a 32-bit integer, with the following bits:
//
// 0-1: architecture version
// 2-3: architecture variant
// 4-7: architecture extension
// 8-15: architecture implementation
// 16-31: architecture implementation
// 32-29: architecture variant
// 30-31: architecture extension
//
// The architecture version is the major version of the architecture.



////////////////////////////////
// Sections
////////////////////////////////




pub const ZFINX_VERSION: u32 = 0;


pub const ZFINX_VARIANT: u32 = 0;


pub const ZFINX_EXTENSION: u32 = 0;


pub const ZFINX_IMPLEMENTATION: u32 = 0;


#[derive(Debug)]
pub enum RiscVError {
    Io(std::io::Error),
    Json(serde_json::Error),
    Parse(std::num::ParseIntError),
    Other(String),
}

impl From<std::io::Error> for RiscVError {
    fn from(err: std::io::Error) -> Self {
        RiscVError::Io(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::Io(err)
    }
}

impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Self {
        Error::Json(err)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(err: std::num::ParseIntError) -> Self {
        Error::Parse(err)
    }
}

impl From<std::string::FromUtf8Error> for Error {
    fn from(err: std::string::FromUtf8Error) -> Self {
        Error::Other(err.to_string())
    }
}

//Zfinx on Rissc-V (RISCV) architecture
//ISA
//https://www.riscv.org/documents/riscv-isa-spec/


//Zfinx on Rissc-V (RISCV) architecture



#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Json(serde_json::Error),
    Parse(std::num::ParseIntError),
    Other(String),
}





////////////////////////////////
// Functions
////////////////////////////////

#[inline(always)]
pub fn read_file(path: &str) -> Result<String, Error> {
    let file = File::open(path)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    Ok(contents)
}


#[inline(always)]
pub fn write_file(path: &str, contents: &str) -> Result<(), Error> {
    let mut file = File::create(path)?;
    file.write_all(contents.as_bytes())?;
    Ok(())
}


#[inline(always)]
pub fn read_json(path: &str) -> Result<Value, Error> {
    let contents = read_file(path)?;
    let value = from_str(&contents)?;
    Ok(value)
}


#[inline(always)]
pub fn write_json(path: &str, value: &Value) -> Result<(), Error> {
    let contents = to_string(value)?;
    write_file(path, &contents)?;
    Ok(())
}


#[inline(always)]
pub fn read_json_from_reader(reader: &mut BufReader<File>) -> Result<Value, Error> {
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    let value = from_str(&contents)?;
    Ok(value)
}

////////////////////////////////
// Main
////////////////////////////////

#[inline(always)]
pub fn main() {


    let mut args = env::args();
    let _ = args.next();
    let path = args.next().unwrap_or("zfinx.json".to_string());

    let value = read_json(&path).unwrap();
    println!("{:?}", value);

}


////////////////////////////////
// Tests
////////////////////////////////


#[cfg(test)]
mod tests {

    use super::*;
    use std::io::
    use std::io::Write;
    use std::fs::File;
    use std::path::Path;
    use std::io::BufReader;
    use std::io::BufWriter;
    use std::io::Write;
    use std::io::Read;
    use std::io::Seek;
    use std::path::PathBuf;
    use std::fs::OpenOptions;
    use std::fs::File;
    use std::io::Write;


    #[test]
    fn test_read_json() {
    let path = "zfinx.json";
        let value = read_json(&path).unwrap();
        println!("{:?}", value);

    for i in 0..10 {
        println!("{}", i);

    }
    }

    #[test]
    fn test_write_json() {
        let value = json!({
            "hello": "world"
        });
        let path = "zfinx.json";
        write_json(&path, &value).unwrap();

  let  value = read_json(&path).unwrap();
        println!("{:?}", value);
    }

    #[test]
    fn test_read_yaml() {
    let path = "zfinx.yaml";
        let value = read_json(&path).unwrap();
        println!("{:?}", value);
    }
}