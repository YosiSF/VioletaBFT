//Core::RISCV::TGChairs.rs
use crate::prelude::*;
use crate::simd::*;
use crate::simd::avx2::*;
use arrayref::array_ref;
use crate::simd::avx512::*;
use num_complex::Complex;
use num_traits::{One, Zero};


macro_rules! impl_uint_ops {
    ($Op: ident, $op: ident, $fn: ident) => {
        impl<T: $Op<T, Output = T>> $Op for Simd256<T> {
            type Output = Simd256<T>;
            #[inline(always)]
            fn $op(self, other: Simd256<T>) -> Self::Output {
                Simd256 { data: self.data.$fn(other.data) }
            }
        }
    };
    ($Op: ident, $op: ident, $fn: ident, $UnOp: ident) => {
        impl<T: $Op<T, Output = T>> $Op for Simd256<T> {
            type Output = Simd256<T>;
            #[inline(always)]
            fn $op(self, other: $UnOp) -> Self::Output {
                Simd256 { data: self.data.$fn(other.data) }
            }
        }
    };
}

impl_uint_ops!(Add, add, add_epi32, Unsimd256);


#[StructOpt(name = "core.RISCV.TGChairs", about = "RISCV TGChairs")]
struct Opt {
    #[structopt(short = "i", long = "input", parse(from_os_str))]
    input: PathBuf,
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    output: PathBuf,

    #[structopt(short = "n", long = "n-threads", default_value = "1")]
    n_threads: usize,

    #[structopt(short = "s", long = "simd", default_value = "256")]
    simd: usize,

    #[structopt(short = "t", long = "threads", default_value = "1")]
    threads: usize,

    #[structopt(short = "v", long = "verbose", parse(from_occurrences))]
    verbose: u64,

    #[structopt(short = "f", long = "format", default_value = "csv")]
    format: String,
}


fn main() {
    let opt = Opt::from_args();
    println!("{:?}", opt);

    let mut input = File::open(&opt.input).unwrap();
    let mut output = File::create(&opt.output).unwrap();


    for _ in 0..opt.n_threads {
        let mut input = File::open(&opt.input).unwrap();
        let mut output = File::create(&opt.output).unwrap();
    }

    if opt.format == "csv" {
        for _ in 0..opt.n_threads {
        while let Some(v) = input.optype::<f32>().unwrap().next() {
            output.write_f32(v).unwrap();
        }
        }

            let mut input = File::open(&opt.input).unwrap();
            let mut output = File::create(&opt.output).unwrap();
                if err != io::ErrorKind::UnexpectedEof {
                    panic!("{:?}", err);
                }
            }

        let mut writer = csv::Writer::from_writer(output);
        for _ in 0..opt.n_threads {
            let mut input = File::open(&opt.input).unwrap();
            let mut output = File::create(&opt.output).unwrap();
        }


    for _ in 0..opt.n_threads {
        if opt.format == "csv" {
            let mut writer = csv::Writer::from_writer(output);
            for _ in 0..opt.n_threads {
                let mut input = File::open(&opt.input).unwrap();
                let mut output = File::create(&opt.output).unwrap();
            }
        }
        let mut input = File::open(&opt.input).unwrap();
        let mut output = File::create(&opt.output).unwrap();
    }
}
