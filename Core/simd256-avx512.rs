//Core
use crate::prelude::*;
use crate::simd::*;
use arrayref::array_ref;
use crate::simd::avx2::*;
use num_complex::Complex;
use num_traits::{One, Zero};


impl<T, const N: usize> Simd<[T; N]> for [T; N] {
    type Element = T; // type of each element
    type Mask = [bool; N]; // type of each mask
    type Masked = Self; // type of each masked element

    #[inline(always)]
    fn splat(x: T) -> Self {
        [x; N]
    }

    #[inline(always)]
    fn extract(self, i: usize) -> T {
        self[i]
    }

    #[inline(always)]
    fn replace(&mut self, i: usize, x: T) {
        self[i] = x;
    }

    #[inline(always)]
    fn select(self, mask: Self::Mask, other: Self) -> Self {
        let mut result = self;
        for i in 0..N {
            if mask[i] {
                result[i] = other[i];
            }
        }
        result
    }

    #[inline(always)]
    fn compare(self, other: Self) -> Self {
        let mut result = [false; N];
        for i in 0..N {
            result[i] = self[i] == other[i];
        }
        result
    }
}


#[derive(Clone, Copy)]
pub struct Simd256<T> {
    pub data: [T; 32],
}

impl<T> Simd256<T> {
    pub fn new(data: [T; 32]) -> Self {
        Simd256 { data }
    }
}


#[derive(Clone, Copy)]
pub struct Simd256Mask<T> {
    pub data: [bool; 32],
}

impl<T> Simd256Mask<T> {
    pub fn new(data: [bool; 32]) -> Self {
        Simd256Mask { data }
    }
}