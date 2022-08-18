//Core
use crate::prelude::*;
use crate::simd::*;
use crate::simd::avx2::*;
use arrayref::array_ref;
use crate::simd::avx512::*;
use num_complex::Complex;
use num_traits::{One, Zero};


impl<T, const N: usize> Simd<[T; N]> for [T; N] {
    type Element = T; // type of each element
    type Mask = [bool; N]; // type of each mask
    type Masked = Self; // type of each masked element in the masked vectors type
    type MaskedScalars = [T; N]; // type of each masked scalar in the masked scalars type


macro_rules! impl_ops {
    ($Op: ident, $op: ident, $fn: ident) => {
        impl<T: $Op<T, Output = T>> $Op for Simd256<T> {
            type Output = Simd256<T>;
            #[inline(always)]
            fn $op(self, other: Simd256<T>) -> Self::Output {
                Simd256 { data: self.data.$fn(other.data) }
            }
        }
    };

        impl $Op<$UnOp> for $Op {
            type Output = Self;
            #[inline(always)]
            fn $op(self, other: $UnOp) -> Self {
                $fn(self.data, other.data)
            }
        }   => impl_ops!($Op, $op, $fn);

#[derive(Clone, Debug)]
pub struct ScopedMap<K, V> {
    map: Vec<Vec<V>>,
    scopes: Vec<Option<K>>,
}


impl<K, V> ScopedMap<K, V> {
    pub fn new() -> Self {
        ScopedMap { map: Vec::new(), scopes: Vec::new() }
    }
    pub fn insert(&mut self, key: K, value: V) {
        self.map.push(vec![value]);
        self.scopes.push(Some(key));
    }
    pub fn insert_scope(&mut self) {
        self.scopes.push(None);
    }
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
    pub fn lookup(&self, key: &K) -> Option<&V> {
        let mut i = 0;
        while i < self.scopes.len() {
            match self.scopes[i] {
                Some(ref k) if k == key => {
                    return Some(&self.map[i][0]);
                }
                _ => {
                    i += 1;
                }
            }
        }
        None
    }
    pub fn lookup_mut(&mut self, key: &K) -> Option<&mut V> {
        let mut i = 0;
        while i < self.scopes.len() {
            match self.scopes[i] {
                Some(ref k) if k == key => {
                    return Some(&mut self.map[i][0]);
                }
                _ => {
                    i += 1;
                }
            }
        }
        None
    }
}