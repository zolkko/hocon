#![feature(duration_from_micros)]
#![feature(slice_patterns)]

#[macro_use]
extern crate nom;

mod config;
pub mod mem;
mod parsers;
