#![feature(duration_from_micros)]
#![feature(slice_patterns)]
#![recursion_limit="67"]

extern crate combine;
extern crate failure;


#[macro_use]
pub mod macros;
pub mod config;
pub mod config_factory;
pub mod grammar;
