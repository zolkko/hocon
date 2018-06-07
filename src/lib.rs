#![feature(duration_from_micros)]
#![feature(slice_patterns)]
#![recursion_limit="1024"]

#[macro_use]
extern crate nom;

extern crate combine;

#[macro_use]
extern crate failure;


#[macro_use]
pub mod macros;
pub mod config;
pub mod mem;
pub mod parsers;
pub mod config_factory;

mod grammar;
