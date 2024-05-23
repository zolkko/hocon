#![allow(unused)]
use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::{space0, u64 as parse_u64};
use nom::combinator::{map, value};
use nom::sequence::separated_pair;
use nom::IResult;

#[derive(Debug, PartialEq, Clone)]
pub enum MemoryUnit {
    Bytes,
    Kilobytes,
    Kibibytes,
    Megabytes,
    Mebibytes,
    Gigabytes,
    Gibibytes,
    Terabytes,
    Tebibytes,
    Petabytes,
    Pebibytes,
    Exabytes,
    Exbibytes,
    Zettabytes,
    Zebibytes,
    Yottabytes,
    Yobibytes,
}

#[derive(Debug, PartialEq)]
pub struct MemorySize {
    size: u32,
    unit: MemoryUnit,
}

impl MemorySize {
    pub fn new(size: u32, unit: MemoryUnit) -> Self {
        MemorySize { size, unit }
    }
}

fn mem_bytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(MemoryUnit::Bytes, alt((tag_no_case("bytes"), tag_no_case("byte"), tag_no_case("b"))))(input)
}

fn mem_kilobytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(MemoryUnit::Kilobytes, alt((tag_no_case("kilobytes"), tag_no_case("kilobyte"), tag_no_case("kB"))))(input)
}

fn mem_kibibytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(
        MemoryUnit::Kibibytes,
        alt((tag_no_case("kibibytes"), tag_no_case("kibibyte"), tag_no_case("KiB"), tag_no_case("Ki"), tag_no_case("k"))),
    )(input)
}

fn mem_megabytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(MemoryUnit::Megabytes, alt((tag_no_case("megabytes"), tag_no_case("megabyte"), tag_no_case("MB"))))(input)
}

fn mem_mebibytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(
        MemoryUnit::Mebibytes,
        alt((
            tag_no_case("mebibytes"),
            tag_no_case("mebibyte"),
            tag_no_case("MiB"),
            tag_no_case("Mi"),
            tag_no_case("M"),
            tag_no_case("m"),
        )),
    )(input)
}

fn mem_gigabytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(MemoryUnit::Gigabytes, alt((tag_no_case("gigabytes"), tag_no_case("gigabyte"), tag_no_case("GB"))))(input)
}

fn mem_gibibytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(
        MemoryUnit::Gibibytes,
        alt((
            tag_no_case("gibibytes"),
            tag_no_case("gibibyte"),
            tag_no_case("GiB"),
            tag_no_case("Gi"),
            tag_no_case("G"),
            tag_no_case("g"),
        )),
    )(input)
}

fn mem_terabytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(MemoryUnit::Terabytes, alt((tag_no_case("terabytes"), tag_no_case("terabyte"), tag_no_case("TB"))))(input)
}

fn mem_tebibytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(
        MemoryUnit::Tebibytes,
        alt((
            tag_no_case("tebibytes"),
            tag_no_case("tebibyte"),
            tag_no_case("TiB"),
            tag_no_case("Ti"),
            tag_no_case("T"),
            tag_no_case("t"),
        )),
    )(input)
}

fn mem_petabytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(MemoryUnit::Petabytes, alt((tag_no_case("petabytes"), tag_no_case("petabyte"), tag_no_case("PB"))))(input)
}

fn mem_pebibytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(
        MemoryUnit::Pebibytes,
        alt((
            tag_no_case("pebibytes"),
            tag_no_case("pebibyte"),
            tag_no_case("PiB"),
            tag_no_case("Pi"),
            tag_no_case("P"),
            tag_no_case("p"),
        )),
    )(input)
}

fn mem_exabytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(MemoryUnit::Exabytes, alt((tag_no_case("exabytes"), tag_no_case("exabyte"), tag_no_case("EB"))))(input)
}

fn mem_exbibytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(
        MemoryUnit::Exbibytes,
        alt((
            tag_no_case("exbibytes"),
            tag_no_case("exbibyte"),
            tag_no_case("EiB"),
            tag_no_case("Ei"),
            tag_no_case("E"),
            tag_no_case("e"),
        )),
    )(input)
}

fn mem_zettabytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(MemoryUnit::Zettabytes, alt((tag_no_case("zettabytes"), tag_no_case("zettabyte"), tag_no_case("ZB"))))(input)
}

fn mem_zebibytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(
        MemoryUnit::Zebibytes,
        alt((
            tag_no_case("zebibytes"),
            tag_no_case("zebibyte"),
            tag_no_case("ZiB"),
            tag_no_case("Zi"),
            tag_no_case("Z"),
            tag_no_case("z"),
        )),
    )(input)
}

fn mem_yottabytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(MemoryUnit::Yottabytes, alt((tag_no_case("yottabytes"), tag_no_case("yottabyte"), tag_no_case("YB"))))(input)
}

fn mem_yobibytes(input: &str) -> IResult<&str, MemoryUnit> {
    value(
        MemoryUnit::Yobibytes,
        alt((
            tag_no_case("yobibytes"),
            tag_no_case("yobibyte"),
            tag_no_case("YiB"),
            tag_no_case("Yi"),
            tag_no_case("Y"),
            tag_no_case("y"),
        )),
    )(input)
}

fn memory_unit(input: &str) -> IResult<&str, MemoryUnit> {
    alt((
        mem_bytes,
        mem_kilobytes,
        mem_kibibytes,
        mem_megabytes,
        mem_mebibytes,
        mem_gigabytes,
        mem_gibibytes,
        mem_terabytes,
        mem_tebibytes,
        mem_petabytes,
        mem_pebibytes,
        mem_exabytes,
        mem_exbibytes,
        mem_zettabytes,
        mem_zebibytes,
        mem_yottabytes,
        mem_yobibytes,
    ))(input)
}

fn memory_size(input: &str) -> IResult<&str, MemorySize> {
    map(separated_pair(parse_u64, space0, memory_unit), |(v, mu)| MemorySize::new(v as u32, mu))(input)
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::mem::{MemorySize, MemoryUnit};

    macro_rules! assert_memory_size (
    ($mem_unit:expr, $value:expr, $($rest:expr),+) => {
      assert_memory_size!($mem_unit, $value);
      assert_memory_size!($mem_unit, $($rest),+);
    };
    ($mem_unit:expr, $value:expr) => {
      assert_eq!(memory_size(format!("123{}", $value).as_str()), Ok(("", MemorySize::new(123, $mem_unit))));
      assert_eq!(memory_size(format!("321 {}", $value).as_str()), Ok(("", MemorySize::new(321, $mem_unit))));
      assert_eq!(memory_size(format!("213  {}", $value).as_str()), Ok(("", MemorySize::new(213, $mem_unit))));
    }
  );

    #[test]
    fn memory_size_should_recognize_bytes() {
        assert_memory_size!(MemoryUnit::Bytes, "B", "b", "byte", "bytes");
    }

    #[test]
    fn memory_size_should_recognize_kilobytes() {
        assert_memory_size!(MemoryUnit::Kilobytes, "kilobytes", "kilobyte", "kB");
    }

    #[test]
    fn memory_size_should_recognize_kibibytes() {
        assert_memory_size!(MemoryUnit::Kibibytes, "kibibytes", "kibibyte", "KiB", "Ki", "K", "k");
    }

    #[test]
    fn memory_size_should_recognize_megabytes() {
        assert_memory_size!(MemoryUnit::Megabytes, "megabytes", "megabyte", "MB");
    }

    #[test]
    fn memory_size_should_recognize_mebibytes() {
        assert_memory_size!(MemoryUnit::Mebibytes, "mebibytes", "mebibyte", "MiB", "Mi", "M", "m");
    }

    #[test]
    fn memory_size_should_recognize_gigabytes() {
        assert_memory_size!(MemoryUnit::Gigabytes, "gigabytes", "gigabyte", "GB");
    }

    #[test]
    fn memory_size_should_recognize_gibibytes() {
        assert_memory_size!(MemoryUnit::Gibibytes, "gibibytes", "gibibyte", "GiB", "Gi", "G", "g");
    }

    #[test]
    fn memory_size_should_recognize_terabytes() {
        assert_memory_size!(MemoryUnit::Terabytes, "terabytes", "terabyte", "TB");
    }

    #[test]
    fn memory_size_should_recognize_tebibytes() {
        assert_memory_size!(MemoryUnit::Tebibytes, "tebibytes", "tebibyte", "TiB", "Ti", "T", "t");
    }

    #[test]
    fn memory_size_should_recognize_petabytes() {
        assert_memory_size!(MemoryUnit::Petabytes, "petabytes", "petabyte", "PB");
    }

    #[test]
    fn memory_size_should_recognize_pebibytes() {
        assert_memory_size!(MemoryUnit::Pebibytes, "pebibytes", "pebibyte", "PiB", "Pi", "P", "p");
    }

    #[test]
    fn memory_size_should_recognize_exabytes() {
        assert_memory_size!(MemoryUnit::Exabytes, "exabytes", "exabyte", "EB");
    }

    #[test]
    fn memory_size_should_recognize_exbibytes() {
        assert_memory_size!(MemoryUnit::Exbibytes, "exbibytes", "exbibyte", "EiB", "Ei", "E", "e");
    }

    #[test]
    fn memory_size_should_recognize_zettabytes() {
        assert_memory_size!(MemoryUnit::Zettabytes, "zettabytes", "zettabyte", "ZB");
    }

    #[test]
    fn memory_size_should_recognize_zebibytes() {
        assert_memory_size!(MemoryUnit::Zebibytes, "zebibytes", "zebibyte", "ZiB", "Zi", "Z", "z");
    }

    #[test]
    fn memory_size_should_recognize_yottabytes() {
        assert_memory_size!(MemoryUnit::Yottabytes, "yottabytes", "yottabyte", "YB");
    }

    #[test]
    fn memory_size_should_recognize_yobibytes() {
        assert_memory_size!(MemoryUnit::Yobibytes, "yobibytes", "yobibyte", "YiB", "Yi", "Y", "y");
    }
}
