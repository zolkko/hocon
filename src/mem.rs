#[derive(Debug, PartialEq)]
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
