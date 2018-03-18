use super::mem::{MemorySize, MemoryUnit};


named!(
  mem_bytes,
  alt_complete!(tag!(b"bytes") | tag!(b"byte") | tag!(b"b") | tag!(b"B"))
);

named!(
  mem_kilobytes,
  alt_complete!(tag!(b"kilobytes") | tag!(b"kilobyte") | tag!(b"kB"))
);

named!(
  mem_kibibytes,
  alt_complete!(tag!(b"kibibytes") | tag!(b"kibibyte") | tag!(b"KiB") | tag!(b"Ki") | tag!(b"K") | tag!(b"k"))
);

named!(
  mem_megabytes,
  alt_complete!(tag!(b"megabytes") | tag!(b"megabyte") | tag!(b"MB"))
);

named!(
  mem_mebibytes,
  alt_complete!(tag!(b"mebibytes") | tag!(b"mebibyte") | tag!(b"MiB") | tag!(b"Mi") | tag!(b"M") | tag!(b"m"))
);

named!(
  mem_gigabytes,
  alt_complete!(tag!(b"gigabytes") | tag!(b"gigabyte") | tag!(b"GB"))
);

named!(
  mem_gibibytes,
  alt_complete!(tag!(b"gibibytes") | tag!(b"gibibyte") | tag!(b"GiB") | tag!(b"Gi") | tag!(b"G") | tag!(b"g"))
);

named!(
  mem_terabytes,
  alt_complete!(tag!(b"terabytes") | tag!(b"terabyte") | tag!(b"TB"))
);

named!(
  mem_tebibytes,
  alt_complete!(tag!(b"tebibytes") | tag!(b"tebibyte") | tag!(b"TiB") | tag!(b"Ti") | tag!(b"T") | tag!(b"t"))
);

named!(
  mem_petabytes,
  alt_complete!(tag!(b"petabytes") | tag!(b"petabyte") | tag!(b"PB"))
);

named!(
  mem_pebibytes,
  alt_complete!(tag!(b"pebibytes") | tag!(b"pebibyte") | tag!(b"PiB") | tag!(b"Pi") | tag!(b"P") | tag!(b"p"))
);

named!(
  mem_exabytes,
  alt_complete!(tag!(b"exabytes") | tag!(b"exabyte") | tag!(b"EB"))
);

named!(
  mem_exbibytes,
  alt_complete!(tag!(b"exbibytes") | tag!(b"exbibyte") | tag!(b"EiB") | tag!(b"Ei") | tag!(b"E") | tag!(b"e"))
);

named!(
  mem_zettabytes,
  alt_complete!(tag!(b"zettabytes") | tag!(b"zettabyte") | tag!(b"ZB"))
);

named!(
  mem_zebibytes,
  alt_complete!(tag!(b"zebibytes") | tag!(b"zebibyte") | tag!(b"ZiB") | tag!(b"Zi") | tag!(b"Z") | tag!(b"z"))
);

named!(
  mem_yottabytes,
  alt_complete!(tag!(b"yottabytes") | tag!(b"yottabyte") | tag!(b"YB"))
);

named!(
  mem_yobibytes,
  alt_complete!(tag!(b"yobibytes") | tag!(b"yobibyte") | tag!(b"YiB") | tag!(b"Yi") | tag!(b"Y") | tag!(b"y"))
);

named!(
  memory_unit<MemoryUnit>,
  alt!(
    mem_bytes => { |_| MemoryUnit::Bytes } |
    mem_kilobytes => { |_| MemoryUnit::Kilobytes } |
    mem_kibibytes => { |_| MemoryUnit::Kibibytes } |
    mem_megabytes => { |_| MemoryUnit::Megabytes } |
    mem_mebibytes => { |_| MemoryUnit::Mebibytes } |
    mem_gigabytes => { |_| MemoryUnit::Gigabytes } |
    mem_gibibytes => { |_| MemoryUnit::Gibibytes } |
    mem_terabytes => { |_| MemoryUnit::Terabytes } |
    mem_tebibytes => { |_| MemoryUnit::Tebibytes } |
    mem_petabytes => { |_| MemoryUnit::Petabytes } |
    mem_pebibytes => { |_| MemoryUnit::Pebibytes } |
    mem_exabytes => { |_| MemoryUnit::Exabytes } |
    mem_exbibytes => { |_| MemoryUnit::Exbibytes } |
    mem_zettabytes => { |_| MemoryUnit::Zettabytes } |
    mem_zebibytes => { |_| MemoryUnit::Zebibytes } |
    mem_yottabytes => { |_| MemoryUnit::Yottabytes } |
    mem_yobibytes => { |_| MemoryUnit::Yobibytes }
  )
);

named!(
  memory_size<MemorySize>,
  map!(
    separated_pair!(flat_map!(digit, parse_to!(u32)), opt!(space), memory_unit),
    |(v, mu): (u32, MemoryUnit)| MemorySize::new(v, mu)
  )
);

#[cfg(test)]
mod tests {

  use mem::{MemorySize, MemoryUnit};
  use super::*;

  macro_rules! assert_memory_size (
    ($mem_unit:expr, $value:expr, $($rest:expr),+) => {
      assert_memory_size!($mem_unit, $value);
      assert_memory_size!($mem_unit, $($rest),+);
    };
    ($mem_unit:expr, $value:expr) => {
      assert_eq!(memory_size(format!("123{}", $value).as_bytes()), Ok((&b""[..], MemorySize::new(123, $mem_unit))));
      assert_eq!(memory_size(format!("321 {}", $value).as_bytes()), Ok((&b""[..], MemorySize::new(321, $mem_unit))));
      assert_eq!(memory_size(format!("213  {}", $value).as_bytes()), Ok((&b""[..], MemorySize::new(213, $mem_unit))));
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
    assert_memory_size!(
      MemoryUnit::Kibibytes,
      "kibibytes", "kibibyte", "KiB", "Ki", "K", "k"
    );
  }

  #[test]
  fn memory_size_should_recognize_megabytes() {
    assert_memory_size!(MemoryUnit::Megabytes, "megabytes", "megabyte", "MB");
  }

  #[test]
  fn memory_size_should_recognize_mebibytes() {
    assert_memory_size!(
      MemoryUnit::Mebibytes,
      "mebibytes", "mebibyte", "MiB", "Mi", "M", "m"
    );
  }

  #[test]
  fn memory_size_should_recognize_gigabytes() {
    assert_memory_size!(MemoryUnit::Gigabytes, "gigabytes", "gigabyte", "GB");
  }

  #[test]
  fn memory_size_should_recognize_gibibytes() {
    assert_memory_size!(
      MemoryUnit::Gibibytes,
      "gibibytes",
      "gibibyte",
      "GiB",
      "Gi",
      "G",
      "g"
    );
  }

  #[test]
  fn memory_size_should_recognize_terabytes() {
    assert_memory_size!(MemoryUnit::Terabytes, "terabytes", "terabyte", "TB");
  }

  #[test]
  fn memory_size_should_recognize_tebibytes() {
    assert_memory_size!(
      MemoryUnit::Tebibytes,
      "tebibytes",
      "tebibyte",
      "TiB",
      "Ti",
      "T",
      "t"
    );
  }

  #[test]
  fn memory_size_should_recognize_petabytes() {
    assert_memory_size!(MemoryUnit::Petabytes, "petabytes", "petabyte", "PB");
  }

  #[test]
  fn memory_size_should_recognize_pebibytes() {
    assert_memory_size!(
      MemoryUnit::Pebibytes,
      "pebibytes",
      "pebibyte",
      "PiB",
      "Pi",
      "P",
      "p"
    );
  }

  #[test]
  fn memory_size_should_recognize_exabytes() {
    assert_memory_size!(MemoryUnit::Exabytes, "exabytes", "exabyte", "EB");
  }

  #[test]
  fn memory_size_should_recognize_exbibytes() {
    assert_memory_size!(
      MemoryUnit::Exbibytes,
      "exbibytes",
      "exbibyte",
      "EiB",
      "Ei",
      "E",
      "e"
    );
  }

  #[test]
  fn memory_size_should_recognize_zettabytes() {
    assert_memory_size!(MemoryUnit::Zettabytes, "zettabytes", "zettabyte", "ZB");
  }

  #[test]
  fn memory_size_should_recognize_zebibytes() {
    assert_memory_size!(
      MemoryUnit::Zebibytes,
      "zebibytes",
      "zebibyte",
      "ZiB",
      "Zi",
      "Z",
      "z"
    );
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
