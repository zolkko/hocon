#[derive(Debug, PartialEq)]
enum DurationUnit {
  Nanoseconds,
  Microseconds,
  Milliseconds,
  Seconds,
  Minutes,
  Hours,
  Days,
}

named!(
  nanoseconds,
  alt_complete!(tag!(b"ns") | tag!(b"nanoseconds") | tag!(b"nanosecond") | tag!(b"nanos") | tag!(b"nano"))
);

named!(
  microseconds,
  alt_complete!(tag!(b"us") | tag!(b"microseconds") | tag!(b"microsecond") | tag!(b"micros") | tag!(b"micro"))
);

named!(
  milliseconds,
  alt_complete!(tag!(b"ms") | tag!(b"milliseconds") | tag!(b"millisecond") | tag!(b"millis") | tag!(b"milli"))
);

named!(
  seconds,
  alt_complete!(tag!(b"seconds") | tag!(b"second") | tag!(b"s"))
);

named!(
  minutes,
  alt_complete!(tag!(b"minutes") | tag!(b"minute") | tag!(b"mm"))
);

named!(
  hours,
  alt_complete!(tag!(b"hours") | tag!(b"hour") | tag!(b"h"))
);

named!(
  days,
  alt_complete!(tag!(b"days") | tag!(b"day") | tag!(b"d"))
);

named!(
  duration_unit<DurationUnit>,
  alt!(
    nanoseconds => { |_| DurationUnit::Nanoseconds } |
    microseconds => { |_| DurationUnit::Microseconds } |
    milliseconds => { |_| DurationUnit::Milliseconds } |
    seconds => { |_| DurationUnit::Seconds } |
    minutes => { |_| DurationUnit::Minutes } |
    hours => { |_| DurationUnit::Hours } |
    days => { |_| DurationUnit::Days }
  )
);

named!(
  duration<::std::time::Duration>,
  map!(
    separated_pair!(flat_map!(digit, parse_to!(u64)), opt!(space), duration_unit),
    |(v, du): (u64, DurationUnit)| match du {
      DurationUnit::Nanoseconds => {
        if v <= 0xff_ff_ff_ff {
          ::std::time::Duration::new(0, v as u32)
        } else {
          ::std::time::Duration::new(v / 1_000_000_000, (v % 1_000_000_000) as u32)
        }
      }
      DurationUnit::Microseconds => ::std::time::Duration::from_micros(v),
      DurationUnit::Milliseconds => ::std::time::Duration::from_millis(v),
      DurationUnit::Seconds => ::std::time::Duration::new(v, 0),
      DurationUnit::Minutes => ::std::time::Duration::new(v * 60, 0),
      DurationUnit::Hours => ::std::time::Duration::new(v * 60 * 60, 0),
      DurationUnit::Days => ::std::time::Duration::new(v * 60 * 60 * 24, 0),
    }
  )
);

#[cfg(test)]
mod tests {
  macro_rules! assert_duration (
    ($unit:expr, $expect:expr, $v:expr) => {
      assert_eq!(duration_unit($v.as_bytes()), Ok((&b""[..], $unit)));
      assert_eq!(duration(format!("100{}", $v).as_bytes()), Ok((&b""[..], $expect)));
      assert_eq!(duration(format!("100 {}", $v).as_bytes()), Ok((&b""[..], $expect)));
      assert_eq!(duration(format!("100  {}", $v).as_bytes()), Ok((&b""[..], $expect)));
    };
    ($unit:expr, $expect:expr, $v:expr, $($rest:expr),+) => {
      assert_duration!($unit, $expect, $v);
      assert_duration!($unit, $expect, $($rest),+);
    }
  );

  #[test]
  fn duration_should_recognize_nanoseconds() {
    assert_duration!(
      DurationUnit::Nanoseconds,
      ::std::time::Duration::new(0, 100),
      "nanoseconds",
      "nanosecond",
      "nanos",
      "nano",
      "ns"
    );
  }

  #[test]
  fn duration_should_support_big_nanoseconds_values() {
    assert_eq!(
      duration(b"4294967395nanoseconds"),
      Ok((&b""[..], ::std::time::Duration::new(4, 294967395)))
    );
  }

  #[test]
  fn duration_should_recognize_microseconds() {
    assert_duration!(
      DurationUnit::Microseconds,
      ::std::time::Duration::from_micros(100),
      "microseconds",
      "microsecond",
      "micros",
      "micro",
      "us"
    );
  }

  #[test]
  fn duration_should_recognize_milliseconds() {
    assert_duration!(
      DurationUnit::Milliseconds,
      ::std::time::Duration::from_millis(100),
      "milliseconds",
      "millisecond",
      "millis",
      "milli",
      "ms"
    );
  }

  #[test]
  fn duration_should_recognize_seconds() {
    assert_duration!(
      DurationUnit::Seconds,
      ::std::time::Duration::new(100, 0),
      "seconds",
      "second",
      "s"
    );
  }

  #[test]
  fn duration_should_recognize_minutes() {
    assert_duration!(
      DurationUnit::Minutes,
      ::std::time::Duration::new(100 * 60, 0),
      "minutes",
      "minute",
      "mm"
    );
  }

  #[test]
  fn duration_should_recognize_hours() {
    assert_duration!(
      DurationUnit::Hours,
      ::std::time::Duration::new(100 * 60 * 60, 0),
      "hours",
      "hour",
      "h"
    );
  }

  #[test]
  fn duration_should_recognize_days() {
    assert_duration!(
      DurationUnit::Days,
      ::std::time::Duration::new(100 * 60 * 60 * 24, 0),
      "days",
      "day",
      "d"
    );
  }
}