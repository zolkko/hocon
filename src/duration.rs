use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::{space0, u64 as parse_u64};
use nom::combinator::map;
use nom::sequence::separated_pair;
use nom::IResult;
use std::time::Duration;

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

fn nanoseconds(input: &str) -> IResult<&str, DurationUnit> {
    map(
        alt((
            tag_no_case("nanoseconds"),
            tag_no_case("nanosecond"),
            tag_no_case("nanos"),
            tag_no_case("nano"),
            tag_no_case("ns"),
        )),
        |_| DurationUnit::Nanoseconds,
    )(input)
}

fn microseconds(input: &str) -> IResult<&str, DurationUnit> {
    map(
        alt((
            tag_no_case("microseconds"),
            tag_no_case("microsecond"),
            tag_no_case("micros"),
            tag_no_case("micro"),
            tag_no_case("us"),
        )),
        |_| DurationUnit::Microseconds,
    )(input)
}

fn milliseconds(input: &str) -> IResult<&str, DurationUnit> {
    map(
        alt((
            tag_no_case("milliseconds"),
            tag_no_case("millisecond"),
            tag_no_case("millis"),
            tag_no_case("milli"),
            tag_no_case("ms"),
        )),
        |_| DurationUnit::Milliseconds,
    )(input)
}

fn seconds(input: &str) -> IResult<&str, DurationUnit> {
    map(alt((tag_no_case("seconds"), tag_no_case("second"), tag_no_case("s"))), |_| DurationUnit::Seconds)(input)
}

fn minutes(input: &str) -> IResult<&str, DurationUnit> {
    map(alt((tag_no_case("minutes"), tag_no_case("minute"), tag_no_case("mm"))), |_| DurationUnit::Minutes)(input)
}

fn hours(input: &str) -> IResult<&str, DurationUnit> {
    map(alt((tag_no_case("hours"), tag_no_case("hour"), tag_no_case("h"))), |_| DurationUnit::Hours)(input)
}

fn days(input: &str) -> IResult<&str, DurationUnit> {
    map(alt((tag_no_case("days"), tag_no_case("day"), tag_no_case("d"))), |_| DurationUnit::Days)(input)
}

pub(crate) fn parse_duration(input: &str) -> IResult<&str, Duration> {
    let unit_parser = alt((nanoseconds, microseconds, milliseconds, seconds, minutes, hours, days));

    map(separated_pair(parse_u64, space0, unit_parser), |(value, unit)| match unit {
        DurationUnit::Nanoseconds => {
            const NANOS_PER_SEC: u64 = 1_000_000_000;
            if value < NANOS_PER_SEC {
                Duration::new(0, value as u32)
            } else {
                Duration::new(value / NANOS_PER_SEC, (value % NANOS_PER_SEC) as u32)
            }
        }
        DurationUnit::Microseconds => Duration::from_micros(value),
        DurationUnit::Milliseconds => Duration::from_millis(value),
        DurationUnit::Seconds => Duration::new(value, 0),
        DurationUnit::Minutes => Duration::new(value * 60, 0),
        DurationUnit::Hours => Duration::new(value * 60 * 60, 0),
        DurationUnit::Days => Duration::new(value * 60 * 60 * 24, 0),
    })(input)
}

#[cfg(test)]
mod tests {

    use super::*;

    macro_rules! assert_duration (
        ($unit:expr, $v:expr, $expect:expr) => {
            let s = format!("{}{}", $v, $unit);
            assert_eq!(parse_duration(s.as_ref()), Ok(("", $expect)));
            let s = format!("{} {}", $v, $unit);
            assert_eq!(parse_duration(s.as_ref()), Ok(("", $expect)));
            let s = format!("{}  {}", $v, $unit);
            assert_eq!(parse_duration(s.as_ref()), Ok(("", $expect)));
        };
    );

    #[test]
    fn test_duration() {
        assert_duration!("nanoseconds", 100, Duration::new(0, 100));
        assert_duration!("nanosecond", 100, Duration::new(0, 100));
        assert_duration!("nanos", 100, Duration::new(0, 100));
        assert_duration!("nano", 100, Duration::new(0, 100));
        assert_duration!("ns", 100, Duration::new(0, 100));

        assert_duration!("microseconds", 100, Duration::from_micros(100));
        assert_duration!("microsecond", 100, Duration::from_micros(100));
        assert_duration!("micros", 100, Duration::from_micros(100));
        assert_duration!("micro", 100, Duration::from_micros(100));
        assert_duration!("us", 100, Duration::from_micros(100));

        assert_duration!("milliseconds", 100, Duration::from_millis(100));
        assert_duration!("millisecond", 100, Duration::from_millis(100));
        assert_duration!("millis", 100, Duration::from_millis(100));
        assert_duration!("milli", 100, Duration::from_millis(100));
        assert_duration!("ms", 100, Duration::from_millis(100));

        assert!(parse_duration("100m").is_err());
        assert!(parse_duration("100n").is_err());
        assert!(parse_duration("100nanaz").is_err());
    }

    #[test]
    fn test_nanoseconds() {
        assert_eq!(nanoseconds("nanoseconds"), Ok(("", DurationUnit::Nanoseconds)));
        assert_eq!(nanoseconds("nanosecond"), Ok(("", DurationUnit::Nanoseconds)));
        assert_eq!(nanoseconds("nanos"), Ok(("", DurationUnit::Nanoseconds)));
        assert_eq!(nanoseconds("nano"), Ok(("", DurationUnit::Nanoseconds)));
        assert_eq!(nanoseconds("ns"), Ok(("", DurationUnit::Nanoseconds)));
        assert!(nanoseconds("n").is_err());
    }

    #[test]
    fn test_microseconds() {
        assert_eq!(microseconds("microseconds"), Ok(("", DurationUnit::Microseconds)));
        assert_eq!(microseconds("microsecond"), Ok(("", DurationUnit::Microseconds)));
        assert_eq!(microseconds("micros"), Ok(("", DurationUnit::Microseconds)));
        assert_eq!(microseconds("micro"), Ok(("", DurationUnit::Microseconds)));
        assert_eq!(microseconds("us"), Ok(("", DurationUnit::Microseconds)));
        assert!(microseconds("u").is_err());
    }

    #[test]
    fn test_milliseconds() {
        assert_eq!(milliseconds("milliseconds"), Ok(("", DurationUnit::Milliseconds)));
        assert_eq!(milliseconds("millisecond"), Ok(("", DurationUnit::Milliseconds)));
        assert_eq!(milliseconds("millis"), Ok(("", DurationUnit::Milliseconds)));
        assert_eq!(milliseconds("milli"), Ok(("", DurationUnit::Milliseconds)));
        assert_eq!(milliseconds("ms"), Ok(("", DurationUnit::Milliseconds)));
        assert!(milliseconds("m").is_err());
    }

    #[test]
    fn test_seconds() {
        assert_eq!(seconds("seconds"), Ok(("", DurationUnit::Seconds)));
        assert_eq!(seconds("second"), Ok(("", DurationUnit::Seconds)));
        assert_eq!(seconds("s"), Ok(("", DurationUnit::Seconds)));
        assert!(seconds("x").is_err());
    }

    #[test]
    fn test_minutes() {
        assert_eq!(minutes("minutes"), Ok(("", DurationUnit::Minutes)));
        assert_eq!(minutes("minute"), Ok(("", DurationUnit::Minutes)));
        assert_eq!(minutes("mm"), Ok(("", DurationUnit::Minutes)));
        assert!(minutes("x").is_err());
    }

    #[test]
    fn test_hours() {
        assert_eq!(hours("hours"), Ok(("", DurationUnit::Hours)));
        assert_eq!(hours("hour"), Ok(("", DurationUnit::Hours)));
        assert_eq!(hours("h"), Ok(("", DurationUnit::Hours)));
        assert!(hours("x").is_err());
    }

    #[test]
    fn test_days() {
        assert_eq!(days("days"), Ok(("", DurationUnit::Days)));
        assert_eq!(days("day"), Ok(("", DurationUnit::Days)));
        assert_eq!(days("d"), Ok(("", DurationUnit::Days)));
        assert!(days("x").is_err());
    }
}
