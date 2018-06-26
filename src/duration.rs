use std::time::Duration;

use combine::char::{string, digit};
use combine::error::{Consumed, ParseError, StreamError};
use combine::stream::{StreamErrorFor, Stream};
use combine::*;


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


fn nanoseconds<I>() -> impl Parser<Input = I, Output = DurationUnit>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        try(string("nanoseconds")),
        try(string("nanosecond")),
        try(string("nanos")),
        try(string("nano")),
        try(string("ns")),
    )).map(|_| DurationUnit::Nanoseconds)
}


fn microseconds<I>() -> impl Parser<Input = I, Output = DurationUnit>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        try(string("microseconds")),
        try(string("microsecond")),
        try(string("micros")),
        try(string("micro")),
        try(string("us")),
    )).map(|_| DurationUnit::Microseconds)
}


fn milliseconds<I>() -> impl Parser<Input = I, Output = DurationUnit>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        try(string("milliseconds")),
        try(string("millisecond")),
        try(string("millis")),
        try(string("milli")),
        try(string("ms")),
    )).map(|_| DurationUnit::Milliseconds)
}


fn seconds<I>() -> impl Parser<Input = I, Output = DurationUnit>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        try(string("seconds")),
        try(string("second")),
        try(string("s")),
    )).map(|_| DurationUnit::Seconds)
}


fn minutes<I>() -> impl Parser<Input = I, Output = DurationUnit>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        try(string("minutes")),
        try(string("minute")),
        try(string("mm")),
    )).map(|_| DurationUnit::Minutes)
}


fn hours<I>() -> impl Parser<Input = I, Output = DurationUnit>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        try(string("hours")),
        try(string("hour")),
        try(string("h")),
    )).map(|_| DurationUnit::Hours)
}


fn days<I>() -> impl Parser<Input = I, Output = DurationUnit>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        try(string("days")),
        try(string("day")),
        try(string("d")),
    )).map(|_| DurationUnit::Days)
}


pub(crate) fn duration<I>() -> impl Parser<Input = I, Output = Duration>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1::<String, _>(digit()).and_then(|s| s.parse::<u64>().map_err(StreamErrorFor::<I>::other))
        .skip(skip_many(satisfy(|chr| chr == ' ' || chr == '\t')))
        .and(choice((
            nanoseconds(),
            microseconds(),
            milliseconds(),
            seconds(),
            minutes(),
            hours(),
            days(),
        )))
        .skip(eof())
        .map(|(value, unit)| {
            match unit {
                DurationUnit::Nanoseconds => {
                    if value <= 0xff_ff_ff_ff {
                        Duration::new(0, value as u32)
                    } else {
                        Duration::new(value / 1_000_000_000, (value % 1_000_000_000) as u32)
                    }
                }
                DurationUnit::Microseconds => Duration::from_micros(value),
                DurationUnit::Milliseconds => Duration::from_millis(value),
                DurationUnit::Seconds => Duration::new(value, 0),
                DurationUnit::Minutes => Duration::new(value * 60, 0),
                DurationUnit::Hours => Duration::new(value * 60 * 60, 0),
                DurationUnit::Days => Duration::new(value * 60 * 60 * 24, 0),
            }
        })
}

#[cfg(test)]
mod tests {

    use super::*;

    macro_rules! assert_duration (
        ($unit:expr, $v:expr, $expect:expr) => {
            let s = format!("{}{}", $v, $unit);
            assert_eq!(duration().parse(s.as_ref()), Ok(($expect, "")));
            let s = format!("{} {}", $v, $unit);
            assert_eq!(duration().parse(s.as_ref()), Ok(($expect, "")));
            let s = format!("{}  {}", $v, $unit);
            assert_eq!(duration().parse(s.as_ref()), Ok(($expect, "")));
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

        assert!(duration().parse("100m").is_err());
        assert!(duration().parse("100n").is_err());
        assert!(duration().parse("100nanosecondsx").is_err());
    }

    #[test]
    fn test_nanoseconds() {
        assert_eq!(nanoseconds().parse("nanoseconds"), Ok((DurationUnit::Nanoseconds, "")));
        assert_eq!(nanoseconds().parse("nanosecond"), Ok((DurationUnit::Nanoseconds, "")));
        assert_eq!(nanoseconds().parse("nanos"), Ok((DurationUnit::Nanoseconds, "")));
        assert_eq!(nanoseconds().parse("nano"), Ok((DurationUnit::Nanoseconds, "")));
        assert_eq!(nanoseconds().parse("ns"), Ok((DurationUnit::Nanoseconds, "")));
        assert!(nanoseconds().parse("n").is_err());
    }

    #[test]
    fn test_microseconds() {
        assert_eq!(microseconds().parse("microseconds"), Ok((DurationUnit::Microseconds, "")));
        assert_eq!(microseconds().parse("microsecond"), Ok((DurationUnit::Microseconds, "")));
        assert_eq!(microseconds().parse("micros"), Ok((DurationUnit::Microseconds, "")));
        assert_eq!(microseconds().parse("micro"), Ok((DurationUnit::Microseconds, "")));
        assert_eq!(microseconds().parse("us"), Ok((DurationUnit::Microseconds, "")));
        assert!(microseconds().parse("u").is_err());
    }

    #[test]
    fn test_milliseconds() {
        assert_eq!(milliseconds().parse("milliseconds"), Ok((DurationUnit::Milliseconds, "")));
        assert_eq!(milliseconds().parse("millisecond"), Ok((DurationUnit::Milliseconds, "")));
        assert_eq!(milliseconds().parse("millis"), Ok((DurationUnit::Milliseconds, "")));
        assert_eq!(milliseconds().parse("milli"), Ok((DurationUnit::Milliseconds, "")));
        assert_eq!(milliseconds().parse("ms"), Ok((DurationUnit::Milliseconds, "")));
        assert!(milliseconds().parse("m").is_err());
    }

    #[test]
    fn test_seconds() {
        assert_eq!(seconds().parse("seconds"), Ok((DurationUnit::Seconds, "")));
        assert_eq!(seconds().parse("second"), Ok((DurationUnit::Seconds, "")));
        assert_eq!(seconds().parse("s"), Ok((DurationUnit::Seconds, "")));
        assert!(seconds().parse("x").is_err());
    }

    #[test]
    fn test_minutes() {
        assert_eq!(minutes().parse("minutes"), Ok((DurationUnit::Minutes, "")));
        assert_eq!(minutes().parse("minute"), Ok((DurationUnit::Minutes, "")));
        assert_eq!(minutes().parse("mm"), Ok((DurationUnit::Minutes, "")));
        assert!(minutes().parse("x").is_err());
    }

    #[test]
    fn test_hours() {
        assert_eq!(hours().parse("hours"), Ok((DurationUnit::Hours, "")));
        assert_eq!(hours().parse("hour"), Ok((DurationUnit::Hours, "")));
        assert_eq!(hours().parse("h"), Ok((DurationUnit::Hours, "")));
        assert!(hours().parse("x").is_err());
    }

    #[test]
    fn test_days() {
        assert_eq!(days().parse("days"), Ok((DurationUnit::Days, "")));
        assert_eq!(days().parse("day"), Ok((DurationUnit::Days, "")));
        assert_eq!(days().parse("d"), Ok((DurationUnit::Days, "")));
        assert!(days().parse("x").is_err());
    }
}
