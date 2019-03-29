use std::default::Default;
use serde_derive::Deserialize;
use hocon::HoconParser;


#[test]
fn deserialize_normal() {
    #[derive(PartialEq, Debug, Deserialize)]
    struct Sample {
        pub value: i32
    }

    let parser = HoconParser::new();
    let value = parser.parse_str(r#"{
        value: 123
    }"#).expect("cannot parse akka config");
    let value: Sample = hocon::de::from_value(value).expect("must deserialize hocon value");

    assert_eq!(value, Sample { value: 123 });
}

#[test]
fn deserialize_default_missing_fields() {

    #[derive(PartialEq, Debug, Deserialize)]
    struct Sample {
        #[serde(default = "value_default")]
        pub value: i32
    }

    fn value_default() -> i32 {
        321
    }

    impl Default for Sample {
        fn default() -> Self {
            Sample { value: i32::default() }
        }
    }

    let parser = HoconParser::new();
    let value = parser.parse_str(r#"{}"#).expect("cannot parse akka config");
    let value: Sample = hocon::de::from_value(value).expect("must deserialize hocon value");

    assert_eq!(value, Sample { value: value_default() });
}

#[test]
fn deserialize_missing_as_none() {
    #[derive(PartialEq, Debug, Deserialize)]
    struct Sample {
        #[serde(default)]
        pub value: Option<i32>
    }

    let parser = HoconParser::new();
    let value = parser.parse_str(r#"{}"#).expect("cannot parse akka config");
    let value: Sample = hocon::de::from_value(value).expect("must deserialize hocon value");

    assert_eq!(value, Sample { value: None });
}
