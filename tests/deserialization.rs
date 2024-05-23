use serde_derive::Deserialize;
use std::default::Default;

#[test]
fn deserialize_normal() {
    #[derive(PartialEq, Debug, Deserialize)]
    struct Sample {
        pub value: i32,
    }

    let value = r#"{ value: 123 }"#;
    let value: Sample = hocon::from_str(value).expect("must deserialize hocon value");

    assert_eq!(value, Sample { value: 123 });
}

#[test]
fn deserialize_default_missing_fields() {
    #[derive(PartialEq, Debug, Deserialize)]
    struct Sample {
        #[serde(default = "value_default")]
        pub value: i32,
    }

    fn value_default() -> i32 {
        321
    }

    impl Default for Sample {
        fn default() -> Self {
            Sample { value: i32::default() }
        }
    }

    let value = r#"{}"#;
    let value: Sample = hocon::from_str(value).expect("must deserialize hocon value");

    assert_eq!(value, Sample { value: value_default() });
}

#[test]
fn deserialize_missing_as_none() {
    #[derive(PartialEq, Debug, Deserialize)]
    struct Sample {
        #[serde(default)]
        pub value: Option<i32>,
    }

    let value = r#"{}"#;
    let value: Sample = hocon::from_str(value).expect("must deserialize hocon value");

    assert_eq!(value, Sample { value: None });
}

#[test]
fn can_parse_akka_config() {
    static AKKA_CONFIG: &str = include_str!("resources/akka.conf");
    let res = hocon::parse(AKKA_CONFIG);
    assert!(res.is_ok());
}
