use hocon::HoconParser;


static AKKA_CONF: &'static str = include_str!("resources/akka.conf");


#[test]
fn can_parse_akka_config() {
    let parser = HoconParser::new();
    let result = parser.parse_str(AKKA_CONF).expect("cannot parse akka config");
}
