extern crate nom;
extern crate hocon;


#[cfg(test)]
mod tests {

    use nom::types::CompleteStr;
    use hocon::parsers::parse_hocon;

    #[test]
    fn test_akka_config() {
        let source = CompleteStr(include_str!("akka.conf"));

        let res = parse_hocon(source);
        assert!(res.is_ok(), "can parse akka config");
    }
}
