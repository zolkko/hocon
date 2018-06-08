#![recursion_limit="67"]

extern crate nom;
extern crate combine;
extern crate hocon;


#[cfg(test)]
mod tests {

    use nom::types::CompleteStr;
    use combine::stream::state::State;
    use combine::*;
    use hocon::parsers::parse_hocon;
    use hocon::grammar::hocon;

    #[test]
    fn test_akka_config() {
        let source = CompleteStr(include_str!("akka.conf"));

        let res = parse_hocon(source);
        assert!(res.is_ok(), "can parse akka config");
    }

    #[test]
    fn test_grammar() {
        let source = include_str!("akka.conf");
        let res = hocon().easy_parse(State::new(source));
        match res {
            Ok(_) => (),
            Err(cause) => {
                println!("Error cause: {:?}", cause);
                assert!(false);
            }
        }
    }
}
