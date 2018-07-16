#![recursion_limit="67"]

extern crate combine;
extern crate hocon;


#[cfg(test)]
mod tests {

    use combine::stream::state::State;
    use combine::Parser;
//    use hocon::grammar::hocon;


    static AKKA_CONF: &'static str = include_str!("akka.conf");

    #[test]
    fn test_grammar() {
//        let res = hocon().easy_parse(State::new(AKKA_CONF));
//        assert!(res.is_ok());
    }
}
