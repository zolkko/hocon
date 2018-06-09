#![recursion_limit="67"]

extern crate nom;
extern crate combine;
extern crate hocon;


#[cfg(test)]
mod tests {

    use combine::stream::state::State;
    use combine::*;
    use hocon::grammar::hocon;

    #[test]
    fn test_grammar() {
        let source = include_str!("akka.conf");
        let res = hocon().easy_parse(State::new(source));
        assert!(res.is_ok());
    }
}
