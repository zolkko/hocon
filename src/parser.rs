
#[cfg(test)]
mod tests {

    use crate::{HoconParser, Rule};
    use pest::{parses_to, consumes_to};

    #[test]
    fn separator() {
        parses_to! { parser: HoconParser, input: ",", rule: Rule::separator, tokens: [] };
        parses_to! { parser: HoconParser, input: ",\n", rule: Rule::separator, tokens: [] };
        parses_to! { parser: HoconParser, input: "\n", rule: Rule::separator, tokens: [] };
        parses_to! { parser: HoconParser, input: "\n,", rule: Rule::separator, tokens: [] };
        parses_to! { parser: HoconParser, input: "\n# comment\n,//comment\n//comment\n", rule: Rule::separator, tokens: [] };
    }

    #[test]
    fn boolean() {
        parses_to! { parser: HoconParser, input: "yes", rule: Rule::bool, tokens: [ bool_true(0, 3) ] };
        parses_to! { parser: HoconParser, input: "Y", rule: Rule::bool, tokens: [ bool_true(0, 1) ] };
        parses_to! { parser: HoconParser, input: "true", rule: Rule::bool, tokens: [ bool_true(0, 4) ] };
        parses_to! { parser: HoconParser, input: "false", rule: Rule::bool, tokens: [ bool_false(0, 5) ] };
        parses_to! { parser: HoconParser, input: "no", rule: Rule::bool, tokens: [ bool_false(0, 2) ] };
    }

    #[test]
    fn null() {
        parses_to! { parser: HoconParser, input: "null", rule:  Rule::null, tokens: [ null(0, 4) ] };
        parses_to! { parser: HoconParser, input: "Null", rule:  Rule::null, tokens: [ null(0, 4) ] };
    }

    #[test]
    fn integer() {
        parses_to! {
            parser: HoconParser,
            input: "null",
            rule:   Rule::null,
            tokens: [
                null(0, 4)
            ]
        };
    }
}
