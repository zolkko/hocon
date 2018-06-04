use super::config::*;


pub struct ConfigFactory {
    processor: Option<&'static Fn(&Include) -> IncludeResult>,
}

impl ConfigFactory {

    pub fn new() -> ConfigFactory {
        ConfigFactory {
            processor: None,
        }
    }

    pub fn include_processor<'a, T>(&'a mut self, processor: &'static T) -> &'a mut ConfigFactory
    where
        T: Fn(&Include) -> IncludeResult
    {
        self.processor = Some(processor);
        self
    }

    pub fn parse_string(&self, value: &str) -> ParseResult {
        unimplemented!();
    }
}

impl Default for ConfigFactory {

    fn default() -> Self {
        ConfigFactory::new()
    }

}
