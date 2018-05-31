named!(number<f64>, flat_map!(call!(recognize_float), parse_to!(f64)));

named!(
    boolean<bool>,
    alt!(
    tag_no_case!("true")  => { |_| true } |
    tag_no_case!("false") => { |_| false } |
    tag_no_case!("yes")  => { |_| true } |
    tag_no_case!("no") => { |_| false }
  )
);
