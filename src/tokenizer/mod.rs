
pub fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .replace(";", " ; ")
        .replace("{", " { ")
        .replace("}", " } ")        
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_lPabcRp_tokenizes(){
        let value = "(abc)";

        let result = tokenize(value.to_string());

        let expected = vec!["(", "abc", ")"];

        assert_eq!(expected, result);
    }

    
    #[test]
    fn tokenize_lPabc_23_43Rpsemic_tokenizes(){
        let value = "(abc 23 45);";

        let result = tokenize(value.to_string());
        
        let expected = vec!["(", "abc", "23", "45", ")", ";"];

        assert_eq!(expected, result);
    }

    #[test]
    fn tokenize_complex_string_tokenizes(){
        let value = "(abc 23 45){blah ha(haha)};";

        let result = tokenize(value.to_string());
        
        let expected = vec!["(", "abc", "23", "45", ")","{", "blah","ha", "(", "haha", ")", "}", ";"];

        assert_eq!(expected, result);
    }
}
