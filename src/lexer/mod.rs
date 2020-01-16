use std::num;
use std::fmt;


#[derive(Debug, PartialEq)]
pub enum SpiceError {
    Reason(String),
}

#[derive(PartialEq,Debug)]
pub enum Numbers{
    I32(i32)
}


impl fmt::Display for Numbers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Numbers::I32(b) => b.to_string()
        };
        return write!(f, "{}", str);
    }
}

#[derive(PartialEq,Debug)]
pub enum Atoms{
    Bool(bool),
    Number(Numbers)    
}

impl fmt::Display for Atoms {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Atoms::Bool(b) => b.to_string(),
            Atoms::Number(n) => format!("{}", n),
        };
        return write!(f, "{}", str);
    }
}

#[derive(PartialEq, Debug)]
pub enum SpiceExpressions{
    Str(String),
    Atom(Atoms),
    Symbol(String),     
    List(Vec<SpiceExpressions>),
    Func(Vec<SpiceExpressions>),
}

impl fmt::Display for SpiceExpressions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            SpiceExpressions::Atom(b) => format!("{}", b),
            SpiceExpressions::Symbol(s) => s.clone(),
            SpiceExpressions::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            SpiceExpressions::Func(_) => "Function {}".to_string(),            
            SpiceExpressions::Str(_) => "String {}".to_string(),
            //LustExp::Lambda(_) => "Lambda {}".to_string(),
        };
        return write!(f, "{}", str);
    }
}

//TODO: test
pub fn lex(tokens: Vec<String>) -> Result<Vec<SpiceExpressions>, SpiceError>{
    let mut exprs = vec![];

    let mut rest = tokens;

    while rest.is_empty() == false{
        let (token, rest_2) = parse(&rest)?;

        exprs.push(token);
        rest = rest_2.to_vec();
    }

    return Ok(exprs);    
}

//TODO: test
fn parse<'a>(tokens: &'a [String]) -> Result<(SpiceExpressions, &'a [String]), SpiceError> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(SpiceError::Reason("could not get token".to_string()))?;
    match &token[..] {
        "\"" => read_str(rest, '"'),
        "\'" => read_str(rest, '\''),
        "(" => read_seq(rest),
        ")" => Err(SpiceError::Reason("unexpected `)`".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

//TODO: test
fn parse_atom(token: &str) -> SpiceExpressions {
    match token.as_ref() {
        "true" => SpiceExpressions::Atom(Atoms::Bool(true)),
        "false" => SpiceExpressions::Atom(Atoms::Bool(false)),
        _ => {
            let potential_i32: Result<i32, num::ParseIntError> = token.parse();
            match potential_i32 {
                Ok(v) => SpiceExpressions::Atom(Atoms::Number(Numbers::I32(v))),
                Err(_) => SpiceExpressions::Symbol(token.to_string().clone()),
            }
        }
    }
}

//TODO: test
fn read_seq<'a>(tokens: &'a [String]) -> Result<(SpiceExpressions, &'a [String]), SpiceError> {
    let mut res: Vec<SpiceExpressions> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(SpiceError::Reason("could not find closing `)`".to_string()))?;
        if next_token == ")" {
            return Ok((SpiceExpressions::List(res), rest)); // skip `)`, head to the token after
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn read_str<'a>(tokens: &'a [String], end_character: char) -> Result<(SpiceExpressions, &'a [String]), SpiceError> {
    let mut string = String::new();

    let len = tokens.len();
    let mut end_character_found = false;
    let mut end_character_index = 0;

    for i in 0..len{
        string.push_str(&tokens[i]);

        if tokens[i].ends_with(end_character){
            // remove endchar
            string = string.replace(end_character, "");

            end_character_found = true;            
            end_character_index = i + 1;
            break;
        }                
    }

    if !end_character_found {
        return Err(SpiceError::Reason(format!("could not find closing `{}`", end_character).to_string()));
    }

    return Ok((SpiceExpressions::Str(string), &tokens[end_character_index..]));  
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_str_noendchar_returns_err(){
        let tokens = vec!["string".to_string()];

        let result = read_str(&tokens, '.');
                
        assert_eq!(true, result.is_err());
    }

    #[test]
    fn read_str_valid_string_returns_ok(){
        let tokens = vec!["string".to_string(), " string_2".to_string(), ".".to_string()];

        let result = read_str(&tokens, '.');
                
        assert_eq!(false, result.is_err());
        assert_eq!(true, result.is_ok());

        let result = result.unwrap();

        let expected_expr = SpiceExpressions::Str("string string_2".to_string());
        let expected_rest: Vec<std::string::String> = vec![];

        let (actual_expr, actual_rest) = result;  

        assert_eq!(expected_expr, actual_expr);
        assert_eq!(expected_rest, actual_rest);
    }

    #[test]
    fn read_str_valid_string_with_rest_returns_ok(){
        let tokens = vec!["string".to_string(), " string_2".to_string(), ".".to_string(), "a test".to_string()];

        let result = read_str(&tokens, '.');
                
        assert_eq!(false, result.is_err());
        assert_eq!(true, result.is_ok());

        let result = result.unwrap();

        let expected_expr = SpiceExpressions::Str("string string_2".to_string());
        let expected_rest: Vec<std::string::String> = vec!["a test".to_string()];

        let (actual_expr, actual_rest) = result;  

        assert_eq!(expected_expr, actual_expr);
        assert_eq!(expected_rest, actual_rest);
    }

}
