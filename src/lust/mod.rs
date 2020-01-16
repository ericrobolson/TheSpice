use std::collections::HashMap;
use std::fmt;
use std::io;
use std::num;
use std::rc::Rc;

//TODO: Create macros for embeddable LUST code?
// TODO: Rename 'Lust' to 'Lust'

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[LustExp]| -> Result<LustExp, LustErr> {
            let floats = parse_list_of_floats(args)?;
            let first = floats
                .first()
                .ok_or(LustErr::Reason("expected at least one number".to_string()))?;
            let rest = &floats[1..];
            fn f(prev: &LustFloat, xs: &[LustFloat]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true,
                }
            };
            Ok(LustExp::Bool(f(first, rest)))
        }
    }};
}



pub type LustFloat = f64; // can be subbed with fixed point integer for determinism


#[derive(Clone)]
pub enum LustExp {
    Bool(bool),
    Symbol(String),
    Number(LustFloat),
    Str(String),
    List(Vec<LustExp>),
    Func(fn(&[LustExp]) -> Result<LustExp, LustErr>),
    Lambda(LustLambda)
}

#[derive(Clone)]
pub struct LustLambda {
    params_exp: Rc<LustExp>,
    body_exp: Rc<LustExp>
}

#[derive(Debug)]
pub enum LustErr {
    Reason(String),
}

#[derive(Clone)]
pub struct LustEnv<'a> {
    data: HashMap<String, LustExp>,
    outer: Option<&'a LustEnv<'a>>
}

impl fmt::Display for LustExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            LustExp::Bool(b) => b.to_string(),
            LustExp::Symbol(s) => s.clone(),
            LustExp::Number(n) => n.to_string(),
            LustExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            LustExp::Func(_) => "Function {}".to_string(),
            LustExp::Lambda(_) => "Lambda {}".to_string(),
            LustExp::Str(_) => "String {}".to_string(),
        };
        return write!(f, "{}", str);
    }
}

fn default_env<'a>() -> LustEnv<'a> {
    let mut data: HashMap<String, LustExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        LustExp::Func(|args: &[LustExp]| -> Result<LustExp, LustErr> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);
            Ok(LustExp::Number(sum))
        }),
    );

    data.insert(
        "-".to_string(),
        LustExp::Func(|args: &[LustExp]| -> Result<LustExp, LustErr> {
            let floats = parse_list_of_floats(args)?;
            let first = *floats
                .first()
                .ok_or(LustErr::Reason("expected at least one number".to_string()))?;

            let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);
            Ok(LustExp::Number(first - sum_of_rest))
        }),
    );

    data.insert(
        "=".to_string(),
        LustExp::Func(ensure_tonicity!(|a, b| a == b)),
    );
    data.insert(
        ">".to_string(),
        LustExp::Func(ensure_tonicity!(|a, b| a > b)),
    );
    data.insert(
        ">=".to_string(),
        LustExp::Func(ensure_tonicity!(|a, b| a >= b)),
    );
    data.insert(
        "<".to_string(),
        LustExp::Func(ensure_tonicity!(|a, b| a < b)),
    );
    data.insert(
        "<=".to_string(),
        LustExp::Func(ensure_tonicity!(|a, b| a <= b)),
    );
   
    return LustEnv { data, outer: None };
}

fn env_for_lambda<'a>(params: Rc<LustExp>, arg_forms: &[LustExp], outer_env: &'a mut LustEnv) -> Result<LustEnv<'a>, LustErr>{
    let ks = parse_list_of_symbol_strings(params)?;
    if ks.len() != arg_forms.len(){
        return Err(LustErr::Reason(format!("expected {} arguments, got {}", ks.len(), arg_forms.len())));
    }

    let vs = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, LustExp> = HashMap::new();
    for (k, v) in ks.iter().zip(vs.iter()) {
        data.insert(k.clone(), v.clone());
    }

    return Ok(
        LustEnv{
            data, 
            outer: Some(outer_env)
        }
    );
}

fn parse_list_of_symbol_strings(form: Rc<LustExp>) -> Result<Vec<String>, LustErr>{
    let list = match form.as_ref(){
        LustExp::List(s) => Ok(s.clone()),
        _ => Err(LustErr::Reason(
            "expected args form to be a list".to_string()
        ))
    }?;

    return list.iter()
        .map(|x| {
            match x {
                LustExp::Symbol(s) => Ok(s.clone()), 
                _ => Err(LustErr::Reason(
                    "expected symbols in the argument list".to_string()
                ))
            }
        })
        .collect();
}

fn env_get(k: &str, env: &LustEnv) -> Option<LustExp> {
    match env.data.get(k) {
        Some(exp) => Some(exp.clone()),
        None => {
            match &env.outer{
                Some(outer_env) => env_get(k, &outer_env),
                None => None
            }
        }
    }
}


fn parse_list_of_floats(args: &[LustExp]) -> Result<Vec<LustFloat>, LustErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn parse_single_float(exp: &LustExp) -> Result<LustFloat, LustErr> {
    match exp {
        LustExp::Number(num) => Ok(*num),
        _ => Err(LustErr::Reason("expected a number".to_string())),
    }
}

fn eval(exp: &LustExp, env: &mut LustEnv) -> Result<LustExp, LustErr> {
    match exp {
        LustExp::Bool(_a) => Ok(exp.clone()),
        LustExp::Str(_str) => Ok(exp.clone()),
        LustExp::Symbol(k) => 
            env_get(k, env)
            .ok_or(LustErr::Reason(format!("unexpected symbol k: '{}'", k)))
            .map(|x| x.clone()),
        LustExp::Number(_a) => Ok(exp.clone()),
        LustExp::List(list) => {
      let first_form = list
        .first()
        .ok_or(LustErr::Reason("expected a non-empty list".to_string()))?;
      let arg_forms = &list[1..];
      match eval_built_in_form(first_form, arg_forms, env) {
        Some(res) => res,
        None => {
          let first_eval = eval(first_form, env)?;
          match first_eval {
            LustExp::Func(f) => {
              let args_eval = arg_forms
                .iter()
                .map(|x| eval(x, env))
                .collect::<Result<Vec<LustExp>, LustErr>>();
              return f(&args_eval?);
            },
            LustExp::Lambda(lambda) => {
                let new_env = &mut env_for_lambda(lambda.params_exp, arg_forms, env)?;
                eval(&lambda.body_exp, new_env)
            },
            _ => Err(
              LustErr::Reason("first form must be a function".to_string())
            ),
          }
        }
      }
    },

        LustExp::Func(_) => Err(LustErr::Reason("unexpected form".to_string())),        
        LustExp::Lambda(_) => Err(LustErr::Reason("unexpected form".to_string()))
    }
}



fn eval_forms(arg_forms: &[LustExp], env: &mut LustEnv) -> Result<Vec<LustExp>, LustErr> {
    return arg_forms
        .iter()
        .map(|x| eval(x, env))
        .collect();
}

fn eval_built_in_form(
    exp: &LustExp, arg_forms: &[LustExp], env: &mut LustEnv
) -> Option<Result<LustExp, LustErr>> {
    match exp {
        LustExp::Symbol(s) => 
            match s.as_ref() {
                "if" => Some(eval_if_args(arg_forms, env)),
                "defn" => Some(eval_def_args(arg_forms, env)),
                "fn" => Some(eval_lambda_args(arg_forms)),
                _ => None,
            },
        _ => None
    }
}

fn eval_lambda_args(arg_forms: &[LustExp]) -> Result<LustExp, LustErr>{
    let params_exp = arg_forms.first().ok_or(
        LustErr::Reason(
            "expected args form".to_string()
        )
    )?;

    let body_exp = arg_forms.get(1).ok_or(
        LustErr::Reason(
            "expected second form".to_string()
        )
    )?;

    if arg_forms.len() > 2 {
        return Err(
            LustErr::Reason(
                "fn definition can only have two forms ".to_string()
            )
        )
    }

    return Ok(
        LustExp::Lambda(
            LustLambda {
                body_exp: Rc::new(body_exp.clone()),
                params_exp: Rc::new(params_exp.clone())
            }
        )
    );
}

fn eval_if_args(arg_forms: &[LustExp], env: &mut LustEnv) -> Result<LustExp, LustErr>{
    let test_form = arg_forms.first().ok_or(
        LustErr::Reason(
            "expected test form".to_string()
        )
    )?;

    let test_eval = eval(test_form, env)?;
    match test_eval {
        LustExp::Bool(b) => {
            let form_idx = if b {1} else{2};
            let res_form = arg_forms.get(form_idx)
            .ok_or(LustErr::Reason(
                format!("expected form idx={}", form_idx)
            ))?;
            let res_eval = eval(res_form, env);

            return res_eval;
        },
        _ => Err(
            LustErr::Reason(format!("unexpected test form '{}'", test_form.to_string()))
        )
    }
}

fn eval_def_args(arg_forms: &[LustExp], env: &mut LustEnv) -> Result<LustExp, LustErr> {
    let first_form = arg_forms.first().ok_or(
        LustErr::Reason(
            "expected first form".to_string()
        )
    )?;

    let first_str = match first_form {
        LustExp::Symbol(s) => Ok(s.clone()),
        _ => Err(LustErr::Reason("expected first form to be a symbol".to_string()))
    }?;

      let second_form = arg_forms.get(1).ok_or(
    LustErr::Reason(
      "expected second form".to_string()
    )
  )?;
  if arg_forms.len() > 2 {
    return Err(
      LustErr::Reason(
        "def can only have two forms ".to_string()
      )
    )
  } 
  let second_eval = eval(second_form, env)?;
  env.data.insert(first_str, second_eval);
  
  return Ok(first_form.clone())
}


fn parse_eval(expr: String, env: &mut LustEnv) -> Result<LustExp, LustErr> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;
    return Ok(evaled_exp);
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");
    return expr;
}

pub fn execute_repl() {

    let mut runtime = LustRuntime::new();
    
    loop {
        println!("Lust >");
        let expr = slurp_expr();


        match runtime.execute(expr) {
            Ok(res) => println!("// OK => {}", res),
            Err(e) => match e {
                LustErr::Reason(msg) => println!("// Error => {}", msg),
            },
        }
    }
}


pub struct LustRuntime<'a>{
    env: LustEnv<'a>
}

impl LustRuntime<'_>{
    pub fn new() -> LustRuntime<'static>{
        return LustRuntime{
            env: default_env(),
        }
    }

    pub fn execute_s(&mut self, expr: String) -> Result<String, String>{        
        match parse_eval(expr, &mut self.env){
            Ok(res) => return Ok(res.to_string()),
            Err(e) => {
                match e {
                    LustErr::Reason(s) => return Err(s),
                    _ => return Err("unable to determine error".to_string())
                }
            }
        }
    }


    fn execute(&mut self, expr: String) -> Result<LustExp, LustErr>{        
        return parse_eval(expr, &mut self.env);
    }
}


pub fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .replace(";", " ; ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

pub fn parse<'a>(tokens: &'a [String]) -> Result<(LustExp, &'a [String]), LustErr> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(LustErr::Reason("could not get token".to_string()))?;
    match &token[..] {
        "\"" => read_str(rest, '"'),
        "\'" => read_str(rest, '\''),
        "(" => read_seq(rest),
        ")" => Err(LustErr::Reason("unexpected `)`".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_str<'a>(tokens: &'a [String], end_character: char) -> Result<(LustExp, &'a [String]), LustErr> {
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
            end_character_index = i;
            break;
        }        

        string.push_str(&tokens[i]);
    }

    if !end_character_found {
        return Err(LustErr::Reason(format!("could not find closing `{}`", end_character).to_string()));
    }

    return Ok((LustExp::Str(string), &tokens[end_character_index..]));  
}

fn read_seq<'a>(tokens: &'a [String]) -> Result<(LustExp, &'a [String]), LustErr> {
    let mut res: Vec<LustExp> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(LustErr::Reason("could not find closing `)`".to_string()))?;
        if next_token == ")" {
            return Ok((LustExp::List(res), rest)); // skip `)`, head to the token after
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> LustExp {
    match token.as_ref() {
        "true" => LustExp::Bool(true),
        "false" => LustExp::Bool(false),
        _ => {
            let potential_float: Result<LustFloat, num::ParseFloatError> = token.parse();
            match potential_float {
                Ok(v) => LustExp::Number(v),
                Err(_) => LustExp::Symbol(token.to_string().clone()),
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_numbers(){
        let mut runtime = LustRuntime::new();

        let mut res = runtime.execute_s("(+ 1 2)".to_string());
        assert_eq!(res.unwrap(), "3");

        res = runtime.execute_s("(+ 3 -2)".to_string());
        assert_eq!(res.unwrap(), "1");    
    }

    #[test]
    fn lambda_one(){
        let mut runtime = LustRuntime::new();

        runtime.execute_s("(defn fd (fn (a) (a)))".to_string());

        let mut res = runtime.execute_s("(fd 3)".to_string());


        assert_eq!(res.unwrap(), "3");
    }
}
