
pub mod tokenizer;
pub mod lexer;
use lexer::{SpiceExpressions, SpiceError};
/*
    Basic functionality:
    * File I/O
    * Printing 
    * Shell executing
*/



pub fn main() -> Result<(),SpiceError>{
    let file_input = "blah".to_string();

    let tokens = tokenizer::tokenize(file_input);
    let lexed_tokens = lexer::lex(tokens)?;
    //TODO: parser/ast builder?
    SyntaxAnalysis();
    SemanticAnalysis();
    IntermediateCodeGeneration();
    CodeOptimization();
    CodeGeneration();
    Compile();

    return Ok(());
}


fn SyntaxAnalysis(){}

fn SemanticAnalysis(){}

fn IntermediateCodeGeneration(){}

fn CodeOptimization(){}

fn CodeGeneration(){}

fn Compile(){}

/*


fn main(){

}


// example spice code
//NOTE: try to distill this down to the bare minimum; make it so you can totally implement the compiler using itself. Research Elm?
pub Node(){
    input_var -> state[value];
    output_var <- state[value];

    event begin(list of vars/functions){

    }

    event end(){
        signals the node is done processing
    }

    fn render(){
        // a self contained view of the component
    }
}


fn do_stuff(params[])

*/