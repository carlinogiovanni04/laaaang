use std::env;
use std::process::exit;
use std::fs;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    TokIdentifier(String),
    TokNumber(String),
    TokString(String),
    TokCharacter(String),
    TokPlus,
    TokMinus,
    TokLParen,
    TokRParen,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(i64),
    Str(String), 
    Char(char),
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Call {
        callee: String,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Str(String),
    Char(char)
}

fn file_read_to_string(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| e.to_string())
}

fn tokenize(content: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = content.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\n' | '\r' | '\t' => {
                chars.next();
            }
            '+' => {
                tokens.push(Token::TokPlus);
                chars.next();
            }
            '-' => {
                tokens.push(Token::TokMinus);
                chars.next();
            }
            '(' => {
                tokens.push(Token::TokLParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::TokRParen);
                chars.next();
            }
            '0'..='9' => {
                let mut num_str = String::new();
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_ascii_digit() {
                        num_str.push(next_c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::TokNumber(num_str));
            }
            '"' => {
                chars.next(); // Consume opening quote
                let mut string_val = String::new();
                while let Some(&next_c) = chars.peek() {
                    if next_c == '"' {
                        break;
                    }
                    string_val.push(next_c);
                    chars.next();
                }
                if chars.peek() == Some(&'"') {
                    chars.next(); // Consume closing quote
                } else {
                    return Err("Unterminated string literal".into());
                }
                tokens.push(Token::TokString(string_val));
            }
            '\'' => {
                chars.next();
                let mut c_val = String::new();
                while let Some(&next_c) = chars.peek(){
                    if next_c == '\'' {
                        break;
                    }
                    c_val.push(next_c);
                    chars.next();
                }
                if chars.peek() == Some(&'\'') { chars.next(); }
                else { 
                    return Err("Unterminated char quotes".into());
                }
                tokens.push(Token::TokCharacter(c_val));
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_alphanumeric() || next_c == '_' {
                        ident.push(next_c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::TokIdentifier(ident));
            }
            _ => {
                return Err(format!("Unexpected character: '{}'", c));
            }
        }
    }

    Ok(tokens)
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, position: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.position).cloned();
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        let token = self.advance().ok_or("Unexpected end of input")?;

        match token {
            Token::TokIdentifier(name) => {
                match self.advance() {
                    Some(Token::TokLParen) => {}
                    other => return Err(format!("Expected '(', got {:?}", other)),
                }

                let arg = self.parse_expr()?;

                match self.advance() {
                    Some(Token::TokRParen) => {}
                    other => return Err(format!("Expected ')', got {:?}", other)),
                }

                Ok(Expr::Call {
                    callee: name,
                    arguments: vec![arg],
                })
            }
            other => Err(format!("Expected identifier, got {:?}", other)),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        let left_token = self.advance().ok_or("Expected expression")?;
        let left = match left_token {
            Token::TokNumber(n) => Expr::Num(n.parse::<i64>().map_err(|e| e.to_string())?),
            Token::TokString(s) => Expr::Str(s),
            Token::TokCharacter(c) => Expr::Char(c.parse::<char>().map_err(|e| e.to_string())?),
            other => return Err(format!("Expected expression, got {:?}", other)),
        };

        // If it's a string, it won't have arithmetic operators, so we just return it
        if let Expr::Str(_) = left {
            return Ok(left);
        }

        if let Some(op @ (Token::TokPlus | Token::TokMinus)) = self.peek() {
            let op = op.clone();
            self.advance(); // consume operator

            let right_token = self.advance().ok_or("Expected expression after operator")?;
            let right = match right_token {
                Token::TokNumber(n) => Expr::Num(n.parse::<i64>().map_err(|e| e.to_string())?),
                other => return Err(format!("Expected number after operator, got {:?}", other)),
            };

            return Ok(Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            });
        }

        Ok(left)
    }
}

fn eval(expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Num(n) => Ok(Value::Int(*n)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Char(c) => Ok(Value::Char(*c)),
        Expr::Binary { left, op, right } => {
            let l_val = eval(left)?;
            let r_val = eval(right)?;
            
            match (l_val, op, r_val) {
                (Value::Int(l), Token::TokPlus, Value::Int(r)) => Ok(Value::Int(l + r)),
                (Value::Int(l), Token::TokMinus, Value::Int(r)) => Ok(Value::Int(l - r)),
                (Value::Str(l), Token::TokPlus, Value::Str(r)) => Ok(Value::Str(format!("{}{}", l, r))),
                _ => Err("Invalid operand types for operator".into()),
            }
        }
        Expr::Call { callee, arguments } => {
            if callee == "print" {
                if arguments.is_empty() {
                    return Err("print expects at least 1 argument".into());
                }
                let val = eval(&arguments[0])?;
                
                // Print based on the runtime value type
                match &val {
                    Value::Int(n) => println!("{}", n),
                    Value::Str(s) => println!("{}", s),
                    Value::Char(c) => println!("{}", c),
                }
                
                Ok(val)
            } else {
                Err(format!("Unknown function '{}'", callee))
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 2 {
        match file_read_to_string(&args[1]) {
            Ok(content) => {
                match tokenize(&content) {
                    Ok(tokens) => {
                        let mut parser = Parser::new(tokens);
                        match parser.parse() {
                            Ok(ast) => {
                                if let Err(e) = eval(&ast) {
                                    println!("Runtime Error: {}", e);
                                    exit(64);
                                }
                                exit(0);
                            }
                            Err(e) => {
                                println!("Parse Error: {}", e);
                                exit(63);
                            }
                        }
                    }
                    Err(msg) => {
                        println!("Lexer Error: {}", msg);
                        exit(65);
                    }
                }
            }
            Err(msg) => {
                println!("Error reading file: {}", msg);
                exit(66);
            },
        }
    } else {
        println!("Usage: laaaang [script.lg]");
        exit(67);
    } 
}
