use chrono::Utc;
use std::collections::HashMap;
use std::fmt::Display;
use std::fs;
use std::io::{BufRead, Write, stdin, stdout};
use std::path::Path;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Nil => "nil",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Bool(_) => "bool",
            Value::String(_) => "string",
        }
    }
}

#[derive(Debug, Clone)]
pub enum OpCode {
    Nop,
    Push(Value),
    Pop(usize),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    Dup,
    Print,
    Read,
    Goto(String),
    Br(String),
    Label(String),
    Load(String),
    Store(String),
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
    And,
    Or,
    Not,
    Xor,
    Shl,
    Shr,
    Convert(String),
    Rotl,
    Rotr,
    Clz,
    Ctz,
    Min,
    Max,
    Abs,
    Sign,
    Ceil,
    Floor,
    Trunc,
    Sqrt,
    Len,
    GetArg,
    Pow,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Asinh,
    Acosh,
    Atanh,
    Exp,
    Log,
    Assert,
    Error,
    Exit,
}

#[derive(Debug)]
pub struct Error {
    pub file_name: String,
    pub kind: ErrorKind,
    pub pos: Position,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    InvalidArgument(String),
    UnknownCommand(String),
    StackOverflow,
    StackUnderflow,
    Timeout,
    VariableNotFound(String),
    LabelNotFound(String),
    Io(std::io::Error),
    AssertionFailed(String),
    CustomError(String),
    Exit(i64),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::InvalidArgument(cmd) => write!(f, "{}", cmd),
            ErrorKind::UnknownCommand(cmd) => write!(f, "unknown command: {}", cmd),
            ErrorKind::StackUnderflow => write!(f, "stack is empty"),
            ErrorKind::VariableNotFound(var) => write!(f, "variable `{}` not found", var),
            ErrorKind::LabelNotFound(label) => write!(f, "label `{}` not found", label),
            ErrorKind::Io(err) => write!(f, "io error: {}", err),
            ErrorKind::AssertionFailed(msg) => write!(f, "assertion failed: {}", msg),
            ErrorKind::CustomError(msg) => write!(f, "{}", msg),
            ErrorKind::Exit(code) => write!(f, "exit with code: {}", code),
            ErrorKind::StackOverflow => write!(f, "stack overflow"),
            ErrorKind::Timeout => write!(f, "session timeout"),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{} {}", self.file_name, self.pos, self.kind)
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub struct Errors {
    inner: Vec<Error>,
}

impl Errors {
    pub fn inner(&self) -> &[Error] {
        &self.inner
    }
}

impl From<Error> for Errors {
    fn from(value: Error) -> Self {
        Errors { inner: vec![value] }
    }
}

impl Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for e in &self.inner {
            writeln!(f, "{e}")?
        }

        Ok(())
    }
}

impl std::error::Error for Errors {}

// Helper functions for parsing literals with extended format support
fn parse_integer(s: &str) -> Option<i64> {
    if let Some(hex_str) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        i64::from_str_radix(hex_str, 16).ok()
    } else if let Some(bin_str) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
        i64::from_str_radix(bin_str, 2).ok()
    } else {
        s.parse::<i64>().ok()
    }
}

fn parse_float(s: &str) -> Option<f64> {
    s.parse::<f64>().ok()
}

pub struct Script {
    file_name: String,
    program: Vec<OpCode>,
    labels: std::collections::HashMap<String, usize>,
    lines: Vec<usize>,
}

impl Script {
    pub fn from_str(source: &str) -> Result<Self, Errors> {
        Self::parse(source, "main")
    }

    pub fn from_file<T: AsRef<Path>>(path: &T) -> Result<Self, Errors> {
        let file_name = path
            .as_ref()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        let buf = fs::read_to_string(path).map_err(|e| Errors {
            inner: vec![Error {
                file_name: file_name.clone(),
                kind: ErrorKind::Io(e),
                pos: Position::default(),
            }],
        })?;

        Self::parse(&buf, &file_name)
    }

    pub fn parse(source: &str, file_name: &str) -> Result<Self, Errors> {
        let mut program = Vec::new();
        let mut errors = Vec::new();
        let mut lines: Vec<usize> = Vec::new();

        for (idx, raw_line) in source.lines().enumerate() {
            let line_no = idx + 1;
            let without_comment = raw_line.split(';').next().unwrap_or("");
            let line = without_comment.trim();
            if line.is_empty() {
                continue;
            }

            // compute column of command (approximate using position in original without_comment)
            let cmd_token = line.split_whitespace().next().unwrap_or("");
            let cmd_col = without_comment.find(cmd_token).map(|p| p + 1).unwrap_or(1);

            // helper to push opcode and record its source line
            let mut push = |op: OpCode| {
                program.push(op);
                lines.push(line_no);
            };

            // per-line helper to parse inline arguments into Values.
            // expected: Some(n) to require exactly n values, None to accept any number >=1
            let mut parse_inline_values = |s: &str,
                                           expected: Option<usize>|
             -> Option<Vec<Value>> {
                // Split arguments by spaces, but handle quoted strings properly
                let mut parts: Vec<String> = Vec::new();
                let mut current = String::new();
                let mut in_quotes = false;

                for c in s.chars() {
                    match c {
                        '"' => {
                            in_quotes = !in_quotes;
                            current.push(c);
                        }
                        ' ' if !in_quotes => {
                            if !current.is_empty() {
                                parts.push(current);
                                current = String::new();
                            }
                        }
                        _ => current.push(c),
                    }
                }
                if !current.is_empty() {
                    parts.push(current);
                }

                if parts.is_empty() {
                    let col = without_comment.find(s).map(|p| p + 1).unwrap_or(cmd_col);
                    errors.push(Error {
                        file_name: file_name.to_string(),
                        kind: ErrorKind::InvalidArgument("requires at least one value".to_string()),
                        pos: Position { line: line_no, col },
                    });
                    return None;
                }

                if let Some(n) = expected {
                    if parts.len() != n {
                        let col = without_comment.find(s).map(|p| p + 1).unwrap_or(cmd_col);
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(format!("requires {} value(s)", n)),
                            pos: Position { line: line_no, col },
                        });
                        return None;
                    }
                }

                let mut vals = Vec::new();
                for p in parts {
                    let p_trim = p.trim();
                    let val_col = without_comment
                        .find(p_trim)
                        .map(|q| q + 1)
                        .unwrap_or(cmd_col);
                    if let Some(i) = parse_integer(p_trim) {
                        vals.push(Value::Int(i));
                    } else if let Some(f) = parse_float(p_trim) {
                        vals.push(Value::Float(f));
                    } else if p_trim == "true" {
                        vals.push(Value::Bool(true));
                    } else if p_trim == "false" {
                        vals.push(Value::Bool(false));
                    } else if p_trim == "nil" {
                        vals.push(Value::Nil);
                    } else if p_trim.starts_with('"') && p_trim.ends_with('"') {
                        let s = p_trim[1..p_trim.len() - 1].to_string();
                        vals.push(Value::String(s));
                    } else {
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(format!("invalid value: {}", p_trim)),
                            pos: Position {
                                line: line_no,
                                col: val_col,
                            },
                        });
                        return None;
                    }
                }

                Some(vals)
            };

            let parts: Vec<&str> = line.splitn(2, ' ').collect();
            if parts.is_empty() {
                continue;
            }
            let cmd = parts[0];
            let arg = parts.get(1).map(|s| s.trim()).unwrap_or("");

            match cmd {
                "nop" => {
                    if arg.is_empty() {
                        push(OpCode::Nop);
                    } else {
                        let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `nop` (takes no arguments)".to_string(),
                            ),
                            pos: Position { line: line_no, col },
                        });
                    }
                }
                "dup" => {
                    if arg.is_empty() {
                        push(OpCode::Dup);
                    } else {
                        let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `dup` (takes no arguments)".to_string(),
                            ),
                            pos: Position { line: line_no, col },
                        });
                    }
                }
                "print" => {
                    if arg.is_empty() {
                        push(OpCode::Print);
                    } else if let Some(vals) = parse_inline_values(arg, Some(1)) {
                        push(OpCode::Push(vals[0].clone()));
                        push(OpCode::Print);
                    } else {
                        let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `print` (requires a single value)"
                                    .to_string(),
                            ),
                            pos: Position { line: line_no, col },
                        });
                    }
                }
                "read" => {
                    if arg.is_empty() {
                        push(OpCode::Read);
                    } else {
                        let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `read` (takes no arguments)".to_string(),
                            ),
                            pos: Position { line: line_no, col },
                        });
                    }
                }
                "push" => {
                    if arg.is_empty() {
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `push` (requires at least one value)"
                                    .to_string(),
                            ),
                            pos: Position {
                                line: line_no,
                                col: cmd_col,
                            },
                        });
                        continue;
                    }

                    // Split arguments by spaces, but handle quoted strings properly
                    let mut values = Vec::new();
                    let mut current = String::new();
                    let mut in_quotes = false;

                    for c in arg.chars() {
                        match c {
                            '"' => {
                                in_quotes = !in_quotes;
                                current.push(c);
                            }
                            ' ' if !in_quotes => {
                                if !current.is_empty() {
                                    values.push(current);
                                    current = String::new();
                                }
                            }
                            _ => current.push(c),
                        }
                    }
                    if !current.is_empty() {
                        values.push(current);
                    }

                    if values.is_empty() {
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `push` (requires at least one value)"
                                    .to_string(),
                            ),
                            pos: Position {
                                line: line_no,
                                col: cmd_col,
                            },
                        });
                        continue;
                    }

                    for val in values {
                        let val_trim = val.trim();
                        // approximate column for this value
                        let val_col = without_comment
                            .find(val_trim)
                            .map(|p| p + 1)
                            .unwrap_or(cmd_col);
                        if let Some(i) = parse_integer(val_trim) {
                            push(OpCode::Push(Value::Int(i)));
                        } else if let Some(f) = parse_float(val_trim) {
                            push(OpCode::Push(Value::Float(f)));
                        } else if val_trim == "true" {
                            push(OpCode::Push(Value::Bool(true)));
                        } else if val_trim == "false" {
                            push(OpCode::Push(Value::Bool(false)));
                        } else if val_trim == "nil" {
                            push(OpCode::Push(Value::Nil));
                        } else if val_trim.starts_with('"') && val_trim.ends_with('"') {
                            let s = val_trim[1..val_trim.len() - 1].to_string();
                            push(OpCode::Push(Value::String(s)));
                        } else {
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "invalid value: {}",
                                    val_trim
                                )),
                                pos: Position {
                                    line: line_no,
                                    col: val_col,
                                },
                            });
                        }
                    }
                }
                "pop" => {
                    let n = if arg.is_empty() {
                        1
                    } else {
                        match arg.parse::<usize>() {
                            Ok(v) => v,
                            Err(_) => {
                                errors.push(Error {
                                    file_name: file_name.to_string(),
                                    kind: ErrorKind::InvalidArgument(
                                        "unexpected argument to `pop` (takes a positive integer)"
                                            .to_string(),
                                    ),
                                    pos: Position {
                                        line: line_no,
                                        col: cmd_col,
                                    },
                                });
                                1
                            }
                        }
                    };
                    push(OpCode::Pop(n));
                }
                "add" => {
                    if arg.is_empty() {
                        push(OpCode::Add);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = match (&a, &b) {
                            (Value::Int(_), Value::Int(_)) => true,
                            (Value::Float(_), Value::Float(_)) => true,
                            (Value::Int(_), Value::Float(_)) => true,
                            (Value::Float(_), Value::Int(_)) => true,
                            (Value::String(_), Value::String(_)) => true,
                            _ => false,
                        };
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "attempt to add `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Add);
                        }
                    }
                }
                "sub" => {
                    if arg.is_empty() {
                        push(OpCode::Sub);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Int(_), Value::Int(_))
                                | (Value::Float(_), Value::Float(_))
                                | (Value::Int(_), Value::Float(_))
                                | (Value::Float(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "attempt to sub `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Sub);
                        }
                    }
                }
                "mul" => {
                    if arg.is_empty() {
                        push(OpCode::Mul);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Int(_), Value::Int(_))
                                | (Value::Float(_), Value::Float(_))
                                | (Value::Int(_), Value::Float(_))
                                | (Value::Float(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "attempt to mul `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Mul);
                        }
                    }
                }
                "div" => {
                    if arg.is_empty() {
                        push(OpCode::Div);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Int(_), Value::Int(_))
                                | (Value::Float(_), Value::Float(_))
                                | (Value::Int(_), Value::Float(_))
                                | (Value::Float(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "attempt to div `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Div);
                        }
                    }
                }
                "mod" => {
                    if arg.is_empty() {
                        push(OpCode::Mod);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!((&a, &b), (Value::Int(_), Value::Int(_)));
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "attempt to mod `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Mod);
                        }
                    }
                }
                "neg" => {
                    if arg.is_empty() {
                        push(OpCode::Neg);
                    } else if let Some(vals) = parse_inline_values(arg, Some(1)) {
                        let v = vals[0].clone();
                        match v {
                            Value::Int(_) | Value::Float(_) => {
                                push(OpCode::Push(v));
                                push(OpCode::Neg);
                            }
                            _ => {
                                let col =
                                    without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                                errors.push(Error {
                                    file_name: file_name.to_string(),
                                    kind: ErrorKind::InvalidArgument(format!(
                                        "cannot apply neg to `{}`",
                                        v.type_name()
                                    )),
                                    pos: Position { line: line_no, col },
                                });
                            }
                        }
                    }
                }
                "gt" => {
                    if arg.is_empty() {
                        push(OpCode::Gt);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Int(_), Value::Int(_))
                                | (Value::Float(_), Value::Float(_))
                                | (Value::Int(_), Value::Float(_))
                                | (Value::Float(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "cannot compare `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Gt);
                        }
                    }
                }
                "lt" => {
                    if arg.is_empty() {
                        push(OpCode::Lt);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Int(_), Value::Int(_))
                                | (Value::Float(_), Value::Float(_))
                                | (Value::Int(_), Value::Float(_))
                                | (Value::Float(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "cannot compare `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Lt);
                        }
                    }
                }
                "ge" => {
                    if arg.is_empty() {
                        push(OpCode::Ge);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Int(_), Value::Int(_))
                                | (Value::Float(_), Value::Float(_))
                                | (Value::Int(_), Value::Float(_))
                                | (Value::Float(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "cannot compare `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Ge);
                        }
                    }
                }
                "le" => {
                    if arg.is_empty() {
                        push(OpCode::Le);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Int(_), Value::Int(_))
                                | (Value::Float(_), Value::Float(_))
                                | (Value::Int(_), Value::Float(_))
                                | (Value::Float(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "cannot compare `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Le);
                        }
                    }
                }
                "and" => {
                    if arg.is_empty() {
                        push(OpCode::And);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Bool(_), Value::Bool(_)) | (Value::Int(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "cannot compare `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::And);
                        }
                    }
                }
                "or" => {
                    if arg.is_empty() {
                        push(OpCode::Or);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Bool(_), Value::Bool(_)) | (Value::Int(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "cannot compare `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Or);
                        }
                    }
                }
                "not" => {
                    if arg.is_empty() {
                        push(OpCode::Not);
                    } else if let Some(vals) = parse_inline_values(arg, Some(1)) {
                        let v = vals[0].clone();
                        if matches!(v, Value::Bool(_) | Value::Int(_)) {
                            push(OpCode::Push(v));
                            push(OpCode::Not);
                        } else {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "cannot apply not to `{}`",
                                    v.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        }
                    }
                }
                "xor" => {
                    if arg.is_empty() {
                        push(OpCode::Xor);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        let ok = matches!(
                            (&a, &b),
                            (Value::Bool(_), Value::Bool(_)) | (Value::Int(_), Value::Int(_))
                        );
                        if !ok {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "cannot compare `{}` and `{}`",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Xor);
                        }
                    }
                }
                "shl" => {
                    if arg.is_empty() {
                        push(OpCode::Shl);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        if !matches!((&a, &b), (Value::Int(_), Value::Int(_))) {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "unexpected argument to `shl` (requires two ints, found {} and {})",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Shl);
                        }
                    }
                }
                "shr" => {
                    if arg.is_empty() {
                        push(OpCode::Shr);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        let a = vals[0].clone();
                        let b = vals[1].clone();
                        if !matches!((&a, &b), (Value::Int(_), Value::Int(_))) {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(format!(
                                    "unexpected argument to `shr` (requires two ints, found {} and {})",
                                    a.type_name(),
                                    b.type_name()
                                )),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Shr);
                        }
                    }
                }
                "convert" => {
                    // expect one inline type name: convert <type>
                    const VALID_TYPES: &[&str] = &["string", "int", "float", "ptr", "bool", "nil"];
                    if arg.is_empty() {
                        let col = cmd_col;
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `convert` (requires a type name)"
                                    .to_string(),
                            ),
                            pos: Position { line: line_no, col },
                        });
                    } else {
                        let parts: Vec<&str> = arg.split_whitespace().collect();
                        if parts.len() != 1 {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `convert` (requires a type name)"
                                        .to_string(),
                                ),
                                pos: Position { line: line_no, col },
                            });
                        } else {
                            let to = parts[0];
                            if !VALID_TYPES.contains(&to) {
                                let col =
                                    without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                                errors.push(Error {
                                    file_name: file_name.to_string(),
                                    kind: ErrorKind::InvalidArgument(format!(
                                        "invalid type name: {}",
                                        to
                                    )),
                                    pos: Position { line: line_no, col },
                                });
                            } else {
                                push(OpCode::Convert(to.to_string()));
                            }
                        }
                    }
                }
                "rotl" => {
                    if arg.is_empty() {
                        push(OpCode::Rotl);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        // expect two integers
                        if matches!(vals[0], Value::Int(_)) && matches!(vals[1], Value::Int(_)) {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Rotl);
                        } else {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `rotl` (requires two integers)"
                                        .to_string(),
                                ),
                                pos: Position { line: line_no, col },
                            });
                        }
                    }
                }
                "rotr" => {
                    if arg.is_empty() {
                        push(OpCode::Rotr);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        if matches!(vals[0], Value::Int(_)) && matches!(vals[1], Value::Int(_)) {
                            for v in vals {
                                push(OpCode::Push(v));
                            }
                            push(OpCode::Rotr);
                        } else {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `rotr` (requires two integers)"
                                        .to_string(),
                                ),
                                pos: Position { line: line_no, col },
                            });
                        }
                    }
                }
                "clz" => {
                    if arg.is_empty() {
                        push(OpCode::Clz);
                    } else if let Some(vals) = parse_inline_values(arg, Some(1)) {
                        if matches!(vals[0], Value::Int(_)) {
                            push(OpCode::Push(vals[0].clone()));
                            push(OpCode::Clz);
                        } else {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `clz` (requires an integer)"
                                        .to_string(),
                                ),
                                pos: Position { line: line_no, col },
                            });
                        }
                    }
                }
                "ctz" => {
                    if arg.is_empty() {
                        push(OpCode::Ctz);
                    } else if let Some(vals) = parse_inline_values(arg, Some(1)) {
                        if matches!(vals[0], Value::Int(_)) {
                            push(OpCode::Push(vals[0].clone()));
                            push(OpCode::Ctz);
                        } else {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `ctz` (requires an integer)"
                                        .to_string(),
                                ),
                                pos: Position { line: line_no, col },
                            });
                        }
                    }
                }
                "eq" => {
                    if arg.is_empty() {
                        push(OpCode::Eq);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        for v in vals {
                            push(OpCode::Push(v));
                        }
                        push(OpCode::Eq);
                    }
                }
                "ne" => {
                    if arg.is_empty() {
                        push(OpCode::Ne);
                    } else if let Some(vals) = parse_inline_values(arg, Some(2)) {
                        for v in vals {
                            push(OpCode::Push(v));
                        }
                        push(OpCode::Ne);
                    }
                }
                "min" => {
                    if arg.is_empty() {
                        push(OpCode::Min);
                    } else if let Some(vals) = parse_inline_values(arg, None) {
                        for v in &vals {
                            push(OpCode::Push(v.clone()));
                        }
                        if vals.len() == 1 {
                            push(OpCode::Min);
                        } else {
                            for _ in 0..(vals.len() - 1) {
                                push(OpCode::Min);
                            }
                        }
                    }
                }
                "max" => {
                    if arg.is_empty() {
                        push(OpCode::Max);
                    } else if let Some(vals) = parse_inline_values(arg, None) {
                        for v in &vals {
                            push(OpCode::Push(v.clone()));
                        }
                        if vals.len() == 1 {
                            push(OpCode::Max);
                        } else {
                            for _ in 0..(vals.len() - 1) {
                                push(OpCode::Max);
                            }
                        }
                    }
                }
                "abs" => {
                    if arg.is_empty() {
                        push(OpCode::Abs);
                    } else {
                        let val = arg;
                        let _val_col = without_comment.find(val).map(|p| p + 1).unwrap_or(cmd_col);
                        if let Some(i) = parse_integer(val) {
                            push(OpCode::Push(Value::Int(i)));
                            push(OpCode::Abs);
                        } else if let Some(f) = parse_float(val) {
                            push(OpCode::Push(Value::Float(f)));
                            push(OpCode::Abs);
                        } else {
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `abs` (requires an int or float)"
                                        .to_string(),
                                ),
                                pos: Position {
                                    line: line_no,
                                    col: _val_col,
                                },
                            });
                        }
                    }
                }
                "sign" => {
                    if arg.is_empty() {
                        push(OpCode::Sign);
                    } else {
                        let val = arg;
                        let _val_col = without_comment.find(val).map(|p| p + 1).unwrap_or(cmd_col);
                        if let Some(i) = parse_integer(val) {
                            push(OpCode::Push(Value::Int(i)));
                            push(OpCode::Sign);
                        } else if let Some(f) = parse_float(val) {
                            push(OpCode::Push(Value::Float(f)));
                            push(OpCode::Sign);
                        } else {
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `sign` (requires an int or float)"
                                        .to_string(),
                                ),
                                pos: Position {
                                    line: line_no,
                                    col: _val_col,
                                },
                            });
                        }
                    }
                }
                "ceil" => {
                    if arg.is_empty() {
                        push(OpCode::Ceil);
                    } else {
                        let val = arg;
                        let _val_col = without_comment.find(val).map(|p| p + 1).unwrap_or(cmd_col);
                        if let Some(i) = parse_integer(val) {
                            push(OpCode::Push(Value::Int(i)));
                            push(OpCode::Ceil);
                        } else if let Some(f) = parse_float(val) {
                            push(OpCode::Push(Value::Float(f)));
                            push(OpCode::Ceil);
                        } else {
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `ceil` (requires an int or float)"
                                        .to_string(),
                                ),
                                pos: Position {
                                    line: line_no,
                                    col: _val_col,
                                },
                            });
                        }
                    }
                }
                "floor" => {
                    if arg.is_empty() {
                        push(OpCode::Floor);
                    } else {
                        let val = arg;
                        let _val_col = without_comment.find(val).map(|p| p + 1).unwrap_or(cmd_col);
                        if let Some(i) = parse_integer(val) {
                            push(OpCode::Push(Value::Int(i)));
                            push(OpCode::Floor);
                        } else if let Some(f) = parse_float(val) {
                            push(OpCode::Push(Value::Float(f)));
                            push(OpCode::Floor);
                        } else {
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `floor` (requires an int or float)"
                                        .to_string(),
                                ),
                                pos: Position {
                                    line: line_no,
                                    col: _val_col,
                                },
                            });
                        }
                    }
                }
                "trunc" => {
                    if arg.is_empty() {
                        push(OpCode::Trunc);
                    } else {
                        let val = arg;
                        let _val_col = without_comment.find(val).map(|p| p + 1).unwrap_or(cmd_col);
                        if let Some(i) = parse_integer(val) {
                            push(OpCode::Push(Value::Int(i)));
                            push(OpCode::Trunc);
                        } else if let Some(f) = parse_float(val) {
                            push(OpCode::Push(Value::Float(f)));
                            push(OpCode::Trunc);
                        } else {
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `trunc` (requires an int or float)"
                                        .to_string(),
                                ),
                                pos: Position {
                                    line: line_no,
                                    col: _val_col,
                                },
                            });
                        }
                    }
                }
                "sqrt" => {
                    if arg.is_empty() {
                        // runtime pop
                        push(OpCode::Sqrt);
                    } else {
                        let val = arg;
                        let _val_col = without_comment.find(val).map(|p| p + 1).unwrap_or(cmd_col);
                        if let Some(i) = parse_integer(val) {
                            push(OpCode::Push(Value::Int(i)));
                            push(OpCode::Sqrt);
                        } else if let Some(f) = parse_float(val) {
                            push(OpCode::Push(Value::Float(f)));
                            push(OpCode::Sqrt);
                        } else {
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `sqrt` (requires an int or float)"
                                        .to_string(),
                                ),
                                pos: Position {
                                    line: line_no,
                                    col: _val_col,
                                },
                            });
                        }
                    }
                }
                "getarg" => {
                    if arg.is_empty() {
                        push(OpCode::GetArg);
                    } else if let Some(vals) = parse_inline_values(arg, Some(1)) {
                        if let Value::Int(i) = vals[0].clone() {
                            push(OpCode::Push(Value::Int(i)));
                            push(OpCode::GetArg);
                        } else {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `getarg` (requires a integer)"
                                        .to_string(),
                                ),
                                pos: Position { line: line_no, col },
                            });
                        }
                    }
                }
                "pow" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "sinh" | "cosh"
                | "tanh" | "asinh" | "acosh" | "atanh" | "exp" | "log" => {
                    if arg.is_empty() {
                        match cmd {
                            "pow" => push(OpCode::Pow),
                            "sin" => push(OpCode::Sin),
                            "cos" => push(OpCode::Cos),
                            "tan" => push(OpCode::Tan),
                            "asin" => push(OpCode::Asin),
                            "acos" => push(OpCode::Acos),
                            "atan" => push(OpCode::Atan),
                            "sinh" => push(OpCode::Sinh),
                            "cosh" => push(OpCode::Cosh),
                            "tanh" => push(OpCode::Tanh),
                            "asinh" => push(OpCode::Asinh),
                            "acosh" => push(OpCode::Acosh),
                            "atanh" => push(OpCode::Atanh),
                            "exp" => push(OpCode::Exp),
                            "log" => push(OpCode::Log),
                            _ => {}
                        }
                    } else {
                        let expected = if cmd == "pow" {
                            Some(2usize)
                        } else {
                            Some(1usize)
                        };
                        if let Some(vals) = parse_inline_values(arg, expected) {
                            if !vals
                                .iter()
                                .all(|v| matches!(v, Value::Int(_) | Value::Float(_)))
                            {
                                let col =
                                    without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                                errors.push(Error {
                                    file_name: file_name.to_string(),
                                    kind: ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `{}` (requires numeric argument(s))",
                                        cmd
                                    )),
                                    pos: Position { line: line_no, col },
                                });
                            } else {
                                for v in vals {
                                    push(OpCode::Push(v));
                                }
                                match cmd {
                                    "pow" => push(OpCode::Pow),
                                    "sin" => push(OpCode::Sin),
                                    "cos" => push(OpCode::Cos),
                                    "tan" => push(OpCode::Tan),
                                    "asin" => push(OpCode::Asin),
                                    "acos" => push(OpCode::Acos),
                                    "atan" => push(OpCode::Atan),
                                    "sinh" => push(OpCode::Sinh),
                                    "cosh" => push(OpCode::Cosh),
                                    "tanh" => push(OpCode::Tanh),
                                    "asinh" => push(OpCode::Asinh),
                                    "acosh" => push(OpCode::Acosh),
                                    "atanh" => push(OpCode::Atanh),
                                    "exp" => push(OpCode::Exp),
                                    "log" => push(OpCode::Log),
                                    _ => {}
                                }
                            }
                        }
                    }
                }
                "len" => {
                    if arg.is_empty() {
                        push(OpCode::Len);
                    } else if let Some(vals) = parse_inline_values(arg, Some(1)) {
                        if matches!(vals[0], Value::String(_)) {
                            push(OpCode::Push(vals[0].clone()));
                            push(OpCode::Len);
                        } else {
                            let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                            errors.push(Error {
                                file_name: file_name.to_string(),
                                kind: ErrorKind::InvalidArgument(
                                    "unexpected argument to `len` (requires a string)".to_string(),
                                ),
                                pos: Position { line: line_no, col },
                            });
                        }
                    }
                }
                "load" => {
                    if arg.is_empty() {
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `load` (requires a variable name)"
                                    .to_string(),
                            ),
                            pos: Position {
                                line: line_no,
                                col: cmd_col,
                            },
                        });
                    } else {
                        push(OpCode::Load(arg.to_string()));
                    }
                }
                "store" => {
                    if arg.is_empty() {
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `store` (requires a variable name)"
                                    .to_string(),
                            ),
                            pos: Position {
                                line: line_no,
                                col: cmd_col,
                            },
                        });
                    } else {
                        push(OpCode::Store(arg.to_string()));
                    }
                }
                "br" => {
                    if arg.is_empty() {
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `br` (requires a label)".to_string(),
                            ),
                            pos: Position {
                                line: line_no,
                                col: cmd_col,
                            },
                        });
                    } else {
                        push(OpCode::Br(arg.to_string()));
                    }
                }
                "assert" => {
                    if arg.is_empty() {
                        push(OpCode::Assert);
                    } else if arg.starts_with('"') && arg.ends_with('"') && arg.len() >= 2 {
                        let message = arg[1..arg.len() - 1].to_string();
                        push(OpCode::Push(Value::String(message)));
                        push(OpCode::Assert);
                    } else {
                        let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "assert message must be a string".to_string(),
                            ),
                            pos: Position { line: line_no, col },
                        });
                    }
                }
                "error" => {
                    if arg.is_empty() {
                        push(OpCode::Error);
                    } else if arg.starts_with('"') && arg.ends_with('"') && arg.len() >= 2 {
                        let message = arg[1..arg.len() - 1].to_string();
                        push(OpCode::Push(Value::String(message)));
                        push(OpCode::Error);
                    } else {
                        let col = without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "error message must be a string".to_string(),
                            ),
                            pos: Position { line: line_no, col },
                        });
                    }
                }
                "exit" => {
                    if arg.is_empty() {
                        push(OpCode::Exit);
                    } else {
                        match parse_integer(arg) {
                            Some(code) => {
                                push(OpCode::Push(Value::Int(code)));
                                push(OpCode::Exit);
                            }
                            None => {
                                let col =
                                    without_comment.find(arg).map(|p| p + 1).unwrap_or(cmd_col);
                                errors.push(Error {
                                    file_name: file_name.to_string(),
                                    kind: ErrorKind::InvalidArgument(
                                        "exit code must be an integer".to_string(),
                                    ),
                                    pos: Position { line: line_no, col },
                                });
                            }
                        }
                    }
                }
                "goto" => {
                    if arg.is_empty() {
                        errors.push(Error {
                            file_name: file_name.to_string(),
                            kind: ErrorKind::InvalidArgument(
                                "unexpected argument to `goto` (requires a label)".to_string(),
                            ),
                            pos: Position {
                                line: line_no,
                                col: cmd_col,
                            },
                        });
                    } else {
                        push(OpCode::Goto(arg.to_string()));
                    }
                }
                s if s.ends_with(':') => {
                    let label = s.trim_end_matches(':').to_string();
                    push(OpCode::Label(label));
                }
                _ => {
                    errors.push(Error {
                        file_name: file_name.to_string(),
                        kind: ErrorKind::UnknownCommand(cmd.to_string()),
                        pos: Position {
                            line: line_no,
                            col: cmd_col,
                        },
                    });
                }
            }
        }

        let mut labels = std::collections::HashMap::new();
        for (i, op) in program.iter().enumerate() {
            if let OpCode::Label(name) = op {
                labels.insert(name.clone(), i);
            }
        }

        if errors.is_empty() {
            Ok(Script {
                file_name: file_name.to_string(),
                program,
                labels,
                lines,
            })
        } else {
            Err(Errors { inner: errors })
        }
    }
}

pub struct Interpreter<'a> {
    stack: Vec<Value>,
    variables: HashMap<String, Value>,
    input: Option<Box<dyn BufRead + 'a>>,
    output: Option<Box<dyn Write + 'a>>,
    max_stack_size: Option<usize>,
    max_execution_time: Option<usize>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Interpreter {
            stack: Vec::new(),
            variables: HashMap::new(),
            input: None,
            output: None,
            max_execution_time: None,
            max_stack_size: None,
        }
    }

    pub fn stack(&self) -> &[Value] {
        &self.stack
    }

    pub fn with_stdio(mut self) -> Self {
        self.input = Some(Box::new(stdin().lock()));
        self.output = Some(Box::new(stdout()));
        self
    }

    pub fn with_input<I: BufRead + 'a>(mut self, input: I) -> Self {
        self.input = Some(Box::new(input));
        self
    }

    pub fn with_output<I: Write + 'a>(mut self, input: I) -> Self {
        self.output = Some(Box::new(input));
        self
    }

    pub fn with_max_execution_time(mut self, time: usize) -> Self {
        self.max_execution_time = Some(time);
        self
    }

    pub fn with_max_stack_size(mut self, size: usize) -> Self {
        self.max_stack_size = Some(size);
        self
    }

    pub fn run(&mut self, script: &Script, args: &[&str]) -> Result<(), Error> {
        let mut pc = 0;

        // start time for execution (ms since epoch); we'll check elapsed ms after each opcode
        let start_ms = Utc::now().timestamp_millis();

        // helper to build an Error using a pc -> source location (does not borrow self)
        let make_err = |kind: ErrorKind, pc: usize| -> Error {
            let line = if pc < script.lines.len() {
                script.lines[pc]
            } else {
                0
            };
            Error {
                file_name: script.file_name.clone(),
                kind,
                pos: Position { line, col: 1 },
            }
        };

        while pc < script.program.len() {
            // record pc for this opcode so we can report errors pointing to the executing op
            let current_pc = pc;
            let op = &script.program[pc];
            match op {
                &OpCode::Nop => {
                    pc += 1;
                }
                &OpCode::Push(ref val) => {
                    self.stack.push(val.clone());
                    pc += 1;
                }
                &OpCode::Pop(n) => {
                    if self.stack.len() >= n {
                        for _ in 0..n {
                            self.stack.pop();
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Add => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a + b)),
                            (Value::Float(a), Value::Float(b)) => {
                                self.stack.push(Value::Float(a + b))
                            }
                            (Value::Int(a), Value::Float(b)) => {
                                self.stack.push(Value::Float(a as f64 + b))
                            }
                            (Value::Float(a), Value::Int(b)) => {
                                self.stack.push(Value::Float(a + b as f64))
                            }
                            (Value::String(a), Value::String(b)) => {
                                self.stack.push(Value::String(a + &b));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "add",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Sub => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a - b)),
                            (Value::Float(a), Value::Float(b)) => {
                                self.stack.push(Value::Float(a - b))
                            }
                            (Value::Int(a), Value::Float(b)) => {
                                self.stack.push(Value::Float(a as f64 - b))
                            }
                            (Value::Float(a), Value::Int(b)) => {
                                self.stack.push(Value::Float(a - b as f64))
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "sub",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Mul => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a * b)),
                            (Value::Float(a), Value::Float(b)) => {
                                self.stack.push(Value::Float(a * b))
                            }
                            (Value::Int(a), Value::Float(b)) => {
                                self.stack.push(Value::Float(a as f64 * b))
                            }
                            (Value::Float(a), Value::Int(b)) => {
                                self.stack.push(Value::Float(a * b as f64))
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "mul",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Div => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a / b)),
                            (Value::Float(a), Value::Float(b)) => {
                                self.stack.push(Value::Float(a / b))
                            }
                            (Value::Int(a), Value::Float(b)) => {
                                self.stack.push(Value::Float(a as f64 / b))
                            }
                            (Value::Float(a), Value::Int(b)) => {
                                self.stack.push(Value::Float(a / b as f64))
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "div",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Mod => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        if let (Value::Int(a_val), Value::Int(b_val)) = (a, b) {
                            self.stack.push(Value::Int(a_val % b_val));
                        } else {
                            return Err(make_err(
                                ErrorKind::InvalidArgument(format!(
                                    "attempt to {} `{}` and `{}`",
                                    "mod",
                                    a_clone.type_name(),
                                    b_clone.type_name()
                                )),
                                pc,
                            ));
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Neg => {
                    if let Some(a) = self.stack.pop() {
                        match a {
                            Value::Int(val) => self.stack.push(Value::Int(-val)),
                            Value::Float(val) => self.stack.push(Value::Float(-val)),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "{}: type mismatch: {}",
                                        "neg",
                                        a.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Print => {
                    if self.output.is_none() {
                        return Err(make_err(
                            ErrorKind::CustomError("output is not set".to_string()),
                            pc,
                        ));
                    }

                    if let Some(a) = self.stack.pop() {
                        let output_line = match a {
                            Value::String(s) => format!("{}\n", s),
                            Value::Int(i) => format!("{}\n", i),
                            Value::Float(f) => format!("{}\n", f),
                            Value::Bool(b) => format!("{}\n", b),
                            Value::Nil => "nil\n".to_string(),
                        };
                        let o = self.output.as_mut().unwrap();
                        if let Err(e) = o.write_all(output_line.as_bytes()) {
                            return Err(make_err(ErrorKind::Io(e), pc));
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Read => {
                    if self.input.is_none() {
                        return Err(make_err(
                            ErrorKind::CustomError("input is not set".to_string()),
                            pc,
                        ));
                    }

                    let mut input = String::new();
                    let i = self.input.as_mut().unwrap();
                    match i.read_line(&mut input) {
                        Ok(_) => {
                            let trimmed = input.trim().to_string();
                            self.stack.push(Value::String(trimmed));
                        }
                        Err(e) => return Err(make_err(ErrorKind::Io(e), pc)),
                    }
                    pc += 1;
                }
                &OpCode::Br(ref label) => {
                    if let Some(value) = self.stack.pop() {
                        match value {
                            Value::Bool(true) => {
                                if let Some(&addr) = script.labels.get(label) {
                                    pc = addr;
                                } else {
                                    return Err(make_err(
                                        ErrorKind::LabelNotFound(label.clone()),
                                        pc,
                                    ));
                                }
                            }
                            Value::Bool(false) => {
                                pc += 1;
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "expected `bool`, found `{}`",
                                        value.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                }
                &OpCode::Load(ref var) => {
                    if let Some(value) = self.variables.get(var) {
                        self.stack.push(value.clone());
                    } else {
                        return Err(make_err(ErrorKind::VariableNotFound(var.clone()), pc));
                    }
                    pc += 1;
                }
                &OpCode::Store(ref var) => {
                    if let Some(value) = self.stack.pop() {
                        self.variables.insert(var.clone(), value);
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Goto(ref label) => {
                    if let Some(&addr) = script.labels.get(label) {
                        pc = addr;
                    } else {
                        return Err(make_err(ErrorKind::LabelNotFound(label.clone()), pc));
                    }
                }
                &OpCode::Label(_) => {
                    pc += 1;
                }
                &OpCode::Dup => {
                    if let Some(a) = self.stack.last().cloned() {
                        self.stack.push(a);
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Gt => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Int(a), Value::Int(b)) => {
                                self.stack.push(Value::Bool(a > b));
                            }
                            (Value::Float(a), Value::Float(b)) => {
                                self.stack.push(Value::Bool(a > b));
                            }
                            (Value::Int(a), Value::Float(b)) => {
                                self.stack.push(Value::Bool((a as f64) > b));
                            }
                            (Value::Float(a), Value::Int(b)) => {
                                self.stack.push(Value::Bool(a > (b as f64)));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "cannot compare `{}` and `{}`",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Lt => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Int(a), Value::Int(b)) => {
                                self.stack.push(Value::Bool(a < b));
                            }
                            (Value::Float(a), Value::Float(b)) => {
                                self.stack.push(Value::Bool(a < b));
                            }
                            (Value::Int(a), Value::Float(b)) => {
                                self.stack.push(Value::Bool((a as f64) < b));
                            }
                            (Value::Float(a), Value::Int(b)) => {
                                self.stack.push(Value::Bool(a < (b as f64)));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "cannot compare `{}` and `{}`",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Ge => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Int(a), Value::Int(b)) => {
                                self.stack.push(Value::Bool(a >= b));
                            }
                            (Value::Float(a), Value::Float(b)) => {
                                self.stack.push(Value::Bool(a >= b));
                            }
                            (Value::Int(a), Value::Float(b)) => {
                                self.stack.push(Value::Bool((a as f64) >= b));
                            }
                            (Value::Float(a), Value::Int(b)) => {
                                self.stack.push(Value::Bool(a >= (b as f64)));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "cannot compare `{}` and `{}`",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Le => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Int(a), Value::Int(b)) => {
                                self.stack.push(Value::Bool(a <= b));
                            }
                            (Value::Float(a), Value::Float(b)) => {
                                self.stack.push(Value::Bool(a <= b));
                            }
                            (Value::Int(a), Value::Float(b)) => {
                                self.stack.push(Value::Bool((a as f64) <= b));
                            }
                            (Value::Float(a), Value::Int(b)) => {
                                self.stack.push(Value::Bool(a <= (b as f64)));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "cannot compare `{}` and `{}`",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Eq => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let result = a == b;
                        self.stack.push(Value::Bool(result));
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Ne => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let result = a != b;
                        self.stack.push(Value::Bool(result));
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::And => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Bool(a), Value::Bool(b)) => {
                                self.stack.push(Value::Bool(a && b))
                            }
                            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a & b)),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "and",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Or => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Bool(a), Value::Bool(b)) => {
                                self.stack.push(Value::Bool(a || b))
                            }
                            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a | b)),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "or",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Not => {
                    if let Some(a) = self.stack.pop() {
                        match a {
                            Value::Bool(a) => self.stack.push(Value::Bool(!a)),
                            Value::Int(a) => self.stack.push(Value::Int(!a)),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "cannot apply not to `{}`",
                                        a.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Xor => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Bool(a), Value::Bool(b)) => self.stack.push(Value::Bool(a ^ b)),
                            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a ^ b)),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "xor",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Shl => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        if let (Value::Int(a), Value::Int(b)) = (a, b) {
                            self.stack.push(Value::Int(a << b));
                        } else {
                            return Err(make_err(
                                ErrorKind::InvalidArgument(format!(
                                    "attempt to {} `{}` and `{}`",
                                    "shl",
                                    a_clone.type_name(),
                                    b_clone.type_name()
                                )),
                                pc,
                            ));
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Shr => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        if let (Value::Int(a), Value::Int(b)) = (a, b) {
                            self.stack.push(Value::Int(a >> b));
                        } else {
                            return Err(make_err(
                                ErrorKind::InvalidArgument(format!(
                                    "attempt to {} `{}` and `{}`",
                                    "shr",
                                    a_clone.type_name(),
                                    b_clone.type_name()
                                )),
                                pc,
                            ));
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Convert(ref to) => {
                    if let Some(val) = self.stack.pop() {
                        let from = val.type_name();
                        let res = match (from, to.as_str(), val) {
                            // string -> int
                            ("string", "int", Value::String(s)) => match parse_integer(&s) {
                                Some(i) => Ok(Value::Int(i)),
                                None => Err(ErrorKind::InvalidArgument(format!(
                                    "cannot convert `{}` to `int`",
                                    s
                                ))),
                            },
                            // string -> float
                            ("string", "float", Value::String(s)) => match parse_float(&s) {
                                Some(f) => Ok(Value::Float(f)),
                                None => Err(ErrorKind::InvalidArgument(format!(
                                    "cannot convert `{}` to `float`",
                                    s
                                ))),
                            },
                            // int -> string
                            ("int", "string", Value::Int(i)) => Ok(Value::String(i.to_string())),
                            // float -> string
                            ("float", "string", Value::Float(f)) => {
                                Ok(Value::String(f.to_string()))
                            }
                            // int -> float
                            ("int", "float", Value::Int(i)) => Ok(Value::Float(i as f64)),
                            // float -> int (trunc toward zero)
                            ("float", "int", Value::Float(f)) => Ok(Value::Int(f as i64)),
                            // bool conversions
                            ("int", "bool", Value::Int(i)) => Ok(Value::Bool(i != 0)),
                            ("bool", "int", Value::Bool(b)) => {
                                Ok(Value::Int(if b { 1 } else { 0 }))
                            }
                            ("string", "bool", Value::String(s)) => match s.as_str() {
                                "true" => Ok(Value::Bool(true)),
                                "false" => Ok(Value::Bool(false)),
                                _ => Err(ErrorKind::InvalidArgument(format!(
                                    "cannot convert `{}` to `bool`",
                                    s
                                ))),
                            },
                            // identity when type matches
                            (f1, t1, v) if f1 == t1 => Ok(v),
                            // unsupported combinations
                            (_, _, v) => Err(ErrorKind::InvalidArgument(format!(
                                "cannot convert `{}` to `{}`",
                                v.type_name(),
                                to,
                            ))),
                        };

                        match res {
                            Ok(out) => self.stack.push(out),
                            Err(k) => return Err(make_err(k, pc)),
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Rotl => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        if let (Value::Int(a), Value::Int(b)) = (a, b) {
                            let ua = a as u64;
                            let ub = (b as i64).rem_euclid(64) as u32;
                            let res = ua.rotate_left(ub) as i64;
                            self.stack.push(Value::Int(res));
                        } else {
                            return Err(make_err(
                                ErrorKind::InvalidArgument(format!(
                                    "cannot rotl `{}` and `{}`",
                                    a_clone.type_name(),
                                    b_clone.type_name()
                                )),
                                pc,
                            ));
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Rotr => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        if let (Value::Int(a), Value::Int(b)) = (a, b) {
                            let ua = a as u64;
                            let ub = (b as i64).rem_euclid(64) as u32;
                            let res = ua.rotate_right(ub) as i64;
                            self.stack.push(Value::Int(res));
                        } else {
                            return Err(make_err(
                                ErrorKind::InvalidArgument(format!(
                                    "cannot rotr `{}` and `{}`",
                                    a_clone.type_name(),
                                    b_clone.type_name()
                                )),
                                pc,
                            ));
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Clz => {
                    if let Some(value) = self.stack.pop() {
                        match value {
                            Value::Int(i) => {
                                let u = (i as u64).leading_zeros() as i64;
                                self.stack.push(Value::Int(u));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `clz` ({})",
                                        value.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Ctz => {
                    if let Some(value) = self.stack.pop() {
                        match value {
                            Value::Int(i) => {
                                let u = (i as u64).trailing_zeros() as i64;
                                self.stack.push(Value::Int(u));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `ctz` ({})",
                                        value.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Min => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        match (a, b) {
                            (Value::Int(ai), Value::Int(bi)) => {
                                self.stack.push(Value::Int(std::cmp::min(ai, bi)));
                            }
                            (Value::Float(af), Value::Float(bf)) => {
                                self.stack.push(Value::Float(af.min(bf)));
                            }
                            (Value::Int(ai), Value::Float(bf)) => {
                                self.stack.push(Value::Float((ai as f64).min(bf)));
                            }
                            (Value::Float(af), Value::Int(bi)) => {
                                self.stack.push(Value::Float(af.min(bi as f64)));
                            }
                            (a_clone, b_clone) => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "min",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Max => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        match (a, b) {
                            (Value::Int(ai), Value::Int(bi)) => {
                                self.stack.push(Value::Int(std::cmp::max(ai, bi)));
                            }
                            (Value::Float(af), Value::Float(bf)) => {
                                self.stack.push(Value::Float(af.max(bf)));
                            }
                            (Value::Int(ai), Value::Float(bf)) => {
                                self.stack.push(Value::Float((ai as f64).max(bf)));
                            }
                            (Value::Float(af), Value::Int(bi)) => {
                                self.stack.push(Value::Float(af.max(bi as f64)));
                            }
                            (a_clone, b_clone) => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "max",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Abs => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Int(i) => self.stack.push(Value::Int(i.abs())),
                            Value::Float(f) => self.stack.push(Value::Float(f.abs())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `abs` ({})",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Sign => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Int(i) => {
                                let s = if i > 0 {
                                    1
                                } else if i < 0 {
                                    -1
                                } else {
                                    0
                                };
                                self.stack.push(Value::Int(s));
                            }
                            Value::Float(f) => {
                                let s = if f > 0.0 {
                                    1
                                } else if f < 0.0 {
                                    -1
                                } else {
                                    0
                                };
                                self.stack.push(Value::Int(s));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `sign` ({})",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Ceil => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.ceil())),
                            Value::Int(i) => self.stack.push(Value::Int(i)),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `ceil` ({})",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Floor => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.floor())),
                            Value::Int(i) => self.stack.push(Value::Int(i)),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `floor` ({})",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Trunc => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.trunc())),
                            Value::Int(i) => self.stack.push(Value::Int(i)),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `trunc` ({})",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Sqrt => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.sqrt())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).sqrt())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `sqrt` ({})",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Len => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::String(s) => self.stack.push(Value::Int(s.len() as i64)),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument to `len` ({})",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Pow => {
                    if self.stack.len() >= 2 {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let a_clone = a.clone();
                        let b_clone = b.clone();
                        match (a, b) {
                            (Value::Int(ai), Value::Int(bi)) => {
                                self.stack.push(Value::Float((ai as f64).powf(bi as f64)))
                            }
                            (Value::Float(af), Value::Float(bf)) => {
                                self.stack.push(Value::Float(af.powf(bf)))
                            }
                            (Value::Int(ai), Value::Float(bf)) => {
                                self.stack.push(Value::Float((ai as f64).powf(bf)))
                            }
                            (Value::Float(af), Value::Int(bi)) => {
                                self.stack.push(Value::Float(af.powf(bi as f64)))
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "attempt to {} `{}` and `{}`",
                                        "pow",
                                        a_clone.type_name(),
                                        b_clone.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Sin => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.sin())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).sin())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Cos => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.cos())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).cos())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Tan => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.tan())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).tan())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Asin => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.asin())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).asin())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Acos => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.acos())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).acos())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Atan => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.atan())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).atan())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Sinh => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.sinh())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).sinh())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Cosh => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.cosh())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).cosh())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Tanh => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.tanh())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).tanh())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Asinh => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.asinh())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).asinh())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Acosh => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.acosh())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).acosh())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Atanh => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.atanh())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).atanh())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Exp => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.exp())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).exp())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Log => {
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Float(f) => self.stack.push(Value::Float(f.ln())),
                            Value::Int(i) => self.stack.push(Value::Float((i as f64).ln())),
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::GetArg => {
                    // pop top of stack for index or numeric string
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Int(i) => {
                                if i < 0 {
                                    return Err(make_err(
                                        ErrorKind::InvalidArgument(format!(
                                            "invalid argument #2: expected `{}`, found `{}`",
                                            "non-negative index", "negative index"
                                        )),
                                        pc,
                                    ));
                                }
                                let idx = i as usize;
                                if idx < args.len() {
                                    self.stack.push(Value::String(args[idx].to_string()));
                                } else {
                                    self.stack.push(Value::Nil);
                                }
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "unexpected argument of type `{}`",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Assert => {
                    // Assert supports two forms on the stack:
                    // - top is a string -> treated as message, next is the condition
                    // - top is the condition value directly
                    if let Some(v) = self.stack.pop() {
                        let (cond_val, msg_opt) = match v {
                            Value::String(s) => {
                                if let Some(cond) = self.stack.pop() {
                                    (cond, Some(s))
                                } else {
                                    return Err(make_err(ErrorKind::StackUnderflow, pc));
                                }
                            }
                            other => (other, None),
                        };

                        let is_true = match cond_val {
                            Value::Bool(b) => b,
                            Value::Int(i) => i != 0,
                            Value::Float(f) => f != 0.0,
                            Value::String(s) => !s.is_empty(),
                            Value::Nil => false,
                        };

                        if !is_true {
                            let msg = msg_opt.unwrap_or_else(|| "false".to_string());
                            return Err(make_err(ErrorKind::AssertionFailed(msg), pc));
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                    pc += 1;
                }
                &OpCode::Error => {
                    // message must be provided either inline (parser pushed it) or on the stack
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::String(s) => {
                                return Err(make_err(ErrorKind::CustomError(s), pc));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "error message must be string, found {}",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                }
                &OpCode::Exit => {
                    // exit code must be provided inline (parser pushed it) or on the stack
                    if let Some(v) = self.stack.pop() {
                        match v {
                            Value::Int(i) => {
                                return Err(make_err(ErrorKind::Exit(i), pc));
                            }
                            _ => {
                                return Err(make_err(
                                    ErrorKind::InvalidArgument(format!(
                                        "exit code must be integer, found {}",
                                        v.type_name()
                                    )),
                                    pc,
                                ));
                            }
                        }
                    } else {
                        return Err(make_err(ErrorKind::StackUnderflow, pc));
                    }
                }
            }

            // after each opcode execution check elapsed time (ms) and stack size
            if let Some(max_execution_time) = self.max_execution_time {
                if (Utc::now().timestamp_millis() - start_ms) as usize > max_execution_time {
                    return Err(make_err(ErrorKind::Timeout, current_pc));
                }
            }

            if let Some(max_stack_size) = self.max_stack_size {
                if self.stack.len() > max_stack_size {
                    return Err(make_err(ErrorKind::StackOverflow, current_pc));
                }
            }
        }
        Ok(())
    }
}
