use super::parse::ast;
use super::Value;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Write;
use std::process::Command;

const GENERATED_SOURCE_PRELUDE: &'static str = "extern crate mold;";

fn rust_signature(signature: &ast::FunctionSignature) -> String {
    let mut result = String::new();
    write!(&mut result, "fn {}(", signature.name.as_str()).unwrap();

    let mut first = true;
    for arg in &signature.args {
        if first {
            write!(&mut result, "{}: mold::Value", arg.as_str()).unwrap();
        } else {
            write!(&mut result, ", {}: mold::Value", arg.as_str()).unwrap();
        }

        first = false;
    }

    write!(&mut result, ") -> mold::Value").unwrap();

    result
}

#[derive(Debug, PartialEq, Clone)]
pub struct DynamicNativeFunction {
    pub dylib_path: std::path::PathBuf,
    pub symbol_name: String,
}

pub struct StaticNativeFunction1 {
    pub function: fn(Value) -> Value,
}

// Unfortunately, this stuff needs to be manual:
// https://github.com/rust-lang/rust/issues/54508
impl Debug for StaticNativeFunction1 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "StaticNativeFunction1(0x{:x})", self.function as usize)
    }
}

impl PartialEq for StaticNativeFunction1 {
    fn eq(&self, rhs: &Self) -> bool {
        self.function as usize == rhs.function as usize
    }
}

impl Clone for StaticNativeFunction1 {
    fn clone(&self) -> Self {
        StaticNativeFunction1 {
            function: self.function.clone(),
        }
    }
}

pub struct StaticNativeFunction2 {
    pub function: for<'a> fn(Value<'a>, Value<'a>) -> Value<'a>,
}

impl Debug for StaticNativeFunction2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "StaticNativeFunction2(0x{:x})", self.function as usize)
    }
}

impl PartialEq for StaticNativeFunction2 {
    fn eq(&self, rhs: &Self) -> bool {
        self.function as usize == rhs.function as usize
    }
}

impl Clone for StaticNativeFunction2 {
    fn clone(&self) -> Self {
        StaticNativeFunction2 {
            function: self.function.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum NativeFunction {
    Dynamic(DynamicNativeFunction),
    Static1(StaticNativeFunction1),
    Static2(StaticNativeFunction2),
}

pub fn external_eval_1<'a>(function: &DynamicNativeFunction, arg1: Value<'a>) -> Value<'a> {
    use dylib::DynamicLibrary;

    DynamicLibrary::prepend_search_path(std::path::Path::new(
        "/Users/john/code/mold/target/debug/deps",
    ));
    match DynamicLibrary::open(Some(&function.dylib_path)) {
        Err(e) => panic!("Error opening dylib: {}", e),
        Ok(lib) => {
            let func: fn(Value<'a>) -> Value<'a> = unsafe {
                match lib.symbol::<u8>(function.symbol_name.as_str()) {
                    Err(e) => panic!("Could not find symbol: {}", e),
                    Ok(f) => std::mem::transmute(f),
                }
            };
            func(arg1)
        }
    }
}

fn generate_rust_source(f: &ast::RustFunctionDefinition) -> String {
    let signature = rust_signature(&f.signature);
    format!(
        "{}\n#[no_mangle]\n{}{{ {} }}",
        GENERATED_SOURCE_PRELUDE, signature, f.body
    )
}

#[derive(Debug, PartialEq)]
pub struct ExternCompileError {
    pub message: String,
}

pub fn compile_function(
    f: &ast::RustFunctionDefinition,
) -> Result<NativeFunction, ExternCompileError> {
    let source = generate_rust_source(f);

    println!("Compiling function:\n {}", source);

    let dir = std::env::temp_dir();
    let file_path = dir.with_file_name(format!("{}.rs", f.signature.name));
    let dylib_path = dir.with_file_name(format!("{}.dylib", f.signature.name));
    println!("To path: {}", file_path.to_str().unwrap());
    std::fs::write(&file_path, source.as_str()).expect("Unable to write file");

    //--extern mold=target/debug/libmold.rlib --crate-type dylib
    let result = Command::new("rustc")
        .args(&[
            file_path.to_str().unwrap(),
            "--extern",
            "mold=target/debug/libmold.rlib",
            "-L",
            "target/debug/deps",
            "--crate-type",
            "dylib",
            "-o",
            dylib_path.to_str().unwrap(),
        ])
        .output();

    match result {
        Ok(output) => {
            println!("{}", String::from_utf8(output.stdout).unwrap());

            if !output.status.success() {
                eprintln!("{}", String::from_utf8(output.stderr).unwrap());
                return Err(ExternCompileError {
                    message: String::from("Compilation of rust fn failed"),
                });
            }

            Ok(NativeFunction::Dynamic(DynamicNativeFunction {
                dylib_path: dylib_path,
                symbol_name: f.signature.name.clone(),
            }))
        }
        Err(e) => {
            eprintln!("{}", e);
            Err(ExternCompileError {
                message: String::from("Compilation of rust fn failed"),
            })
        }
    }
}
