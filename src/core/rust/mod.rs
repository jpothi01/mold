use super::eval;
use super::parse::ast;
use super::Value;
use std::fmt::Write;
use std::io;
use std::process::Command;
use std::process::Output;

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
pub struct ExternFunction {
    pub dylib_path: std::path::PathBuf,
    pub symbol_name: String,
}

pub fn external_eval_0<'a>(function: &ExternFunction) -> eval::Value<'a> {
    use dylib::DynamicLibrary;

    DynamicLibrary::prepend_search_path(std::path::Path::new(
        "/Users/john/code/mold/target/debug/deps",
    ));
    match DynamicLibrary::open(Some(&function.dylib_path)) {
        Err(e) => panic!("Error opening dylib: {}", e),
        Ok(lib) => {
            let func: fn() -> Value<'a> = unsafe {
                match lib.symbol::<u8>(function.symbol_name.as_str()) {
                    Err(e) => panic!("Could not find symbol: {}", e),
                    Ok(f) => std::mem::transmute(f),
                }
            };
            func()
        }
    }
}

pub fn external_eval_1<'a>(function: &ExternFunction, arg1: Value<'a>) -> Value<'a> {
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
) -> Result<ExternFunction, ExternCompileError> {
    let source = generate_rust_source(f);

    println!("Compiling function:\n {}", source);

    let dir = std::env::temp_dir();
    let file_path = dir.with_file_name("temp.rs");
    let dylib_path = dir.with_file_name("temp.dylib");
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

            Ok(ExternFunction {
                dylib_path: dylib_path,
                symbol_name: f.signature.name.clone(),
            })
        }
        Err(e) => {
            eprintln!("{}", e);
            Err(ExternCompileError {
                message: String::from("Compilation of rust fn failed"),
            })
        }
    }
}
