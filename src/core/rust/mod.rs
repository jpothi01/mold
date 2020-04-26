use super::eval;
use super::parse::ast;
use std::fmt::Write;
use std::io;
use std::process::Command;
use std::process::Output;

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

    write!(&mut result, ")").unwrap();

    result
}

pub struct ExternFunction {
    pub dylib_path: std::path::PathBuf,
    pub function_symbol: String,
}

pub fn external_eval<'a>(function: ExternFunction) -> eval::Value<'a> {
    use dylib::DynamicLibrary;

    // DynamicLibrary::prepend_search_path(std::path::Path::new("/Users/john/code/"));
    match DynamicLibrary::open(Some(&function.dylib_path)) {
        Err(e) => panic!("Error opening dylib: {}", e),
        Ok(lib) => {
            let func: fn() -> () = unsafe {
                match lib.symbol::<u8>("print") {
                    Err(e) => panic!("Could not find symbol: {}", e),
                    Ok(f) => std::mem::transmute(f),
                }
            };
            func();
            eval::Value::Unit(eval::types::Unit {})
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ExternCompileError {
    pub message: String,
}

pub fn compile_function(
    f: &ast::RustFunctionDefinition,
) -> Result<ExternFunction, ExternCompileError> {
    let signature = rust_signature(&f.signature);
    let function = format!("#[no_mangle]\n{}{{ {} }}", signature, f.body);

    println!("Compiling function: {}", function);

    let dir = std::env::temp_dir();
    let file_path = dir.with_file_name("temp.rs");
    let dylib_path = dir.with_file_name("temp.dylib");
    println!("To path: {}", file_path.to_str().unwrap());
    std::fs::write(&file_path, function.as_str()).expect("Unable to write file");

    //--extern mold=target/debug/libmold.rlib --crate-type dylib
    let result = Command::new("rustc")
        .args(&[
            file_path.to_str().unwrap(),
            "--extern",
            "mold=target/debug/libmold.rlib",
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
                panic!();
            }

            Ok(ExternFunction {
                dylib_path: dylib_path,
                function_symbol: f.signature.name.clone(),
            })
        }
        Err(e) => {
            eprintln!("{}", e);
            panic!();
        }
    }
}
