// Copyright (c) 2017 King's College London
// created by the Software Development Team <http://soft-dev.org/>
//
// The Universal Permissive License (UPL), Version 1.0
//
// Subject to the condition set forth below, permission is hereby granted to any
// person obtaining a copy of this software, associated documentation and/or
// data (collectively the "Software"), free of charge and under any and all
// copyright rights in the Software, and any and all patent rights owned or
// freely licensable by each licensor hereunder covering either (i) the
// unmodified Software as contributed to or provided by such licensor, or (ii)
// the Larger Works (as defined below), to deal in both
//
// (a) the Software, and
// (b) any piece of software and/or hardware listed in the lrgrwrks.txt file //
// if one is included with the Software (each a “Larger Work” to which the Software
// is contributed by such licensors),
//
// without restriction, including without limitation the rights to copy, create
// derivative works of, display, perform, and distribute the Software and make,
// use, sell, offer for sale, import, export, have made, and have sold the
// Software and the Larger Work(s), and to sublicense the foregoing rights on
// either these or other terms.
//
// This license is subject to the following condition: The above copyright
// notice and either this complete permission notice or at a minimum a reference
// to the UPL must be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#![warn(missing_docs)]

use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::Path;

use lrlex::{build_lex, Lexeme};
use lrpar::parser;
use lrtable::{Grammar, Minimiser, TIdx, yacc_to_statetable};

/// Errors raised when parsing a source file.
#[derive(Debug)]
pub enum ParseError {
    /// Io error returned from std library routine.
    Io(io::Error),
    /// File not found.
    FileNotFound,
    /// File name had no extension (used to determine which lexer/parser to use).
    NoFileExtension,
    /// Lexer could not be built by `lrlex`.
    BrokenLexer,
    /// Parser could not be built by `lrpar`.
    BrokenParser,
    /// File contained lexical error and could not be lexed.
    LexicalError,
    /// File contained syntax error and could not be parsed.
    SyntaxError,
}

// Read file and return its contents.
fn read_file(path: &str) -> Result<String, ParseError> {
    let mut f = match File::open(path) {
        Ok(r) => r,
        Err(e) => return Err(ParseError::Io(e)),
    };
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    Ok(s)
}

// Pretty print AST (place-holder for later code).
fn pretty_print(pt: &parser::Node<u16>, grm: &Grammar, input: &str) -> String {
    let mut st = vec![(0, pt)]; // Stack of (indent level, node) pairs
    let mut s = String::new();
    while !st.is_empty() {
        let (indent, e) = st.pop().unwrap();
        for _ in 0..indent {
            s.push_str(" ");
        }
        match e {
            &parser::Node::Terminal { lexeme } => {
                let tid: usize = lexeme.tok_id().try_into().ok().unwrap();
                let tn = grm.term_name(TIdx::from(tid)).unwrap();
                let lt = &input[lexeme.start()..lexeme.start() + lexeme.len()];
                s.push_str(&format!("{} {}\n", tn, lt));
            }
            &parser::Node::Nonterminal {
                 nonterm_idx,
                 ref nodes,
             } => {
                s.push_str(&format!("{}\n", grm.nonterm_name(nonterm_idx).unwrap()));
                for x in nodes.iter().rev() {
                    st.push((indent + 1, x));
                }
            }
        }
    }
    s
}

/// Parse an individual input file, and return an lrpar::parser::Node or
/// [ParseError](enum.ParseError.html).
///
/// In the near future this function will return a custom tree type (or error).
pub fn parse_file(input_path: &str) -> Result<parser::Node<u16>, ParseError> {
    // Determine lexer and yacc files by extension. For example if the input
    // file is named Foo.java, the lexer should be grammars/java.l.
    // TODO: create a HashMap of file extensions -> lex/yacc files.
    let extension = match Path::new(&input_path).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => return Err(ParseError::NoFileExtension),
    };
    let lex_l_path = format!("grammars/{}.l", extension);
    let yacc_y_path = format!("grammars/{}.y", extension);
    info!("Using lexer: {} and parser: {} for input: {}",
          &lex_l_path,
          &yacc_y_path,
          input_path);
    if !Path::new(&lex_l_path).exists() || !Path::new(&yacc_y_path).exists() {
        return Err(ParseError::FileNotFound);
    }

    // Get input files.
    let lexs = match read_file(&lex_l_path) {
        Ok(string) => string,
        Err(err) => return Err(err),
    };
    let grms = match read_file(&yacc_y_path) {
        Ok(string) => string,
        Err(err) => return Err(err),
    };
    let input = match read_file(input_path) {
        Ok(string) => string,
        Err(err) => return Err(err),
    };

    // Create lexer.
    let mut lexer = match build_lex::<u16>(&lexs) {
        Ok(ast) => ast,
        Err(_) => return Err(ParseError::BrokenLexer),
    };

    // Create parser.
    let (grm, stable) = match yacc_to_statetable(&grms, Minimiser::Pager) {
        Ok(x) => x,
        Err(_) => return Err(ParseError::BrokenParser),
    };

    // Sync up the IDs of terminals in the lexer and parser.
    let mut rule_ids = HashMap::<&str, u16>::new();
    for term_idx in grm.iter_term_idxs() {
        rule_ids.insert(grm.term_name(term_idx).unwrap(),
                        u16::try_from(usize::from(term_idx)).unwrap());
    }
    lexer.set_rule_ids(&rule_ids);

    // Lex input file.
    let mut lexemes = match lexer.lex(&input) {
        Ok(tokens) => tokens,
        Err(_) => return Err(ParseError::LexicalError),
    };
    lexemes.push(Lexeme::new(u16::try_from(usize::from(grm.end_term)).unwrap(),
                             input.len(),
                             0));

    // Return parse tree.
    let pt = match parser::parse::<u16>(&grm, &stable, &lexemes) {
        Ok(tree) => tree,
        Err(_) => return Err(ParseError::SyntaxError),
    };
    println!("{}", pretty_print(&pt, &grm, &input));
    Ok(pt)
}
