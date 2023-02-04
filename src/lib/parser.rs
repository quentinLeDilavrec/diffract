// Copyright (c) 2018 King's College London
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
// (b) any piece of software and/or hardware listed in the lrgrwrks.txt file
// if one is included with the Software (each a "Larger Work" to which the Software
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

use std::convert::TryFrom;
use std::fs::{canonicalize, File};
use std::io::{stderr, Read};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use cfgrammar::yacc::ast::ASTWithValidityInfo;
use cfgrammar::yacc::{YaccGrammar, YaccKind, YaccOriginalActionKind};
use cfgrammar::{NewlineCache, Spanned, TIdx};
use lrlex::{
    lrlex_mod, CTLexerBuilder, DefaultLexeme, DefaultLexerTypes, LRNonStreamingLexerDef, LexerDef
};
use lrpar::{
    lrpar_mod, parser, LexParseError, Lexeme, Lexer, LexerTypes, NonStreamingLexer,
    RTParserBuilder, RecoveryKind
};
use lrtable::{from_yacc, Minimiser};

use crate::ast::{Arena, NodeId};

/// Needed for grmtools: must be big enough to index (separately) all nonterminals, productions,
/// symbols (in productions) and terminals.
type StorageT = u32;

quick_error! {
    /// Errors raised when parsing a source file.
    #[allow(missing_docs)]
    #[derive(Debug)]
    pub enum ParseError {
        Io(path: String) {
            description("Could not create a new file.")
            display(r#"File "{}" could not be created."#, path)
        }
        FileNotFound(path: String) {
            description("Could not find file.")
            display(r#"Could not find file: "{}"."#, path)
        }
        BrokenLexer {
            description("Lexer could not be built by lrlex.")
            display(r#"Lexer could not be built by lrlex."#)
        }
        BrokenParser {
            description("Parser could not be built by lrpar.")
            display(r#"Parser could not be built by lrpar."#)
        }
        LexicalError(path: String) {
            description("File contained lexical error.")
            display(r#"File "{}" contains a lexical error."#, path)
        }
        SyntaxError(path: String) {
            description("File contains a syntax error.")
            display(r#"File "{}" contains a syntax error."#, path)
        }
    }
}

pub enum Parsers {
    CALC,
    JAVA,
    TXT
}

/// Given a filename (with extension), return a suitable lex file.
pub fn get_lexer(path: &str) -> Box<dyn LexerDef<DefaultLexerTypes>> {
    let ext = match Path::new(&path).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => ".txt"
    };
    match ext {
        "calc" => Box::new(calc_l::lexerdef()),
        "java" => Box::new(calc_l::lexerdef()),
        "txt" => Box::new(calc_l::lexerdef()),
        // "java" | "calc" => canonicalize(format!("grammars/{}.l", ext)).unwrap(),
        // _ => canonicalize("grammars/txt.l".to_string()).unwrap()
        _ => unimplemented!()
    }
}
/// Given a filename (with extension), return a suitable yacc file.
pub fn get_parser(path: &str) -> Parsers {
    let ext = match Path::new(&path).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => ".txt"
    };
    match ext {
        "calc" => Parsers::CALC,
        "java" => Parsers::JAVA,
        "txt" => Parsers::TXT,
        // "java" | "calc" => canonicalize(format!("grammars/{}.y", ext)).unwrap(),
        // _ => canonicalize("grammars/txt.y".to_string()).unwrap()
        _ => unimplemented!()
    }
}

// Read file and return its contents or `ParseError`.
fn read_file(path: &Path) -> Result<String, ParseError> {
    if !Path::new(path).exists() {
        return Err(ParseError::FileNotFound(path.to_str().unwrap().into()));
    }
    let mut f = File::open(path).map_err(|e| ParseError::Io(e.to_string()))?;
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    Ok(s)
}

lrlex_mod!("grammars/calc.l");
// Using `lrpar_mod!` brings the parser for `calc.y` into scope.
lrpar_mod!("grammars/calc.y");

// fn read_file(path: &str) -> String {
//     let mut f = match File::open(path) {
//         Ok(r) => r,
//         Err(e) => {
//             writeln!(stderr(), "Can't open file {}: {}", path, e).ok();
//             process::exit(1);
//         }
//     };
//     let mut s = String::new();
//     f.read_to_string(&mut s).unwrap();
//     s
// }
/// Parse a string, and return an `Arena` or `ParseError`.
pub fn parse_string<T: PartialEq + Copy>(input: &str,
                                         input_path: &str,
                                         parser: &Parsers)
                                         -> Result<Arena<String, T>, ParseError> {
    let lex_l_path = Path::join(Path::new(env!("CARGO_MANIFEST_DIR")),
                                Path::new(match parser {
                                              Parsers::CALC => "src/grammars/calc.l",
                                              Parsers::JAVA => "src/grammars/java.l",
                                              Parsers::TXT => "src/grammars/txt.l",
                                              _ => panic!()
                                          }));
    let recoverykind = RecoveryKind::None;

    let yacckind = YaccKind::Eco; //::Original(YaccOriginalActionKind::GenericParseTree);

    let lex_l_path = lex_l_path.as_path();
    let lex_src = read_file(lex_l_path).unwrap();
    use std::io::Write;
    let spanned_fmt = |spanned: &dyn Spanned, nlcache: &NewlineCache, src, src_path, prefix| {
        if let Some((line, column)) =
            nlcache.byte_to_line_num_and_col_num(src, spanned.spans()[0].start())
        {
            writeln!(stderr(),
                     "{:?}: {prefix} {} at line {line} column {column}",
                     src_path,
                     &spanned).ok();
        } else {
            writeln!(stderr(), "{:?}: {}", &src_path, &spanned).ok();
        }
    };
    let mut lexerdef = match LRNonStreamingLexerDef::<DefaultLexerTypes<u32>>::from_str(&lex_src) {
        Ok(ast) => ast,
        Err(errs) => {
            let nlcache = NewlineCache::from_str(&lex_src).unwrap();
            for e in errs {
                spanned_fmt(&e, &nlcache, &lex_src, lex_l_path, "[ERROR]");
            }
            panic!()
        }
    };
    let yacc_y_path = Path::join(Path::new(env!("CARGO_MANIFEST_DIR")),
                                 Path::new(match parser {
                                               Parsers::CALC => "src/grammars/calc.y",
                                               Parsers::JAVA => "src/grammars/java.y",
                                               Parsers::TXT => "src/grammars/txt.y",
                                               _ => panic!()
                                           }));

    let yacc_y_path = yacc_y_path.as_path();
    let yacc_src = read_file(yacc_y_path).unwrap();
    let ast_validation = ASTWithValidityInfo::new(yacckind, &yacc_src);
    let warnings = ast_validation.ast().warnings();
    let res = YaccGrammar::new_from_ast_with_validity_info(yacckind, &ast_validation);
    let grm = match res {
        Ok(x) => {
            if !warnings.is_empty() {
                let nlcache = NewlineCache::from_str(&yacc_src).unwrap();
                for w in warnings {
                    spanned_fmt(&w, &nlcache, &yacc_src, yacc_y_path, "[WARNING]");
                }
            }
            x
        }
        Err(errs) => {
            let nlcache = NewlineCache::from_str(&yacc_src).unwrap();
            for e in errs {
                spanned_fmt(&e, &nlcache, &yacc_src, yacc_y_path, "[ERROR]");
            }
            for w in warnings {
                spanned_fmt(&w, &nlcache, &yacc_src, yacc_y_path, "[WARNING]");
            }
            // return Err(ParseError::SyntaxError(input_path.to_string()));
            panic!()
        }
    };

    let (sgraph, stable) = match from_yacc(&grm, Minimiser::Pager) {
        Ok(x) => x,
        Err(s) => {
            writeln!(stderr(), "{:?}: {}", &yacc_y_path, &s).ok();
            panic!();
        }
    };

    if let Some(c) = stable.conflicts() {
        let pp_rr = if let Some(i) = grm.expectrr() {
            i != c.rr_len()
        } else {
            0 != c.rr_len()
        };
        let pp_sr = if let Some(i) = grm.expect() {
            i != c.sr_len()
        } else {
            0 != c.sr_len()
        };
        if pp_rr {
            println!("{}", c.pp_rr(&grm));
        }
        if pp_sr {
            println!("{}", c.pp_sr(&grm));
        }
        if pp_rr || pp_sr {
            println!("Stategraph:\n{}\n", sgraph.pp_core_states(&grm));
        }
    }

    {
        let rule_ids = grm.tokens_map().iter().map(|(&n, &i)| (n, i.0)).collect();
        let (missing_from_lexer, missing_from_parser) = lexerdef.set_rule_ids(&rule_ids);
        if let Some(tokens) = missing_from_parser {
            writeln!(stderr(), "Warning: these tokens are defined in the lexer but not referenced in the\ngrammar:").ok();
            let mut sorted = tokens.iter().cloned().collect::<Vec<&str>>();
            sorted.sort_unstable();
            for n in sorted {
                writeln!(stderr(), "  {}", n).ok();
            }
        }
        if let Some(tokens) = missing_from_lexer {
            writeln!(
                                stderr(),
                                "Error: these tokens are referenced in the grammar but not defined in the lexer:"
                            )
                            .ok();
            let mut sorted = tokens.iter().cloned().collect::<Vec<&str>>();
            sorted.sort_unstable();
            for n in sorted {
                writeln!(stderr(), "  {}", n).ok();
            }
            panic!();
        }
    }
    let lexer = lexerdef.lexer(&input);
    let pb = RTParserBuilder::new(&grm, &stable).recoverer(recoverykind);
    let (pt, errs) = pb.parse_generictree(&lexer);
    if !errs.is_empty() {
        for err in errs {
            println!("{}", &err);
            return match err {
                LexParseError::LexError(_) => Err(ParseError::LexicalError(input_path.to_string())),
                LexParseError::ParseError(_) => Err(ParseError::SyntaxError(input_path.to_string()))
            };
        }
    }
    let pt = pt.ok_or_else(|| ParseError::SyntaxError(input_path.to_string()))?;
    Ok(parse_into_ast::<T>(&pt, &lexer, &grm, input))
}

/// Parse an individual input file, and return an `Arena` or `ParseError`.
pub fn parse_file<T: PartialEq + Copy>(input_path: &str,
                                       parser: &Parsers)
                                       -> Result<Arena<String, T>, ParseError> {
    let input = read_file(Path::new(input_path))?;
    parse_string(&input, &input_path, parser)
}

// Turn a grammar, parser and input string into an AST arena.
fn parse_into_ast<T: PartialEq + Copy>(pt: &parser::Node<DefaultLexeme, StorageT>,
                                       lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
                                       grm: &YaccGrammar,
                                       input: &str)
                                       -> Arena<String, T> {
    let mut arena = Arena::new();
    let mut st = vec![pt]; // Stack of nodes.
                           // Stack of `Option<NodeId>`s which are parents of nodes on the `st` stack.
                           // The stack should never be empty, a `None` should be at the bottom of the stack.
    let mut parent = vec![None];
    let mut child_node: NodeId<T>;
    while !st.is_empty() {
        let e = st.pop().unwrap();
        match *e {
            parser::Node::Term { lexeme } => {
                let token_id = lexeme.tok_id();
                let term_name = grm.token_name(TIdx(token_id)).unwrap();
                let span = lexeme.span();
                let lexeme_string = &input[span.start()..span.start() + span.len()];
                let (line_no, col_no) = lexer.line_col(span).0;
                child_node = arena.new_node(term_name.to_string(),
                                            lexeme_string.to_string(),
                                            Some(col_no),
                                            Some(line_no),
                                            Some(span.start()),
                                            Some(span.len()));
                match parent.pop().unwrap() {
                    None => parent.push(None),
                    Some(id) => {
                        child_node.make_child_of(id, &mut arena).ok();
                    }
                };
            }
            parser::Node::Nonterm { ridx, ref nodes } => {
                // A non-terminal has no label of its own, but has a node type.
                child_node = arena.new_node(grm.rule_name_str(ridx).to_string(),
                                            "".to_string(),
                                            None,
                                            None,
                                            None,
                                            None);
                match parent.pop().unwrap() {
                    None => parent.push(None),
                    Some(id) => {
                        child_node.make_child_of(id, &mut arena).ok();
                    }
                };
                // Push children of current non-terminal onto stacks.
                for x in nodes.iter().rev() {
                    st.push(x);
                    parent.push(Some(child_node));
                }
            }
        }
    }
    arena
}
