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

//! Integration tests for ast module.
//! All file paths are relative to the root of the repository.

extern crate diffract;

use std::path::Path;

use diffract::ast::{ParseError, parse_file};

fn compare_ast_dump_to_lrpar_output(is_java: bool, filepath: &str, expected: &str) {
    let lex = if is_java {
        Path::new("grammars/java.l")
    } else {
        Path::new("grammars/calc.l")
    };
    let yacc = if is_java {
        Path::new("grammars/java.y")
    } else {
        Path::new("grammars/calc.y")
    };
    let arena = parse_file(filepath, lex, yacc).unwrap();
    let arena_pretty_printed = format!("{:}", arena);
    assert_eq!(expected, arena_pretty_printed);
}

#[test]
fn test_empty_calc() {
    compare_ast_dump_to_lrpar_output(
        false,
        "tests/empty.calc",
        "^~
 ~
  WHITESPACE  \n
  ~
 Expr
  Term
   Factor
",
    );
}

#[test]
fn test_one_calc() {
    compare_ast_dump_to_lrpar_output(
        false,
        "tests/one.calc",
        "^~
 ~
 Expr
  Term
   Factor
    INT 1
    ~
     WHITESPACE \n
     ~\n",
    );
}

#[test]
fn test_add_calc() {
    compare_ast_dump_to_lrpar_output(
        false,
        "tests/add.calc",
        "^~
 ~
 Expr
  Term
   Factor
    INT 1
    ~
  PLUS +
  ~
  Expr
   Term
    Factor
     INT 2
     ~
      WHITESPACE \n
      ~\n",
      );
}

#[test]
fn test_mult_calc() {
    compare_ast_dump_to_lrpar_output(
        false,
        "tests/mult.calc",
        "^~
 ~
 Expr
  Term
   Factor
    INT 3
    ~
   MULT *
   ~
   Term
    Factor
     INT 1
     ~
  PLUS +
  ~
  Expr
   Term
    Factor
     INT 2
     ~
      WHITESPACE \n
      ~\n",
    );
}

#[test]
fn test_hello_java() {
    compare_ast_dump_to_lrpar_output(
        true,
        "tests/Hello.java",
       "^~
 ~
 goal
  compilation_unit
   package_declaration_opt
   import_declarations_opt
   type_declarations_opt
    type_declarations
     type_declaration
      class_declaration
       modifiers_opt
        modifiers
         modifier
          PUBLIC public
          ~
           WHITESPACE  \n           ~
       CLASS class
       ~
        WHITESPACE  \n        ~
       IDENTIFIER Hello
       ~
        WHITESPACE  \n        ~
       type_parameters_opt
       super_opt
       interfaces_opt
       class_body
        LBRACE {
        ~
         WHITESPACE \n    \n         ~
        class_body_declarations_opt
         class_body_declarations
          class_body_declaration
           class_member_declaration
            method_declaration
             method_header
              modifiers_opt
               modifiers
                modifiers
                 modifier
                  PUBLIC public
                  ~
                   WHITESPACE  \n                   ~
                modifier
                 STATIC static
                 ~
                  WHITESPACE  \n                  ~
              VOID void
              ~
               WHITESPACE  \n               ~
              method_declarator
               IDENTIFIER main
               ~
               LPAREN (
               ~
               formal_parameter_list_opt
                formal_parameter_list
                 formal_parameter
                  type
                   reference_type
                    array_type
                     name
                      simple_name
                       IDENTIFIER String
                       ~
                     dims
                      LBRACK [
                      ~
                      RBRACK ]
                      ~
                       WHITESPACE  \n                       ~
                  variable_declarator_id
                   IDENTIFIER args
                   ~
               RPAREN )
               ~
                WHITESPACE  \n                ~
              throws_opt
             method_body
              block
               LBRACE {
               ~
                WHITESPACE \n        \n                ~
               block_statements_opt
                block_statements
                 block_statement
                  statement
                   statement_without_trailing_substatement
                    expression_statement
                     statement_expression
                      method_invocation
                       qualified_name
                        name
                         qualified_name
                          name
                           simple_name
                            IDENTIFIER System
                            ~
                          DOT .
                          ~
                          IDENTIFIER out
                          ~
                        DOT .
                        ~
                        IDENTIFIER println
                        ~
                       LPAREN (
                       ~
                       argument_list_opt
                        argument_list
                         expression
                          assignment_expression
                           conditional_expression
                            conditional_or_expression
                             conditional_and_expression
                              inclusive_or_expression
                               exclusive_or_expression
                                and_expression
                                 equality_expression
                                  instanceof_expression
                                   relational_expression
                                    shift_expression
                                     additive_expression
                                      multiplicative_expression
                                       unary_expression
                                        unary_expression_not_plus_minus
                                         postfix_expression
                                          primary
                                           primary_no_new_array
                                            literal
                                             STRING_LITERAL \"Hello, world!\"
                                             ~
                       RPAREN )
                       ~
                     SEMICOLON ;
                     ~
                      WHITESPACE \n    \n                      ~
               RBRACE }
               ~
                WHITESPACE \n\n                ~
        RBRACE }
        ~
         WHITESPACE \n\n         ~
",
    );
}

#[test]
fn test_comment_java() {
    compare_ast_dump_to_lrpar_output(
        true,
        "tests/Comment.java",
        "^~
 ~
  COMMENT /* Multiline 1 */
  ~
   WHITESPACE \n
   ~
 goal
  compilation_unit
   package_declaration_opt
   import_declarations_opt
   type_declarations_opt
    type_declarations
     type_declaration
      class_declaration
       modifiers_opt
        modifiers
         modifier
          PUBLIC public
          ~
           WHITESPACE  \n           ~
            COMMENT /* Multiline 2 */
            ~
             WHITESPACE  \n             ~
       CLASS class
       ~
        WHITESPACE  \n        ~
         COMMENT /* Multiline 3 */
         ~
          WHITESPACE  \n          ~
       IDENTIFIER Comment
       ~
        WHITESPACE  \n        ~
         COMMENT /* Multiline 4 */
         ~
          WHITESPACE  \n          ~
       type_parameters_opt
       super_opt
       interfaces_opt
       class_body
        LBRACE {
        ~
         WHITESPACE \n    \n         ~
          COMMENT // Single line comment.
          ~
           WHITESPACE \n
           ~
        class_body_declarations_opt
        RBRACE }
        ~
         WHITESPACE  \n         ~
          COMMENT /* Multiline 5
   *
   *
   *
   */
          ~
           WHITESPACE \n
           ~
");
}

#[test]
fn test_nested_comment_java() {
    compare_ast_dump_to_lrpar_output(
        true,
        "tests/NestedComment.java",
        "^~
 ~
  COMMENT /*
 * // Single line comment nested in multi-line comment.
 */
  ~
   WHITESPACE \n
   ~
 goal
  compilation_unit
   package_declaration_opt
   import_declarations_opt
   type_declarations_opt
    type_declarations
     type_declaration
      class_declaration
       modifiers_opt
        modifiers
         modifier
          PUBLIC public
          ~
           WHITESPACE  \n           ~
       CLASS class
       ~
        WHITESPACE  \n        ~
       IDENTIFIER NestedComment
       ~
        WHITESPACE  \n        ~
       type_parameters_opt
       super_opt
       interfaces_opt
       class_body
        LBRACE {
        ~
        class_body_declarations_opt
        RBRACE }
        ~
         WHITESPACE \n
         ~
");
}

#[test]
fn test_get_lines() {
    let lex = Path::new("grammars/java.l");
    let yacc = Path::new("grammars/java.y");
    let ast = parse_file("tests/Hello.java", lex, yacc).unwrap();
    let lines = ast.get_lines();
    assert_eq!(5, lines.len());
    for i in 1..6 as usize {
        assert!(lines.contains_key(&i));
    }
    // Input file contents:
    // -------------------
    // public class Hello {
    //     public static void main(String[] args) {
    //         System.out.println("Hello, world!");
    //     }
    // }
    let expected_1 = vec!["public", " ", "class", " ", "Hello", " ", "{", "\n    "]
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>();
    let expected_2 = vec!["public", " ", "static", " ", "void", " ", "main", "(",
                          "String", "[", "]", " ", "args", ")", " ", "{", "\n        "]
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>();
    let expected_3 = vec!["System",
                          ".",
                          "out",
                          ".",
                          "println",
                          "(",
                          "\"Hello, world!\"",
                          ")",
                          ";",
                          "\n    "]
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>();
    let expected_4 = vec![String::from("}"), String::from("\n")];
    let expected_5 = vec![String::from("}"), String::from("\n")];
    let expected = vec![expected_1, expected_2, expected_3, expected_4, expected_5];
    for line_no in 1..6 {
        let labels = lines[&line_no]
            .iter()
            .map(|id| ast[*id].value.clone())
            .collect::<Vec<String>>();
        assert_eq!(expected[line_no - 1], labels);
    }
}

#[test]
fn test_non_existant_input() {
    let lex = Path::new("grammars/calc.l");
    let yacc = Path::new("grammars/calc.y");
    let ast_result = parse_file("nosuchfileexists.calc", lex, yacc);
    match ast_result {
        Err(ParseError::FileNotFound(s)) => assert_eq!(s, String::from("nosuchfileexists.calc")),
        Err(e) => panic!("Expected FileNotFound error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST"),
    }
}

#[test]
fn test_non_existant_lex() {
    let lex = Path::new("grammars/nosuchfileexists.l");
    let yacc = Path::new("grammars/calc.y");
    let ast_result = parse_file("tets/one.calc", lex, yacc);
    match ast_result {
        Err(ParseError::FileNotFound(s)) => {
            assert_eq!(s, String::from("grammars/nosuchfileexists.l"))
        }
        Err(e) => panic!("Expected FileNotFound error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST"),
    }
}

#[test]
fn test_non_existant_grm() {
    let lex = Path::new("grammars/calc.l");
    let yacc = Path::new("grammars/nosuchfileexists.y");
    let ast_result = parse_file("tests/one.calc", lex, yacc);
    match ast_result {
        Err(ParseError::FileNotFound(s)) => {
            assert_eq!(s, String::from("grammars/nosuchfileexists.y"))
        }
        Err(e) => panic!("Expected FileNotFound error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST"),
    }
}

#[test]
fn test_lexical_err() {
    let lex = Path::new("grammars/calc.l");
    let yacc = Path::new("grammars/calc.y");
    let ast_result = parse_file("tests/calc_lexical_err.calc", lex, yacc);
    match ast_result {
        Err(ParseError::LexicalError) => assert!(true),
        Err(e) => panic!("Expected LexicalError error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST"),
    }
}

#[test]
fn test_syntax_err() {
    let lex = Path::new("grammars/calc.l");
    let yacc = Path::new("grammars/calc.y");
    let ast_result = parse_file("tests/calc_syntax_err.calc", lex, yacc);
    match ast_result {
        Err(ParseError::SyntaxError) => assert!(true),
        Err(e) => panic!("Expected SyntaxError error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST"),
    }
}

#[test]
fn test_broken_lex() {
    let lex = Path::new("tests/grammars/broken.l");
    let yacc = Path::new("grammars/txt.y");
    let ast_result = parse_file("tests/lorem1.txt", lex, yacc);
    match ast_result {
        Err(ParseError::BrokenLexer) => assert!(true),
        Err(e) => panic!("Expected BrokenLexer error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST"),
    }
}

#[test]
fn test_broken_grm() {
    let lex = Path::new("grammars/txt.l");
    let yacc = Path::new("tests/grammars/broken.y");
    let ast_result = parse_file("tests/lorem1.txt", lex, yacc);
    match ast_result {
        Err(ParseError::BrokenParser) => assert!(true),
        Err(e) => panic!("Expected BrokenParser error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST"),
    }
}
