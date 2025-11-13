/**
 * @file Starstream grammar for tree-sitter
 * @author Paima Studios
 * @license MIT/Apache-2.0
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "starstream",

  rules: {
    source_file: ($) => repeat($.definition),

    definition: ($) => choice($.function_definition),

    function_definition: ($) =>
      seq(
        optional($.function_export),
        "fn",
        $.identifier,
        "(",
        optional(seq($.parameter, repeat(seq(",", $.parameter)))),
        ")",
        optional(seq("->", $.type_annotation)),
        $.block,
      ),

    function_export: ($) => "script",

    parameter: ($) => seq($.identifier, ":", $.type_annotation),

    type_annotation: ($) =>
      seq(
        $.identifier,
        optional(
          seq("<", $.type_annotation, repeat(seq(",", $.type_annotation)), ">"),
        ),
      ),

    _statement: ($) =>
      choice(
        $.variable_declaration,
        $.assignment,
        $.return_statement,
        $.if_statement,
        $.while_statement,
        $.block,
        $._expression_statement,
      ),

    variable_declaration: ($) =>
      seq(
        "let",
        optional("mut"),
        $.identifier,
        optional(seq(":", $.type_annotation)),
        "=",
        $.expression,
        ";",
      ),

    assignment: ($) => seq($.identifier, "=", $.expression, ";"),

    if_statement: ($) =>
      seq(
        // First `if` branch.
        "if",
        "(",
        $.expression,
        ")",
        $.block,
        // Subsequent `else if` branches.
        repeat(seq("else", "if", "(", $.expression, ")", $.block)),
        // Final `else` branch.
        optional(seq("else", $.block)),
      ),

    while_statement: ($) => seq("while", "(", $.expression, ")", $.block),

    block: ($) => seq("{", repeat($._statement), optional($.expression), "}"),

    return_statement: ($) => seq("return", optional($.expression), ";"),

    _expression_statement: ($) => seq($.expression, ";"),

    expression: ($) =>
      choice(
        $.integer_literal,
        $.boolean_literal,
        $.identifier,
        seq("(", $.expression, ")"),

        prec.left(7, seq("!", $.expression)),
        prec.left(7, seq("-", $.expression)),

        prec.left(6, seq($.expression, "*", $.expression)),
        prec.left(6, seq($.expression, "/", $.expression)),
        prec.left(6, seq($.expression, "%", $.expression)),

        prec.left(5, seq($.expression, "+", $.expression)),
        prec.left(5, seq($.expression, "-", $.expression)),

        prec.left(4, seq($.expression, "<", $.expression)),
        prec.left(4, seq($.expression, "<=", $.expression)),
        prec.left(4, seq($.expression, ">", $.expression)),
        prec.left(4, seq($.expression, ">=", $.expression)),

        prec.left(3, seq($.expression, "==", $.expression)),
        prec.left(3, seq($.expression, "!=", $.expression)),

        prec.left(2, seq($.expression, "&&", $.expression)),

        prec.left(1, seq($.expression, "||", $.expression)),
      ),

    integer_literal: ($) => /[0-9]+/,
    boolean_literal: ($) => /true|false/,
    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,
  },
});
