/**
 * @file Starstream grammar for tree-sitter
 * @author Paima Studios
 * @license MIT/Apache-2.0
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "starstream",

  // Meant to follow spec grammar closely.
  // Rules names prefixed with _ are hidden in the output syntax tree, best
  // used for choices between single children.
  rules: {
    program: ($) => repeat($._definition),

    // Definitions

    _definition: ($) =>
      choice(
        $.function_definition,
        $.struct_definition,
        $.enum_definition,
        $.utxo_definition,
      ),

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

    function_export: ($) => choice("script"),

    parameter: ($) => seq($.identifier, ":", $.type_annotation),

    struct_definition: ($) =>
      seq(
        "struct",
        $.identifier,
        "{",
        optional(seq($.struct_field, repeat(seq(",", $.struct_field)))),
        optional(","),
        "}",
      ),

    struct_field: ($) => seq($.identifier, ":", $.type_annotation),

    enum_definition: ($) =>
      seq(
        "enum",
        $.identifier,
        "{",
        optional(seq($.enum_variant, repeat(seq(",", $.enum_variant)))),
        optional(","),
        "}",
      ),

    enum_variant: ($) =>
      seq(
        $.identifier,
        optional(
          choice($.enum_variant_tuple_payload, $.enum_variant_struct_payload),
        ),
      ),

    enum_variant_tuple_payload: ($) =>
      seq(
        "(",
        optional(seq($.type_annotation, repeat(seq(",", $.type_annotation)))),
        optional(","),
        ")",
      ),

    enum_variant_struct_payload: ($) =>
      seq(
        "{",
        optional(seq($.struct_field, repeat(seq(",", $.struct_field)))),
        optional(","),
        "}",
      ),

    utxo_definition: ($) =>
      seq("utxo", $.identifier, "{", repeat($._utxo_part), "}"),

    _utxo_part: ($) => choice($.storage_utxo_part),

    storage_utxo_part: ($) => seq("storage", "{", repeat($.utxo_global), "}"),

    utxo_global: ($) =>
      seq("let", "mut", $.identifier, ":", $.type_annotation, ";"),

    // Type syntax

    type_annotation: ($) =>
      seq(
        $.identifier,
        optional(
          seq("<", $.type_annotation, repeat(seq(",", $.type_annotation)), ">"),
        ),
      ),

    // Blocks and statements

    block: ($) => seq("{", repeat($._statement), optional($.expression), "}"),

    _statement: ($) =>
      choice(
        $.variable_declaration,
        $.assignment,
        $.while_statement,
        $.return_statement,
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

    while_statement: ($) => seq("while", "(", $.expression, ")", $.block),

    return_statement: ($) => seq("return", optional($.expression), ";"),

    _expression_statement: ($) => seq($.expression, ";"),

    // Expressions

    expression: ($) =>
      choice(
        $._postfix_expression,
        $._primary_expression,

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

    // Postfix expressions: function calls and field access
    _postfix_expression: ($) => choice($.call_expression, $.field_expression),

    call_expression: ($) =>
      seq(
        field("function", choice($._primary_expression, $.field_expression)),
        field("arguments", $.arguments),
      ),

    arguments: ($) =>
      seq(
        "(",
        optional(seq($.expression, repeat(seq(",", $.expression)), optional(","))),
        ")",
      ),

    field_expression: ($) =>
      seq(
        field("operand", choice($._primary_expression, $.call_expression, $.field_expression)),
        ".",
        field("field", $.identifier),
      ),

    if_expression: ($) =>
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

    _primary_expression: ($) =>
      choice(
        seq("(", $.expression, ")"),

        // Ambiguity: `match foo { patterns }` vs `match foo { struct fields } { patterns }`.
        // In this case, identifier has priority. Use parens to get struct literal.
        prec(1, $.identifier),

        $.integer_literal,
        $.boolean_literal,
        $.unit_literal,
        $.struct_literal,
        $.enum_constructor,

        $.block,
        $.if_expression,
        $.match_expression,
      ),

    struct_literal: ($) =>
      seq(
        $.identifier,
        "{",
        optional(
          seq(
            $.struct_field_initializer,
            repeat(seq(",", $.struct_field_initializer)),
          ),
        ),
        optional(","),
        "}",
      ),

    struct_field_initializer: ($) => seq($.identifier, ":", $.expression),

    enum_constructor: ($) =>
      seq(
        $.identifier,
        "::",
        $.identifier,
        optional(
          choice(
            $.enum_constructor_tuple_payload,
            $.enum_constructor_struct_payload,
          ),
        ),
      ),

    enum_constructor_tuple_payload: ($) =>
      seq(
        "(",
        optional(seq($.expression, repeat(seq(",", $.expression)))),
        optional(","),
        ")",
      ),

    enum_constructor_struct_payload: ($) =>
      seq(
        "{",
        optional(
          seq(
            $.struct_field_initializer,
            repeat(seq(",", $.struct_field_initializer)),
          ),
        ),
        optional(","),
        "}",
      ),

    match_expression: ($) =>
      seq(
        "match",
        $.expression,
        "{",
        optional(seq($.match_arm, repeat(seq(",", $.match_arm)))),
        optional(","),
        "}",
      ),

    match_arm: ($) => seq($.pattern, "=>", $.block),

    // Patterns

    pattern: ($) =>
      choice(
        $.wildcard_pattern,
        $.literal_pattern,
        $.struct_pattern,
        $.enum_variant_pattern,
        $.identifier,
      ),

    wildcard_pattern: ($) => "_",

    literal_pattern: ($) =>
      choice($.integer_literal, $.boolean_literal, $.unit_literal),

    struct_pattern: ($) =>
      seq(
        $.identifier,
        "{",
        optional(
          seq($.struct_field_pattern, repeat(seq(",", $.struct_field_pattern))),
        ),
        optional(","),
        "}",
      ),

    struct_field_pattern: ($) =>
      choice(
        seq($.identifier, ":", $.pattern),
        alias($.identifier, $.struct_field_pattern_name),
      ),

    enum_variant_pattern: ($) =>
      seq(
        $.identifier,
        "::",
        $.identifier,
        optional(
          choice(
            $.enum_variant_pattern_tuple_payload,
            $.enum_variant_pattern_struct_payload,
          ),
        ),
      ),

    enum_variant_pattern_tuple_payload: ($) =>
      seq(
        "(",
        optional(seq($.pattern, repeat(seq(",", $.pattern)))),
        optional(","),
        ")",
      ),

    enum_variant_pattern_struct_payload: ($) =>
      seq(
        "{",
        optional(
          seq($.struct_field_pattern, repeat(seq(",", $.struct_field_pattern))),
        ),
        optional(","),
        "}",
      ),

    // Literals and other terminals

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,
    integer_literal: ($) => /[0-9]+/,
    boolean_literal: ($) => choice("true", "false"),
    unit_literal: ($) => seq("(", ")"),
  },
  conflicts: ($) => [[$.enum_constructor]],
});
