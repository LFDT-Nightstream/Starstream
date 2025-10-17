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
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});
