const assert = require("node:assert");
const fs = require("node:fs");
const path = require("node:path");
const { test } = require("node:test");

const Parser = require("tree-sitter");

test("can load grammar", () => {
  const parser = new Parser();
  assert.doesNotThrow(() => parser.setLanguage(require(".")));
});

test("keywords do not match inside identifiers", () => {
  const language = require(".");
  const parser = new Parser();
  parser.setLanguage(language);

  // This recovery case mirrors examples/pay_to_pubkey_hash.star. The effect
  // annotation is not in the grammar yet, but its type name should still be
  // highlighted as one token rather than matching the `as` keyword within it.
  const tree = parser.parse("abi X { fn consume() / [TokenReleased]; }");
  const highlights = fs.readFileSync(
    path.join(__dirname, "../../queries/highlights.scm"),
    "utf8",
  );
  const query = new Parser.Query(language, highlights);
  const keywords = query
    .captures(tree.rootNode)
    .filter(({ name }) => name === "keyword")
    .map(({ node }) => node.text);

  assert.deepStrictEqual(keywords, ["abi", "fn"]);
});
