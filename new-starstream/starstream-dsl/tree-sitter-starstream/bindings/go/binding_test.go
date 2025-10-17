package tree_sitter_starstream_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_starstream "github.com/paimastudios/starstream/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_starstream.Language())
	if language == nil {
		t.Errorf("Error loading Starstream grammar")
	}
}
