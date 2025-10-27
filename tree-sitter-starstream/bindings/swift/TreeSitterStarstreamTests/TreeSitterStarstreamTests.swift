import XCTest
import SwiftTreeSitter
import TreeSitterStarstream

final class TreeSitterStarstreamTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_starstream())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Starstream grammar")
    }
}
