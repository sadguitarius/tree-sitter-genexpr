import XCTest
import SwiftTreeSitter
import TreeSitterGenexpr

final class TreeSitterGenexprTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_genexpr())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading GenExpr grammar")
    }
}
