import AppKit
@testable import SpikeTrainPatternDetectorMac
import Testing

@Suite("Application edit menu")
@MainActor
struct AppEditMenuTests {
    @Test("Standard text commands route through the first responder")
    func editCommandsUseFirstResponderSelectorsAndShortcuts() throws {
        let menu = AppDelegate.makeStandardEditMenu()
        let expected: [(title: String, action: String, key: String, modifiers: NSEvent.ModifierFlags)] = [
            ("Undo", "undo:", "z", [.command]),
            ("Redo", "redo:", "z", [.command, .shift]),
            ("Cut", "cut:", "x", [.command]),
            ("Copy", "copy:", "c", [.command]),
            ("Paste", "paste:", "v", [.command]),
            ("Select All", "selectAll:", "a", [.command]),
        ]

        #expect(menu.title == "Edit")
        for command in expected {
            let item = try #require(menu.items.first { $0.title == command.title })
            #expect(item.action.map(NSStringFromSelector) == command.action)
            #expect(item.target == nil)
            #expect(item.keyEquivalent == command.key)
            #expect(item.keyEquivalentModifierMask == command.modifiers)
        }
    }
}
