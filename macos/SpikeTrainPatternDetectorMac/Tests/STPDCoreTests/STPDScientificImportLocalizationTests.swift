@testable import STPDCore
import Testing

@Suite("Scientific import localization")
struct STPDScientificImportLocalizationTests {
    @Test("English and Russian scientific-import catalogs have identical coverage")
    func catalogsHaveMatchingKeys() {
        #expect(
            Set(STPDScientificImportLocalization.english.keys)
                == Set(STPDScientificImportLocalization.russian.keys)
        )
    }

    @Test("Every catalog entry resolves without Chinese fallback")
    func catalogEntriesResolveInBothNonChineseLanguages() {
        for source in STPDScientificImportLocalization.sourceKeys {
            let english = STPDLocalization.text(source, language: .en)
            let russian = STPDLocalization.text(source, language: .ru)

            #expect(!english.isEmpty)
            #expect(!russian.isEmpty)
            #expect(!containsHanScript(english), "Untranslated English import copy for: \(source)")
            #expect(!containsHanScript(russian), "Untranslated Russian import copy for: \(source)")
        }
    }

    @Test("Identity-entry and group-definition controls are translated")
    func identityAndGroupControlsAreCovered() {
        let sources = [
            "记录片段 ID",
            "单一分组语义 ID",
            "分组语义 ID",
            "列语义 ID",
            "事件类型 ID",
            "支持键盘输入、右键粘贴以及 Command-V 粘贴。",
        ]

        for source in sources {
            #expect(STPDLocalization.text(source, language: .en) != source)
            #expect(STPDLocalization.text(source, language: .ru) != source)
        }
    }

    private func containsHanScript(_ text: String) -> Bool {
        text.unicodeScalars.contains { scalar in
            switch scalar.value {
            case 0x3400...0x4DBF, 0x4E00...0x9FFF, 0xF900...0xFAFF:
                true
            default:
                false
            }
        }
    }
}
