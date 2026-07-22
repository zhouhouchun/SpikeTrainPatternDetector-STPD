import Foundation
import STPDCore
import Testing

// P10 — pure tests for the candidate-inspector reader of the `manual_threshold_scope=...` decision-path note: it
// classifies the scope that let a hard gate through and renders a bilingual one-line label. Raw tokens never localized.

private func parse(_ path: String) -> ManualThresholdScopeProvenance {
    ManualThresholdScopeProvenance.parse(decisionPath: path)
}

@Test func scopeProvenance_parsesAllTrains() {
    let p = parse("resolved_threshold[burst.seed_upper_sec]=user_hard_gate;manual_threshold_scope=all_trains")
    #expect(p.kind == .allTrains)
    #expect(p.isPresent)
    #expect(p.label(language: .en) == "Hard gate applied by scope: All trains")
    #expect(p.label(language: .zh) == "按范围应用硬门控：全部序列")
}

@Test func scopeProvenance_parsesCurrentTrain() {
    let p = parse("x;manual_threshold_scope=current_train(train_id=t3);y")
    #expect(p.kind == .currentTrain)
    #expect(p.label(language: .en) == "Hard gate applied by scope: Current train")
    #expect(p.label(language: .zh) == "按范围应用硬门控：当前序列")
}

@Test func scopeProvenance_parsesSelectedTrains() {
    let p = parse("manual_threshold_scope=selected_trains(count=2,train_id=t1)")
    #expect(p.kind == .selectedTrains)
    #expect(p.label(language: .en) == "Hard gate applied by scope: Selected trains")
    #expect(p.label(language: .zh) == "按范围应用硬门控：所选序列")
}

@Test func scopeProvenance_noNoteIsAbsent() {
    let p = parse("resolved_threshold[burst.seed_upper_sec]=user_hard_gate;some_other=marker")
    #expect(p.kind == .none)
    #expect(!p.isPresent)
    #expect(p == .absent)
    #expect(p.label(language: .en) == nil)
    #expect(p.label(language: .zh) == nil)
    #expect(parse("") == .absent)
}
