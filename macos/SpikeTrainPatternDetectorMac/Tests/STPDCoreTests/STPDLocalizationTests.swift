import STPDCore
import Testing

// I18N-1: pure tests for the centralized localization helper. Chinese (default) returns the source verbatim;
// English returns R-parity dictionary translations; unknown strings fall back safely; machine strings are
// never translated (they are never dictionary keys and never routed through the helper in practice).

@Test
func zhReturnsSourceVerbatim() {
    #expect(STPDLocalization.text("神经流形", language: .zh) == "神经流形")
    #expect(STPDLocalization.text("ISI 状态空间", language: .zh) == "ISI 状态空间")
    #expect(STPDLocalization.text("anything 任意 mixed", language: .zh) == "anything 任意 mixed")
    #expect(STPDLocalization.text("爆发", language: .zh) == "爆发")
}

@Test
func enTranslatesSIM1DSimulatorPreviewCopy() {
    let cases: [(String, String)] = [
        ("模拟 / 预览", "Simulator / Preview"),
        ("标准强直", "Clean tonic"),
        ("非纯强直", "Not clean tonic"),
        ("CV 过高", "CV too high"),
        ("抖动 SD", "Jitter SD"),
        ("检测标签", "Detector labels"),
        ("判定", "Verdict"),
        ("按当前检测器门限评估", "Evaluated against the current detector gates"),
        ("强直门限", "Tonic gates"),
        ("默认", "Default"),
        ("变化", "Changed"),
        ("无变化", "No change"),
    ]
    for (source, expected) in cases {
        #expect(STPDLocalization.text(source, language: .en) == expected)
        #expect(STPDLocalization.text(source, language: .zh) == source)
    }
}

@Test
func enTranslatesSIM2DBurstPreviewCopy() {
    let cases: [(String, String)] = [
        // Mode picker + burst controls / table headers.
        ("爆发响应", "Burst response"),
        ("按当前检测器参数评估", "Evaluated against the current detector parameters"),
        ("爆发门限", "Burst gates"),
        ("生成的爆发响应序列（按包棘波数从小到大）", "Generated burst-response trains (by packet spike count, ascending)"),
        ("包棘波", "Packet spikes"),
        ("命中 / 可能 / 拆分 / 漏检", "Hit / Poss / Split / Miss"),   // SIM-2G: compacted
        ("包大小阶梯", "Packet ladder"),
        ("常规", "Regular"),
        ("密集", "Dense"),
        ("稀疏", "Sparse"),
        // Burst verdict display labels (these are the model's `localizationSourceZH` strings).
        ("识别为爆发", "Detected as burst"),
        ("识别为可能爆发", "Detected as possible burst"),
        ("拆分或部分爆发包", "Split / partial packet"),
        ("漏检爆发包", "Missed packet"),
    ]
    for (source, expected) in cases {
        #expect(STPDLocalization.text(source, language: .en) == expected)
        #expect(STPDLocalization.text(source, language: .zh) == source)
    }
    // Reused SIM-1D / earlier keys the burst mode also relies on remain intact.
    #expect(STPDLocalization.text("强直", language: .en) == "Tonic")
    #expect(STPDLocalization.text("模式", language: .en) == "Mode")
    #expect(STPDLocalization.text("棘波数", language: .en) == "Spikes")
}

@Test
func enTranslatesSIM2GPolishCopy() {
    // SIM-2G: the per-onset count caption + the compacted dense-table column label.
    #expect(STPDLocalization.text("每个预期爆发时刻的计数。", language: .en) == "Counts are per intended burst onset.")
    #expect(STPDLocalization.text("每个预期爆发时刻的计数。", language: .zh) == "每个预期爆发时刻的计数。")
    #expect(STPDLocalization.text("命中 / 可能 / 拆分 / 漏检", language: .en) == "Hit / Poss / Split / Miss")
    #expect(STPDLocalization.text("命中 / 可能 / 拆分 / 漏检", language: .zh) == "命中 / 可能 / 拆分 / 漏检")
}

@Test
func enTranslatesSIM2EPolishCopy() {
    let cases: [(String, String)] = [
        // Tonic jitter-preset names (the previously-missing English entries).
        ("标准", "Standard"),
        ("精细", "Fine"),
        ("宽", "Wide"),
        // Short burst verdict-chip labels for the dense ladder table (full labels stay on the selected-row card).
        ("可能", "Possible"),
        ("拆分", "Split"),
        ("漏检", "Missed"),
    ]
    for (source, expected) in cases {
        #expect(STPDLocalization.text(source, language: .en) == expected)
        #expect(STPDLocalization.text(source, language: .zh) == source)
    }
    // The detected-as-burst short chip reuses the existing 爆发 → Burst legend key (no duplicate added).
    #expect(STPDLocalization.text("爆发", language: .en) == "Burst")
}

@Test
func sim2DBurstVerdictEnumLocalizesThroughDictionary() {
    // The SIM-2C verdict enum's display labels now resolve to English via the SIM-2D dictionary entries; raw tokens
    // (rawValue) remain the non-localized machine contract.
    #expect(SpikeTrainSimulationBurstVerdict.detectedAsBurst.localizedLabel(language: .en) == "Detected as burst")
    #expect(SpikeTrainSimulationBurstVerdict.detectedAsPossibleBurst.localizedLabel(language: .en) == "Detected as possible burst")
    #expect(SpikeTrainSimulationBurstVerdict.splitOrPartialPacket.localizedLabel(language: .en) == "Split / partial packet")
    #expect(SpikeTrainSimulationBurstVerdict.missedPacket.localizedLabel(language: .en) == "Missed packet")
    // zh returns the verbatim Chinese source; rawValue tokens are never localized.
    #expect(SpikeTrainSimulationBurstVerdict.detectedAsBurst.localizedLabel(language: .zh) == "识别为爆发")
    #expect(SpikeTrainSimulationBurstVerdict.missedPacket.rawValue == "missed_packet")
}

@Test
func enTranslatesP8ManualThresholdScopeCopy() {
    let cases: [(String, String)] = [
        ("应用范围", "Threshold scope"),
        ("全部序列", "All trains"),
        ("当前序列", "Current train"),
        ("所选序列", "Selected trains"),
        ("作用序列", "Applies to"),
        ("硬门控仅作用于所选范围；软锚点与自动模式始终对全部序列生效。范围若未匹配任何序列，硬门控当前不影响任何序列。",
         "Hard gates apply only to the selected scope; soft anchors and automatic modes always apply to all trains. If the scope targets no train, hard gates currently affect no train."),
        ("按范围应用硬门控", "Hard gate applied by scope"),
    ]
    for (source, expected) in cases {
        #expect(STPDLocalization.text(source, language: .en) == expected)
        #expect(STPDLocalization.text(source, language: .zh) == source)
    }
}

@Test
func enTranslatesP7BAdaptiveV2ExplanationCopy() {
    // P7B: every Adaptive-v2 explanation Chinese source has an English translation in the shared dictionary.
    let cases: [(String, String)] = [
        ("保留为爆发（已修整边界）", "Kept as burst (boundary trimmed)"),
        ("降级为可能爆发", "Downgraded to Possible burst"),
        ("去除左边界。", "Trimmed left edge."),
        ("去除右边界。", "Trimmed right edge."),
        ("去除两侧边界。", "Trimmed both edges."),
        ("该区间更像规则强直放电，而不是标准爆发。",
         "This interval is more compatible with tonic-like regular firing than with a canonical burst."),
        ("该区间只符合桥接 / 尾部范围，缺少足够的爆发核心。",
         "The interval only fits the bridge/tail band and lacks a sufficient burst core."),
        ("Adaptive-v2 证据不足；未做出强标准爆发判定。",
         "Adaptive-v2 evidence was insufficient; no strong canonical decision was made."),
        ("爆发准入上限", "Burst eligibility ceiling"),
    ]
    for (source, expected) in cases {
        #expect(STPDLocalization.text(source, language: .en) == expected)
        #expect(STPDLocalization.text(source, language: .zh) == source)   // zh is the verbatim source
    }
}

@Test
func enReturnsRParityExactTranslations() {
    let cases: [(String, String)] = [
        ("语言 / Language", "Language"),
        ("中文", "Chinese"),
        ("导入数据", "Import data"),
        ("对齐时间戳图", "Aligned timestamp plot"),
        ("原始时间戳图", "Raw timestamp plot"),
        ("ISI 时间剖面", "ISI time profile"),
        ("ISI 状态空间", "ISI state space"),
        ("神经流形", "Neural Manifold"),
        ("检测器 / 参数", "Detector / parameters"),
        ("数据集 ISI 直方图", "Dataset ISI histogram"),
        ("结构候选", "Structure candidates"),
        ("Seed / Bridge 诊断", "Seed / Bridge diagnostics"),
        ("手动标记 vs 检测器报告", "Manual labels vs detector report"),
        ("科学验证", "Scientific validation"),
        ("事件 / 输出", "Events / output"),
    ]
    for (source, expected) in cases {
        #expect(STPDLocalization.text(source, language: .en) == expected)
    }
}

@Test
func enTranslatesPatternLegendNames() {
    #expect(STPDLocalization.text("爆发", language: .en) == "Burst")
    #expect(STPDLocalization.text("暂停", language: .en) == "Pause")
    #expect(STPDLocalization.text("强直发放", language: .en) == "Tonic")
    #expect(STPDLocalization.text("高频强直发放", language: .en) == "HF tonic")
    #expect(STPDLocalization.text("高频连续发放", language: .en) == "HF spiking")
    #expect(STPDLocalization.text("其他", language: .en) == "Others")
    #expect(STPDLocalization.text("未标注", language: .en) == "Unlabeled")
    #expect(STPDLocalization.text("非爆发 / 强负例", language: .en) == "Not burst (veto)")
}

@Test
func enPhraseSubstitutionForCompoundForms() {
    // Parenthetical R inline forms fall through to ordered phrase substitution.
    #expect(STPDLocalization.text("爆发（burst）", language: .en) == "burst")
    #expect(STPDLocalization.text("高频连续发放（HF spiking）", language: .en) == "HF spiking")
    #expect(STPDLocalization.text("长爆发（long burst）", language: .en) == "long burst")
}

@Test
func unknownStringsFallBackToSource() {
    let unknown = "完全未知的界面字符串 12345"
    #expect(STPDLocalization.text(unknown, language: .en) == unknown)
    #expect(STPDLocalization.text(unknown, language: .zh) == unknown)
    // A purely-English unknown string also passes through in both modes.
    #expect(STPDLocalization.text("Totally Unmapped Label", language: .en) == "Totally Unmapped Label")
    #expect(STPDLocalization.text("Totally Unmapped Label", language: .zh) == "Totally Unmapped Label")
}

@Test
func machineStringsAreNeverTranslated() {
    // Enum raw values, candidate/train IDs, params hashes, finalLabel strings, decision-path / layer keys,
    // and CSV column names must pass through unchanged in BOTH languages.
    let machine = [
        "high_frequency_burst", "possible_burst", "not_burst",
        "burst_response_2_s", "params_hash", "decisionPath",
        "structure_first_classic_burst_anchor", "isi_profile_hard_threshold_burst",
        "alignedRaster", "neuralManifold", "pattern_family", "label_source",
    ]
    for value in machine {
        #expect(STPDLocalization.text(value, language: .en) == value)
        #expect(STPDLocalization.text(value, language: .zh) == value)
    }
}

@Test
func enReturnsI18N2ATranslations() {
    // Representative strings added for the I18N-2A surfaces (Neural Manifold controls, count controls,
    // structure-candidate headings/buttons/columns). zh returns the source; en returns the dictionary value.
    let cases: [(String, String)] = [
        // Spike-train count controls
        ("条", "trains"),
        ("全部", "All"),
        ("选择", "Select"),
        // Neural Manifold controls / legend / summary
        ("视图 / 坐标轴", "View / axes"),
        ("显示与样式", "Display + style"),
        ("变换", "Transform"),
        ("缩放", "Scaling"),
        ("连线", "Line"),
        ("时间着色", "Time color"),
        ("事件", "Events"),
        ("矩阵 (bins × trains)", "Matrix (bins × trains)"),
        ("方差解释 (PC1–PC3)", "Variance explained (PC1–PC3)"),
        // Structure candidates
        ("结构候选", "Structure candidates"),
        ("运行检测", "Run detection"),
        ("导出 CSV", "Export CSV"),
        ("候选审核", "Candidate audit"),
        ("性能", "Performance"),
        ("通道", "Channel"),
        ("仅显示已选", "Selected only"),
        ("决策", "Decision"),
        ("在 raster 查看", "View in raster"),
        ("接受", "Accept"),
    ]
    for (source, expected) in cases {
        #expect(STPDLocalization.text(source, language: .en) == expected)
        #expect(STPDLocalization.text(source, language: .zh) == source)
    }
}

@Test
func i18n2AMachineStringsStillPassThrough() {
    // Strings that look adjacent to the new UI labels but are machine values must remain verbatim in both
    // languages (e.g. detector decision-path / track keys, CSV/export tokens, enum raw values).
    let machine = [
        "event_track", "gap_track", "state_track", "candidate_audit",
        "isi_profile_hard_threshold_burst", "auditRecommendedTrackRawValue",
        "trainName", "decisionPath", "csv", "params_hash",
    ]
    for value in machine {
        #expect(STPDLocalization.text(value, language: .en) == value)
        #expect(STPDLocalization.text(value, language: .zh) == value)
    }
}

@Test
func enReturnsI18N2BTranslations() {
    let cases: [(String, String)] = [
        // Selection-scope short titles
        ("Spike 序列", "Spike trains"),
        ("ISI 序列", "ISI trains"),
        ("状态空间序列", "State-space trains"),
        ("流形序列", "Manifold trains"),
        // Detector parameters
        ("手动阈值", "Manual thresholds"),
        ("事件模式参数", "Event pattern parameters"),
        ("种子最大毫秒", "Seed max ms"),
        ("种子锚点毫秒", "Seed anchor ms"),
        ("桥接锚点毫秒", "Bridge anchor ms"),
        ("软", "Soft"),
        ("硬", "Hard"),
        // Phase 1F: learned-threshold wording polish
        ("从手动标注学习（预览）", "Learned from manual annotations (preview)"),
        ("应用学习到的阈值", "Apply learned thresholds"),
        ("不覆盖你已设为硬门控的家族。", "Does not overwrite families you set to Hard."),
        // ISI timeline
        ("布局", "Layout"),
        ("阈值线", "Threshold lines"),
        ("未加载 ISI 序列", "ISI Trace Not Loaded"),
        ("状态层", "States"),
        ("单位", "Unit"),
        // Dataset ISI histogram
        ("结构频带", "Structure bands"),
        ("计数", "Count"),
        ("比例", "Fraction"),
        ("锚点", "Anchors"),
        ("逐序列 ISI 审计", "Per-train ISI audit"),
        // Data QC
        ("质量表", "Quality table"),
        ("重复时间戳", "Duplicate timestamps"),
        ("策略", "Policy"),
        ("应用", "Apply"),
    ]
    for (source, expected) in cases {
        #expect(STPDLocalization.text(source, language: .en) == expected)
        #expect(STPDLocalization.text(source, language: .zh) == source)
    }
    // The histogram "Fraction" mode uses 比例, so the earlier variance-table 占比 mapping is unchanged.
    #expect(STPDLocalization.text("占比", language: .en) == "explained")
}

@Test
func causalNeutralMinimumISITranslationsAreStable() {
    let cases: [(String, String)] = [
        ("最小有效 ISI", "Minimum valid ISI"),
        ("低于最小有效 ISI", "Below minimum ISI"),
        ("最小有效 ISI 单位", "Minimum valid ISI unit"),
        ("显示最小有效 ISI 与疑似不应期阈值。", "Show minimum-valid-ISI and refractory-suspect thresholds."),
        ("显示最小有效 ISI 与绝对不应期阈值。", "Show minimum-valid-ISI and absolute-refractory thresholds."),
        ("低于最小有效 ISI 而排除", "Excluded below minimum ISI"),
        ("低于最小有效 ISI 的区间", "Below-minimum ISI"),
        ("低于最小有效 ISI 的区间详情", "Below-minimum ISI details"),
        ("当前没有低于最小有效阈值的 ISI。", "No ISI falls below the current minimum-valid threshold."),
    ]

    for (source, expected) in cases {
        #expect(STPDLocalization.text(source, language: .en) == expected)
        #expect(STPDLocalization.text(source, language: .zh) == source)
    }

    // Persisted legacy machine tokens remain outside localization and are not renamed here.
    for machine in ["artifact", "artifact_below_floor", "artifact_threshold_sec"] {
        #expect(STPDLocalization.text(machine, language: .en) == machine)
        #expect(STPDLocalization.text(machine, language: .zh) == machine)
    }
}

@Test
func i18n2BMachineStringsStillPassThrough() {
    // Threshold / QC / export machine tokens adjacent to the new labels must remain verbatim in both modes.
    let machine = [
        "soft_anchor", "hard_gate", "resolved_thresholds", "duplicate_timestamp_policy",
        "merge_all", "warn_only", "error", "isi_profile_hard_threshold_burst",
        "params_hash", "burst_seed_band", "refractory_suspect",
    ]
    for value in machine {
        #expect(STPDLocalization.text(value, language: .en) == value)
        #expect(STPDLocalization.text(value, language: .zh) == value)
    }
}

@Test
func defaultLanguageIsChineseAndEnumIsStable() {
    #expect(STPDLanguage(rawValue: "zh") == .zh)
    #expect(STPDLanguage(rawValue: "en") == .en)
    #expect(STPDLanguage(rawValue: "ru") == .ru)
    #expect(STPDLanguage(rawValue: "bogus") == nil)
    #expect(STPDLanguage.allCases == [.zh, .en, .ru])
    #expect(STPDLanguage.zh.nativeLabel == "中文")
    #expect(STPDLanguage.en.nativeLabel == "English")
    #expect(STPDLanguage.ru.nativeLabel == "Русский")
}

@Test
func russianUsesOwnerApprovedPatternTermsAndEnglishFallback() {
    #expect(STPDLocalization.text("爆发", language: .ru) == "пачек")
    #expect(STPDLocalization.text("暂停", language: .ru) == "пауза")
    #expect(STPDLocalization.text("强直发放", language: .ru) == "тоник")
    #expect(STPDLocalization.text("Burst", language: .ru) == "пачек")
    #expect(STPDLocalization.text("确定的单神经元电活动", language: .ru)
        == "Подтверждённая активность одиночного нейрона")
    #expect(STPDLocalization.text("Seed / Bridge 诊断", language: .ru) == "Диагностика Seed / Bridge")
    #expect(STPDLocalization.text("数据集 ISI 直方图", language: .ru)
        == "Гистограмма ISI набора данных")
    #expect(STPDLocalization.text("棘波序列", language: .ru) == "Спайковая последовательность")
    #expect(STPDLocalization.text("显示", language: .ru) == "Показ")
    #expect(STPDLocalization.text("条", language: .ru) == "шт.")
    #expect(STPDLocalization.text("全部", language: .ru) == "Все")
    #expect(STPDLocalization.text("选择", language: .ru) == "Выбрать")
    #expect(STPDLocalization.text("棘波序列", language: .en) == "Spike train")
}

@Test
func rasterTimeAxisAndRussianLayoutLabelsAreFullyLocalized() {
    let cases: [(String, String, String)] = [
        ("对齐时间", "Aligned time", "Выровненное время"),
        ("原始时间戳", "Raw timestamp", "Исходное время"),
        ("可见窗口", "Visible window", "Видимый интервал"),
        ("可见", "Visible", "Показано"),
        ("序列", "Train", "Последовательности"),
        ("spike 光栅图", "spike raster", "растр спайков"),
        ("对齐 spike 光栅图", "Aligned spike raster", "Выровненный растр спайков"),
        ("原始 spike 光栅图", "Raw spike raster", "Растр исходных спайков"),
        ("数据源", "Source", "Источник данных"),
        ("Spike 高度", "Spike height", "Высота спайка"),
    ]

    for (source, english, russian) in cases {
        #expect(STPDLocalization.text(source, language: .zh) == source)
        #expect(STPDLocalization.text(source, language: .en) == english)
        #expect(STPDLocalization.text(source, language: .ru) == russian)
    }
}

@Test
func manualThresholdAssistantAndQCTranslateWithoutMixedChinese() {
    let english: [(String, String)] = [
        ("ISI 阈值初标", "ISI threshold-assisted preliminary labeling"),
        ("ISI 闭区间", "Closed ISI range"),
        ("Spike 数闭区间", "Closed spike-count range"),
        ("应用初标", "Apply preliminary labels"),
        ("手工标记 QC", "Manual-labeling QC"),
        ("绝对无效 ISI", "Absolutely invalid ISI"),
        ("MM = 候选段内最大 ISI / 最小 ISI；仅用于 3–5 个 spike 的短 Tonic 初标。",
         "MM = maximum ISI / minimum ISI within the candidate; it is used only for preliminary short-Tonic labeling with 3–5 spikes."),
    ]
    for (source, expected) in english {
        #expect(STPDLocalization.text(source, language: .en) == expected)
        #expect(STPDLocalization.text(source, language: .zh) == source)
    }

    let russian: [(String, String)] = [
        ("ISI 阈值初标", "Предварительная разметка по порогам ISI"),
        ("模式", "Режим"),
        ("下限", "Нижняя граница"),
        ("上限", "Верхняя граница"),
        ("手工标记 QC", "QC ручной разметки"),
        ("绝对无效 ISI", "Абсолютно недопустимый ISI"),
        ("MM Tonic 初标仅支持 3–5 个 spike，且 MM 闭区间不能小于 1。",
         "Предварительная разметка тоника по MM поддерживает только 3–5 spike, а замкнутый диапазон MM не может быть ниже 1."),
    ]
    for (source, expected) in russian {
        #expect(STPDLocalization.text(source, language: .ru) == expected)
    }
}

@Test
func manualISIWorkbenchSurfaceTranslatesWithoutMixedChinese() {
    let visibleChineseSources = [
        "纯手工 ISI 标记（无需模式检测）",
        "本地草稿 · 未封存",
        "导出已标注 ISI",
        "时间图和表格都只使用原始 spike 时间戳，不运行模式检测。可在时间图拖动选择连续 ISI，也可在表格精确多选。状态与事件相互独立，因此 HFS 可与其内嵌的 HFB 共存；可选择导出 CSV、XLSX 或 NeuroExplorer NEX，文件会明确标记为本地人工草稿。",
        "身份绑定草稿",
        "导入手工 ISI 草稿",
        "确认完整审核",
        "轨道",
        "清除选择",
        "使用 Shift-单击或 Command-单击进行批量选择。",
        "左时间戳（s）",
        "左 MM = 左邻 ISI ÷ 当前 ISI",
        "当前 ISI（ms）",
        "右 MM = 右邻 ISI ÷ 当前 ISI",
        "右时间戳（s）",
        "备注",
        "审核 / ISI #",
        "时间图手工选择",
        "拖动选择连续 ISI；点击已标记色块后按 Delete 可清除该区块。",
        "鼠标悬停在相邻 spike 之间时显示该 ISI 的时间戳、间隔与模式信息",
        "适合窗口",
        "手工 ISI 时间图",
        "未标记",
    ]

    func containsCJK(_ value: String) -> Bool {
        value.unicodeScalars.contains { scalar in
            (0x3400...0x4DBF).contains(scalar.value)
                || (0x4E00...0x9FFF).contains(scalar.value)
        }
    }

    for source in visibleChineseSources {
        #expect(!containsCJK(STPDLocalization.text(source, language: .en)))
        #expect(!containsCJK(STPDLocalization.text(source, language: .ru)))
        #expect(STPDLocalization.text(source, language: .zh) == source)
    }

    #expect(STPDLocalization.text("Spike train", language: .ru)
        == "Спайковая последовательность")
    #expect(STPDLocalization.text("Spike", language: .ru) == "Спайки")
    #expect(STPDLocalization.text("Burst", language: .ru) == "пачек")
    #expect(STPDLocalization.text("Tonic", language: .ru) == "тоник")
    #expect(STPDLocalization.text("Pause", language: .ru) == "пауза")
}

@Test
func delayedBackgroundProgressMessagesTranslateInAllSupportedLanguages() {
    let cases: [(String, String, String)] = [
        (
            "正在运行模式检测并构建审计结果…",
            "Running pattern detection and building audit results…",
            "Выполняется детекция паттернов и формируются результаты аудита…"
        ),
        (
            "正在生成模式热力图…",
            "Generating the pattern heatmap…",
            "Построение тепловой карты паттернов…"
        ),
        (
            "正在验证科学含义与规范数据身份…",
            "Validating scientific meaning and canonical dataset identity…",
            "Проверка научного смысла и канонической идентичности набора данных…"
        ),
        (
            "正在生成当前检测的权威结果表…",
            "Building authoritative tables for the current detection run…",
            "Формирование авторитетных таблиц для текущего запуска детектора…"
        ),
        (
            "正在构建结果表的语义化审阅视图…",
            "Building the semantic review view for the result tables…",
            "Формирование семантического представления таблиц результатов для проверки…"
        ),
    ]

    for (source, english, russian) in cases {
        #expect(STPDLocalization.text(source, language: .zh) == source)
        #expect(STPDLocalization.text(source, language: .en) == english)
        #expect(STPDLocalization.text(source, language: .ru) == russian)
    }
}

@Test
func activityModeTerminologyUsesTheApprovedSingleSource() {
    let cases: [(ScientificDatasetActivityMode, String, String, String)] = [
        (.putativeSingleUnit, "确定的单神经元电活动", "确定的单神经元电活动（Single-unit）", "Confirmed single-unit activity"),
        (.intentionalMultiUnit, "多神经元电活动", "多神经元电活动（Multi-unit，实验性）", "Multi-unit activity"),
        (.unknownOrUncertain, "未知或不确定", "未知或不确定", "Unknown or uncertain"),
    ]

    for (mode, source, pickerSource, english) in cases {
        #expect(mode.activityModeDisplaySource == source)
        #expect(mode.activityModePickerDisplaySource == pickerSource)
        #expect(STPDLocalization.text(source, language: .zh) == source)
        #expect(STPDLocalization.text(source, language: .en) == english)
    }
}

@Test
func manualPatternLearningWorkflowTranslatesInEnglishAndRussian() {
    let cases: [(String, String, String)] = [
        (
            "生成学习预览",
            "Generate learning preview",
            "Сформировать предпросмотр"
        ),
        (
            "撤销上一次学习阈值应用",
            "Undo last learned-threshold application",
            "Отменить последнее применение обученных порогов"
        ),
        (
            "单序列探索",
            "Single-train exploratory",
            "Исследовательски: 1 последовательность"
        ),
        (
            "HFS 特征仅报告，尚未写入检测器。",
            "HFS features are report-only and have not been written to the detector.",
            "Признаки HFS только отображаются и не записаны в детектор."
        ),
    ]

    for (source, english, russian) in cases {
        #expect(STPDLocalization.text(source, language: .zh) == source)
        #expect(STPDLocalization.text(source, language: .en) == english)
        #expect(STPDLocalization.text(source, language: .ru) == russian)
    }
}
