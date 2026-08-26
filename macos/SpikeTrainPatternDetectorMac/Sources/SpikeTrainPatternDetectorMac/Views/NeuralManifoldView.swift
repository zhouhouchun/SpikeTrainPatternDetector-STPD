import STPDCore
import SwiftUI

/// The native Neural Manifold page. It builds the NM-1A population matrix from the current settings and embeds
/// `matrix.scaled` with the selected method — PCA (default), Isomap, PHATE fallback, FA, or GPFA-style —
/// showing the population trajectory plus compact matrix / method diagnostics / event-state summaries, with
/// NM-2A event-state coloring (annotation only).
///
/// Scope: PCA + Isomap + PHATE fallback + FA + GPFA-style only — no UMAP/t-SNE/CEBRA, no sliceTCA, no
/// validation / decoding / behavior, no CSV export. Coordinates always come from the binned population
/// activity, never from event labels. Selection + parameters are this page's own in-memory document state,
/// independent of the raster / ISI / ISI-state-space pages.
struct NeuralManifoldView: View {
    @Bindable var document: RasterDocument
    @Environment(\.l10n) private var l10n
    /// The bin currently click-selected in the 3-D plot (its hover card + scene highlight). 2-D uses its own
    /// transient hover state inside the canvas; this drives the click-to-inspect overlay for 3-D only.
    @State private var selected3DBinID: Int?

    private var parameters: NeuralPopulationParameters { document.neuralManifoldParameters }

    /// The selected train ids, restricted to the dataset and in dataset (column) order.
    private func selectedTrainIDsInDatasetOrder(_ dataset: SpikeDataset) -> [String] {
        let selected = document.neuralManifoldSelectedTrainIDs
        return dataset.trains.map(\.id).filter { selected.contains($0) }
    }

    /// Color-by control: Time / Auto pattern / Final reviewed pattern (annotation overlay only).
    private var colorSelector: some View {
        HStack(spacing: 6) {
            Text(l10n.t("颜色")).font(.caption).foregroundStyle(.secondary)
            GlassSegmentedControl(
                options: NeuralManifoldColorMode.allCases.map { ($0, $0.title) },
                selection: $document.neuralManifoldColorMode,
                minSegmentWidth: 40
            )
            .fixedSize(horizontal: true, vertical: false)
        }
    }

    /// NM-3B: embedding method selector (PCA / Isomap). PCA is the default.
    private var methodSelector: some View {
        HStack(spacing: 6) {
            Text(l10n.t("方法")).font(.caption).foregroundStyle(.secondary)
            GlassSegmentedControl(
                options: NeuralManifoldMethod.allCases.map { ($0, $0.title) },
                selection: $document.neuralManifoldMethod,
                minSegmentWidth: 40
            )
            .fixedSize(horizontal: true, vertical: false)
        }
    }

    /// NM-3B: Isomap neighbor count + disconnected-graph handling (shown only when method == Isomap).
    private var isomapControls: some View {
        HStack(spacing: 14) {
            numberControl(l10n.t("邻居数"), suffix: "") {
                DebouncedIntField("15", value: $document.neuralManifoldIsomapNeighbors, range: 2...100, width: 56)
            }
            numberControl(l10n.t("最大嵌入分箱"), suffix: "") {
                DebouncedIntField("1200", value: $document.neuralManifoldMaxEmbeddedBins, range: 20...5000, width: 72)
            }
            HStack(spacing: 6) {
                Text(l10n.t("断开处理")).font(.caption).foregroundStyle(.secondary)
                GlassSegmentedControl(
                    options: NeuralManifoldIsomapComponentMode.allCases.map { ($0, $0.title) },
                    selection: $document.neuralManifoldIsomapComponentMode,
                    minSegmentWidth: 56
                )
                .fixedSize(horizontal: true, vertical: false)
            }
        }
    }

    /// NM-3C: PHATE diffusion neighbor count + diffusion time + max embedded bins (shown only when method ==
    /// PHATE). Neighbor count + max embedded bins are the shared sampled-embedding controls (reused from Isomap).
    private var phateControls: some View {
        HStack(spacing: 14) {
            numberControl(l10n.t("邻居数"), suffix: "") {
                DebouncedIntField("15", value: $document.neuralManifoldIsomapNeighbors, range: 2...100, width: 56)
            }
            numberControl(l10n.t("扩散时间"), suffix: "") {
                DebouncedIntField("3", value: $document.neuralManifoldDiffusionTime, range: 1...50, width: 56)
            }
            numberControl(l10n.t("最大嵌入分箱"), suffix: "") {
                DebouncedIntField("1200", value: $document.neuralManifoldMaxEmbeddedBins, range: 20...5000, width: 72)
            }
        }
    }

    /// Method-aware title for the embedding-parameters control cell.
    private var embeddingParametersTitle: String {
        switch document.neuralManifoldMethod {
        case .pca: return l10n.t("PCA 参数")
        case .isomap: return l10n.t("Isomap 参数")
        case .phate: return l10n.t("PHATE 参数")
        case .fa: return l10n.t("FA 参数")
        case .gpfa: return l10n.t("GPFA 参数")
        }
    }

    /// Method-aware "Computing …" status shown in the plot while the background embedding runs.
    private var embeddingComputingMessage: String {
        document.neuralManifoldMethod == .phate ? l10n.t("正在计算 PHATE…") : l10n.t("正在计算 Isomap…")
    }

    /// Build the R-style event-state layer (`R/61` `stpd_neural_fast_event_state_result`) for one label source
    /// over the current selected trains. Auto = raw selected annotations, Final = reviewed public projection.
    /// Annotation/validation layer only — the population matrix and embedding scores are never touched.
    private func eventStateLayer(
        _ matrix: NeuralPopulationMatrix, source: NeuralPopulationEventLabelSource
    ) -> NeuralPopulationEventStateResult {
        let annotations = source == .auto
            ? document.classicAnchorRawEventAnnotations
            : document.classicAnchorPublicEventAnnotations
        let selectedTrains = (document.dataset?.trains ?? [])
            .filter { document.neuralManifoldSelectedTrainIDs.contains($0.id) }
        return NeuralPopulationEventStateLayer.build(
            matrix: matrix, trains: selectedTrains, annotations: annotations, labelSource: source
        )
    }

    /// Per-bin colors for the pattern color modes (annotation overlay only; `nil` for Time mode), via the
    /// event-state layer's dominant state. Embedding scores are unchanged.
    private func binPatternColors(_ matrix: NeuralPopulationMatrix) -> [Int: Color]? {
        guard document.neuralManifoldColorMode.isPattern else { return nil }
        let source: NeuralPopulationEventLabelSource =
            document.neuralManifoldColorMode == .autoPattern ? .auto : .final
        let layer = eventStateLayer(matrix, source: source)
        return Dictionary(uniqueKeysWithValues: layer.rows.map {
            ($0.binID, NeuralManifoldView.patternColor($0.dominantState))
        })
    }

    static func patternColor(_ state: SpikePatternState) -> Color { SpikePatternColor.color(state) }

    /// Per-bin context for the hover inspector, from the SAME event-state layer as the color overlay — no second
    /// labeling system — querying BOTH sources (auto = raw, final = reviewed-public) regardless of color mode.
    private func binHoverInfo(_ matrix: NeuralPopulationMatrix) -> [Int: NeuralManifoldBinHoverInfo] {
        let auto = eventStateLayer(matrix, source: .auto)
        let final = eventStateLayer(matrix, source: .final)
        let totalTrains = matrix.trainCount

        var info: [Int: NeuralManifoldBinHoverInfo] = [:]
        for bin in matrix.bins {
            let a = auto.row(forBinID: bin.binID)
            let f = final.row(forBinID: bin.binID)
            info[bin.binID] = NeuralManifoldBinHoverInfo(
                autoDominant: a?.dominantState ?? .unlabeled,
                autoDominantFraction: a.map { $0.fraction($0.dominantState) } ?? 0,
                autoUnlabeledFraction: a?.unlabeledFraction ?? 1,
                finalDominant: f?.dominantState ?? .unlabeled,
                finalDominantFraction: f.map { $0.fraction($0.dominantState) } ?? 0,
                finalUnlabeledFraction: f?.unlabeledFraction ?? 1,
                contributingTrains: a?.contributingTrainCount ?? 0,
                totalTrains: totalTrains
            )
        }
        return info
    }

    /// Build the matrix + selected embedding once per render, classifying every empty/error state for the UI.
    private var outcome: NeuralManifoldOutcome {
        guard let dataset = document.dataset else { return .noDataset }
        let ids = selectedTrainIDsInDatasetOrder(dataset)
        guard ids.count >= 2 else { return .tooFewTrainsSelected }

        let matrix: NeuralPopulationMatrix
        do {
            matrix = try NeuralPopulationMatrixBuilder.build(
                dataset: dataset, selectedTrainIDs: ids, parameters: parameters
            )
        } catch let error as NeuralPopulationMatrixError {
            return .matrixFailure(error)
        } catch {
            return .matrixFailure(.noValidTrains)
        }

        switch document.neuralManifoldMethod {
        case .pca:
            do {
                let pca = try NeuralPopulationPCA.run(matrix: matrix)
                return .ready(matrix: matrix, embedding: NeuralManifoldEmbedding(
                    method: .pca, scores: pca.scores, pca: pca, isomap: nil, phate: nil, fa: nil
                ))
            } catch let error as NeuralPopulationPCAError {
                return .embeddingFailure(message: l10n.t(message(for: error)))
            } catch {
                return .embeddingFailure(message: l10n.t(message(for: .tooFewTrains)))
            }
        case .isomap:
            // Isomap is computed in the background and cached by signature (RasterDocument). The body never runs
            // it synchronously, so display-only changes don't recompute it; here we only READ the cached state.
            guard let inputs = document.neuralManifoldIsomapInputs else {
                return .embeddingFailure(message: l10n.t("无法计算 Isomap 嵌入。"))
            }
            if let result = document.neuralManifoldIsomapState.result(for: inputs) {
                // Map bin-aligned Isomap points onto the shared score shape (NaN coords are filtered by the plot
                // when the missing axis is selected — e.g. NM3 in a 2-D embedding).
                let scores = result.points.map { point in
                    NeuralPopulationPCAScore(
                        binID: point.bin.binID,
                        startSec: point.bin.startSec, endSec: point.bin.endSec,
                        midSec: point.bin.midSec, widthSec: point.bin.widthSec,
                        nm1: point.nm1 ?? .nan, nm2: point.nm2 ?? .nan, nm3: point.nm3 ?? .nan
                    )
                }
                return .ready(matrix: matrix, embedding: NeuralManifoldEmbedding(
                    method: .isomap, scores: scores, pca: nil, isomap: result.diagnostics, phate: nil, fa: nil
                ))
            }
            if let failure = document.neuralManifoldIsomapState.failure(for: inputs) {
                return .embeddingFailure(message: isomapFailureMessage(failure))
            }
            // idle / computing / a stale prior signature -> the background task is (re)computing it.
            return .embeddingComputing(matrix: matrix)
        case .phate:
            // PHATE is computed in the background and cached by signature (RasterDocument), exactly like Isomap;
            // the body never runs it synchronously, so display-only changes don't recompute it.
            guard let inputs = document.neuralManifoldPhateInputs else {
                return .embeddingFailure(message: l10n.t("无法计算 PHATE 嵌入。"))
            }
            if let result = document.neuralManifoldPhateState.result(for: inputs) {
                // Map bin-aligned PHATE points onto the shared score shape (PHATE always fills NM1–NM3, padding
                // non-positive dimensions with 0 per R `stpd_neural_take3`).
                let scores = result.points.map { point in
                    NeuralPopulationPCAScore(
                        binID: point.bin.binID,
                        startSec: point.bin.startSec, endSec: point.bin.endSec,
                        midSec: point.bin.midSec, widthSec: point.bin.widthSec,
                        nm1: point.nm1, nm2: point.nm2, nm3: point.nm3
                    )
                }
                return .ready(matrix: matrix, embedding: NeuralManifoldEmbedding(
                    method: .phate, scores: scores, pca: nil, isomap: nil, phate: result.diagnostics, fa: nil
                ))
            }
            if let failure = document.neuralManifoldPhateState.failure(for: inputs) {
                return .embeddingFailure(message: phateFailureMessage(failure))
            }
            return .embeddingComputing(matrix: matrix)
        case .fa:
            // FA runs synchronously on the full matrix (like PCA): low feature count, no O(n³) geodesics, and it
            // ignores `max embedded bins` (every bin is embedded). No background cache.
            return factorOutcome(matrix: matrix, method: .fa, smoothingSigmaBins: nil)
        case .gpfa:
            // GPFA-style = FA with an extra Gaussian pre-smoothing pass over `matrix.scaled`, using the same
            // smoothing σ (R parity: `X_fa <- pop$X` is smoothed again on top of the already-smoothed `pop$X`).
            return factorOutcome(
                matrix: matrix, method: .gpfa, smoothingSigmaBins: document.neuralManifoldSmoothingSigmaBins
            )
        }
    }

    /// NM-3D: run FA / GPFA-style synchronously and map the bin-aligned factor scores onto the shared plot shape.
    private func factorOutcome(
        matrix: NeuralPopulationMatrix, method: NeuralManifoldMethod, smoothingSigmaBins: Double?
    ) -> NeuralManifoldOutcome {
        do {
            let fa = try NeuralPopulationFactorAnalysis.run(matrix: matrix, smoothingSigmaBins: smoothingSigmaBins)
            // Factor scores always fill NM1–NM3 (the service zero-pads absent factors per R `stpd_neural_take3`).
            let scores = fa.points.map { point in
                NeuralPopulationPCAScore(
                    binID: point.bin.binID,
                    startSec: point.bin.startSec, endSec: point.bin.endSec,
                    midSec: point.bin.midSec, widthSec: point.bin.widthSec,
                    nm1: point.nm1, nm2: point.nm2, nm3: point.nm3
                )
            }
            return .ready(matrix: matrix, embedding: NeuralManifoldEmbedding(
                method: method, scores: scores, pca: nil, isomap: nil, phate: nil, fa: fa
            ))
        } catch let error as NeuralPopulationFactorError {
            return .embeddingFailure(message: factorMessage(error))
        } catch {
            return .embeddingFailure(message: factorMessage(.fitFailed))
        }
    }

    /// Localized message for a factor-analysis failure (mirrors the R `stop(...)` guards).
    private func factorMessage(_ error: NeuralPopulationFactorError) -> String {
        switch error {
        case .tooFewBins:
            return l10n.t("FA 至少需要三个群体时间分箱。")
        case .tooFewFeatures:
            return l10n.t("FA 至少需要三个有效神经元特征。")
        case .notIdentified:
            return l10n.t("分箱过少，无法识别该因子数的 FA 模型。")
        case .fitFailed:
            return l10n.t("无法拟合因子分析模型。")
        }
    }

    /// Localize a background Isomap failure (matrix-build or Isomap guard), reusing the existing message maps.
    private func isomapFailureMessage(_ failure: NeuralManifoldIsomapFailure) -> String {
        switch failure {
        case .matrix(let error): return l10n.t(message(for: error))
        case .isomap(let error): return isomapMessage(error)
        }
    }

    /// Localize a background PHATE failure (matrix-build or PHATE guard).
    private func phateFailureMessage(_ failure: NeuralManifoldPhateFailure) -> String {
        switch failure {
        case .matrix(let error): return l10n.t(message(for: error))
        case .phate(let error): return phateMessage(error)
        }
    }

    /// Localized message for a PHATE failure (mirrors the R `stop(...)` guards).
    private func phateMessage(_ error: NeuralPopulationPhateError) -> String {
        switch error {
        case .tooFewBins:
            return l10n.t("PHATE 至少需要五个群体时间分箱。")
        case .tooFewNeurons:
            return l10n.t("PHATE 至少需要两个神经元特征。")
        case .degenerate:
            return l10n.t("PHATE 扩散势退化，无法计算嵌入。")
        }
    }

    /// Localized message for an Isomap failure (mirrors the R `stop(...)` guards).
    private func isomapMessage(_ error: NeuralPopulationIsomapError) -> String {
        switch error {
        case .tooFewBins:
            return l10n.t("Isomap 至少需要五个群体时间分箱。")
        case .tooFewNeurons:
            return l10n.t("Isomap 至少需要两个神经元特征。")
        case .disconnectedGraph:
            return l10n.t("Isomap kNN 图不连通。请增大邻居数，或将“断开处理”设为“最大连通分量”。")
        case .componentTooSmall:
            return l10n.t("最大 Isomap 连通分量的分箱过少。请增大邻居数。")
        }
    }

    var body: some View {
        // Compute the matrix + PCA once and feed the header summary, plot, and summary panels.
        let outcome = self.outcome
        return VStack(alignment: .leading, spacing: 0) {
            header(outcome)
                .padding(.horizontal, 16)
                .padding(.vertical, 12)

            Divider()

            content(outcome)
        }
        .frame(minWidth: 640)
        // Kick off (or reuse) the cached background Isomap whenever the embedding signature changes. Display-only
        // state is not part of the signature, so toggling color/axes/2-D-3-D/line/legend never recomputes Isomap.
        .task(id: document.neuralManifoldIsomapInputs) {
            if let inputs = document.neuralManifoldIsomapInputs {
                document.requestNeuralManifoldIsomap(inputs)
            }
        }
        // Same pattern for PHATE (NM-3C); the signature is nil unless PHATE is the active method, so the two
        // background caches are mutually exclusive by method and never recompute on display-only changes.
        .task(id: document.neuralManifoldPhateInputs) {
            if let inputs = document.neuralManifoldPhateInputs {
                document.requestNeuralManifoldPhate(inputs)
            }
        }
    }

    // MARK: - Header + controls

    private func header(_ outcome: NeuralManifoldOutcome) -> some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .center, spacing: 14) {
                VStack(alignment: .leading, spacing: 4) {
                    HStack(alignment: .firstTextBaseline, spacing: 10) {
                        Text(l10n.t("神经流形"))
                            .font(.headline)
                        Text(l10n.t("实时"))
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.secondary)
                            .padding(.horizontal, 8)
                            .padding(.vertical, 3)
                            .background(.quaternary, in: Capsule())
                        Text(document.neuralManifoldMethod.title)
                            .font(.caption2.weight(.semibold))
                            .foregroundStyle(.secondary)
                            .padding(.horizontal, 6)
                            .padding(.vertical, 2)
                            .background(.quaternary, in: Capsule())
                    }

                    Text(headerSummary(outcome))
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                }

                Spacer(minLength: 16)

                if let dataset = document.dataset {
                    SpikeTrainCountControls(
                        document: document,
                        dataset: dataset,
                        scope: .neuralManifold,
                        showsTitle: true,
                        titleWidth: 96
                    )
                }
            }

            if document.dataset != nil {
                controls
            }
        }
    }

    /// Controls grouped by function into three rows with two cells per row. Segmented controls keep
    /// their natural size so the glass backgrounds never overlap neighboring controls.
    private var controls: some View {
        VStack(alignment: .leading, spacing: 12) {
            controlGridRow(l10n.t("方法 / 嵌入")) {
                controlCell(l10n.t("嵌入方法")) {
                    methodSelector
                }
                .help(l10n.t("选择降维方法：PCA（线性基线）、Isomap（测地流形）、PHATE（扩散势）、FA（因子分析）或 GPFA（平滑因子轨迹）。坐标始终来自群体活动矩阵，从不使用检测标签。"))
            } trailing: {
                controlCell(embeddingParametersTitle) {
                    switch document.neuralManifoldMethod {
                    case .isomap:
                        isomapControls
                    case .phate:
                        phateControls
                    case .pca:
                        Text(l10n.t("PCA 无需额外参数。"))
                            .font(.caption2)
                            .foregroundStyle(.secondary)
                    case .fa:
                        Text(l10n.t("FA 无需额外参数。"))
                            .font(.caption2)
                            .foregroundStyle(.secondary)
                    case .gpfa:
                        Text(l10n.t("GPFA 使用上方的平滑 σ 进行额外的 FA 预平滑。"))
                            .font(.caption2)
                            .foregroundStyle(.secondary)
                            .fixedSize(horizontal: false, vertical: true)
                    }
                }
                .help(l10n.t("Isomap：kNN 邻居数 k 与断开处理；PHATE：扩散邻居数与扩散时间 t；FA / GPFA：无额外参数（GPFA 复用平滑 σ）。最大嵌入分箱仅用于 Isomap/PHATE，FA/GPFA 嵌入全部分箱。"))
            }

            controlGridRow(l10n.t("视图 / 坐标轴")) {
                controlCell(l10n.t("显示与样式")) {
                    ViewThatFits(in: .horizontal) {
                        HStack(spacing: 14) {
                            displaySelector
                            styleToggles
                            colorSelector
                        }
                        VStack(alignment: .leading, spacing: 8) {
                            displaySelector
                            styleToggles
                            colorSelector
                        }
                    }
                }
                .help(l10n.t("切换 2D / 3D 显示，开关轨迹连线 / 时间渐变，并选择着色方式：时间、自动模式或最终复核模式（bin 按主导检测状态着色）。PCA 不变，仅绘制方式改变。"))
            } trailing: {
                controlCell(l10n.t("坐标轴")) {
                    ViewThatFits(in: .horizontal) {
                        HStack(spacing: 14) {
                            axisSelector("X", selection: $document.neuralManifoldXAxis)
                            axisSelector("Y", selection: $document.neuralManifoldYAxis)
                            if document.neuralManifoldDisplayMode == .threeD {
                                axisSelector("Z", selection: $document.neuralManifoldZAxis)
                            }
                        }
                        VStack(alignment: .leading, spacing: 8) {
                            axisSelector("X", selection: $document.neuralManifoldXAxis)
                            axisSelector("Y", selection: $document.neuralManifoldYAxis)
                            if document.neuralManifoldDisplayMode == .threeD {
                                axisSelector("Z", selection: $document.neuralManifoldZAxis)
                            }
                        }
                    }
                }
                .help(l10n.t("坐标轴可使用 PC1/PC2/PC3 或时间（群体 bin 中点）。2D 使用 X/Y；3D 使用 X/Y/Z。"))
            }

            controlGridRow(l10n.t("时间 / 分箱")) {
                controlCell(l10n.t("分箱与原点")) {
                    ViewThatFits(in: .horizontal) {
                        HStack(spacing: 14) {
                            numberControl(l10n.t("分箱"), suffix: "ms") {
                                DebouncedDoubleField("50", value: $document.neuralManifoldBinMs, width: 64, maxFractionDigits: 3)
                            }
                            originSelector
                        }
                        VStack(alignment: .leading, spacing: 8) {
                            numberControl(l10n.t("分箱"), suffix: "ms") {
                                DebouncedDoubleField("50", value: $document.neuralManifoldBinMs, width: 64, maxFractionDigits: 3)
                            }
                            originSelector
                        }
                    }
                }
                .help(l10n.t("群体 bin 宽度（R bin_sec，默认 50 ms / 0.05 s）。bin 越小时间分辨率越高，但每个 bin 的发放率噪声更大、bin 数更多。"))
            } trailing: {
                controlCell(l10n.t("平滑与连线范围")) {
                    ViewThatFits(in: .horizontal) {
                        HStack(spacing: 14) {
                            numberControl("σ", suffix: "bins") {
                                DebouncedDoubleField("1", value: $document.neuralManifoldSmoothingSigmaBins, width: 56, maxFractionDigits: 3)
                            }
                            lineRangeControl
                        }
                        VStack(alignment: .leading, spacing: 8) {
                            numberControl("σ", suffix: "bins") {
                                DebouncedDoubleField("1", value: $document.neuralManifoldSmoothingSigmaBins, width: 56, maxFractionDigits: 3)
                            }
                            lineRangeControl
                        }
                    }
                }
                .help(l10n.t("高斯平滑 sigma（单位 bin，R smoothing_sigma_bins）；0 表示禁用。连线范围控制用于连接轨迹点的时间区间。0→0 表示连接整段记录。"))
            }

            controlGridRow(l10n.t("信号 / PCA 输入")) {
                controlCell(l10n.t("变换")) {
                    GlassSegmentedControl(
                        options: [(.sqrtCount, "√count"), (.log1pRate, "log1p"), (.rate, "rate"), (.count, "count")],
                        selection: $document.neuralManifoldTransform,
                        minSegmentWidth: 44
                    )
                    .fixedSize(horizontal: true, vertical: false)
                }
                .help(l10n.t("每个 bin 的信号变换（R transform）。sqrt(count + 3/8) 稳定计数方差；log1p(rate) 压缩高发放率；rate 为 Hz；count 为原始计数。"))
            } trailing: {
                controlCell(l10n.t("缩放与重置")) {
                    ViewThatFits(in: .horizontal) {
                        HStack(spacing: 14) {
                            scalingSelector
                            resetButton
                        }
                        VStack(alignment: .leading, spacing: 8) {
                            scalingSelector
                            resetButton
                        }
                    }
                }
                .help(l10n.t("PCA 前的每神经元列缩放（R scaling）。z-score 和 robust 使各神经元可比；none 保留原始变换单位。PCA 在缩放后的矩阵上运行。"))
            }
        }
    }

    private func controlGridRow<Leading: View, Trailing: View>(
        _ title: String,
        @ViewBuilder leading: () -> Leading,
        @ViewBuilder trailing: () -> Trailing
    ) -> some View {
        HStack(alignment: .top, spacing: 16) {
            Text(title)
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
                .frame(width: 104, alignment: .leading)
                .padding(.top, 6)
            ViewThatFits(in: .horizontal) {
                HStack(alignment: .top, spacing: 18) {
                    leading()
                        .frame(minWidth: 260, alignment: .leading)
                    trailing()
                        .frame(minWidth: 260, alignment: .leading)
                }
                VStack(alignment: .leading, spacing: 10) {
                    leading()
                    trailing()
                }
            }
            Spacer(minLength: 0)
        }
    }

    private func controlCell<Content: View>(
        _ title: String,
        @ViewBuilder content: () -> Content
    ) -> some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(title)
                .font(.caption2.weight(.medium))
                .foregroundStyle(.secondary)
            content()
        }
        .fixedSize(horizontal: true, vertical: false)
    }

    private func axisSelector(_ label: String, selection: Binding<NeuralManifoldPCAAxis>) -> some View {
        HStack(spacing: 8) {
            Text(label)
                .foregroundStyle(.secondary)
                GlassSegmentedControl(
                options: NeuralManifoldPCAAxis.allCases.map { ($0, axisTitle($0)) },
                selection: selection,
                minSegmentWidth: 40
            )
            .fixedSize(horizontal: true, vertical: false)
        }
    }

    private var displaySelector: some View {
        GlassSegmentedControl(
            options: [(.twoD, "2D"), (.threeD, "3D")],
            selection: $document.neuralManifoldDisplayMode,
            minSegmentWidth: 40
        )
        .fixedSize(horizontal: true, vertical: false)
    }

    private var styleToggles: some View {
        HStack(spacing: 12) {
            Toggle(l10n.t("连线"), isOn: $document.neuralManifoldShowsTrajectoryLine)
            Toggle(l10n.t("坐标轴"), isOn: $document.neuralManifoldShowsAxes)
            Toggle(l10n.t("时间着色"), isOn: $document.neuralManifoldUsesTimeGradient)
        }
        .font(.caption)
        .toggleStyle(.switch)
        .controlSize(.small)
        .fixedSize(horizontal: true, vertical: false)
    }

    private var originSelector: some View {
        HStack(spacing: 8) {
            Text(l10n.t("原点"))
                .foregroundStyle(.secondary)
            GlassSegmentedControl(
                options: [(.raw, "Raw"), (.aligned, "Aligned")],
                selection: $document.neuralManifoldTimeOrigin,
                minSegmentWidth: 52
            )
            .fixedSize(horizontal: true, vertical: false)
        }
    }

    private var scalingSelector: some View {
        GlassSegmentedControl(
            options: [(.zscore, "z-score"), (.robust, "robust"), (.none, "none")],
            selection: $document.neuralManifoldScaling,
            minSegmentWidth: 50
        )
        .fixedSize(horizontal: true, vertical: false)
    }

    private var resetButton: some View {
        Button {
            resetParameters()
        } label: {
            Label(l10n.t("重置"), systemImage: "arrow.counterclockwise")
        }
        .font(.caption)
        .liquidGlassButtonStyle()
        .help(l10n.t("将流形参数重置为 R 默认值（50 ms，raw，√count，z-score，σ = 1）。不会改变训练选择。"))
    }

    private func numberControl<Field: View>(
        _ label: String,
        suffix: String,
        @ViewBuilder field: () -> Field
    ) -> some View {
        HStack(spacing: 8) {
            Text(label)
                .foregroundStyle(.secondary)
            field()
            Text(suffix)
                .foregroundStyle(.secondary)
        }
        .fixedSize(horizontal: true, vertical: false)
    }

    private var lineRangeControl: some View {
        HStack(spacing: 8) {
            Text("Line")
                .foregroundStyle(.secondary)
            DebouncedDoubleField(
                "0",
                value: nonNegativeBinding($document.neuralManifoldLineStartSec),
                width: 54,
                maxFractionDigits: 3
            )
            Text("→")
                .foregroundStyle(.secondary)
            DebouncedDoubleField(
                "0",
                value: nonNegativeBinding($document.neuralManifoldLineEndSec),
                width: 54,
                maxFractionDigits: 3
            )
            Text("s")
                .foregroundStyle(.secondary)
        }
        .fixedSize(horizontal: true, vertical: false)
    }

    private func nonNegativeBinding(_ binding: Binding<Double>) -> Binding<Double> {
        Binding(
            get: { binding.wrappedValue },
            set: { value in
                binding.wrappedValue = value.isFinite ? max(0, value) : 0
            }
        )
    }

    private func resetParameters() {
        document.neuralManifoldBinMs = 50
        document.neuralManifoldTimeOrigin = .raw
        document.neuralManifoldTransform = .sqrtCount
        document.neuralManifoldScaling = .zscore
        document.neuralManifoldSmoothingSigmaBins = 1
        document.neuralManifoldXAxis = .nm1
        document.neuralManifoldYAxis = .nm2
        document.neuralManifoldZAxis = .nm3
        document.neuralManifoldDisplayMode = .twoD
        document.neuralManifoldMethod = .pca
        document.neuralManifoldIsomapNeighbors = 15
        document.neuralManifoldMaxEmbeddedBins = 1200
        document.neuralManifoldIsomapComponentMode = .largest
        document.neuralManifoldDiffusionTime = 3
        document.neuralManifoldColorMode = .time
        document.neuralManifoldShowsAxes = true
        document.neuralManifoldShowsTrajectoryLine = true
        document.neuralManifoldLineStartSec = 0
        document.neuralManifoldLineEndSec = 0
        document.neuralManifoldUsesTimeGradient = true
    }

    // MARK: - Content

    @ViewBuilder
    private func content(_ outcome: NeuralManifoldOutcome) -> some View {
        if case .noDataset = outcome {
            ContentUnavailableView(
                l10n.t("神经流形未加载"),
                systemImage: "rotate.3d",
                description: Text(document.lastErrorMessage ?? l10n.t("先加载 spike train 数据集。"))
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else {
            HStack(alignment: .top, spacing: 0) {
                plotPanel(outcome)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)

                Divider()

                ScrollView {
                    summaryColumn(outcome)
                        .padding(14)
                }
                .frame(width: 300)
            }
        }
    }

    @ViewBuilder
    private func plotPanel(_ outcome: NeuralManifoldOutcome) -> some View {
        switch outcome {
        case let .ready(matrix, embedding):
            VStack(alignment: .leading, spacing: 6) {
                HStack(alignment: .firstTextBaseline, spacing: 10) {
                    Text(plotTitle)
                        .font(.headline)
                    Text(embeddingCaption(embedding))
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .monospacedDigit()
                    Spacer()
                }
                .padding(.horizontal, 12)
                .padding(.top, 10)

                ZStack {
                    if document.neuralManifoldDisplayMode == .threeD {
                        NeuralManifoldScene3DView(
                            scores: embedding.scores,
                            method: embedding.method,
                            xAxis: document.neuralManifoldXAxis,
                            yAxis: document.neuralManifoldYAxis,
                            zAxis: document.neuralManifoldZAxis,
                            showsAxes: document.neuralManifoldShowsAxes,
                            showsTrajectoryLine: document.neuralManifoldShowsTrajectoryLine,
                            lineStartSec: document.neuralManifoldLineStartSec,
                            lineEndSec: document.neuralManifoldLineEndSec,
                            usesTimeGradient: document.neuralManifoldUsesTimeGradient,
                            binColors: binPatternColors(matrix),
                            selectedBinID: $selected3DBinID
                        )
                    } else {
                        NeuralManifoldScatterCanvas(
                            scores: embedding.scores,
                            method: embedding.method,
                            xAxis: document.neuralManifoldXAxis,
                            yAxis: document.neuralManifoldYAxis,
                            showsAxes: document.neuralManifoldShowsAxes,
                            showsTrajectoryLine: document.neuralManifoldShowsTrajectoryLine,
                            lineStartSec: document.neuralManifoldLineStartSec,
                            lineEndSec: document.neuralManifoldLineEndSec,
                            usesTimeGradient: document.neuralManifoldUsesTimeGradient,
                            binColors: binPatternColors(matrix),
                            binInfo: binHoverInfo(matrix),
                            timeOrigin: matrix.timeOrigin
                        )
                    }
                }
                .overlay(alignment: .topTrailing) {
                    plotLegend
                        .padding(.top, 10)
                        .padding(.trailing, 18)
                }
                .overlay(alignment: .topLeading) {
                    // 3-D click-to-inspect card: fixed corner (stable while the camera rotates), shown only
                    // when a bin is selected. Built from the SAME binHoverInfo + shared builder as 2-D.
                    if document.neuralManifoldDisplayMode == .threeD,
                       let binID = selected3DBinID,
                       let score = embedding.scores.first(where: { $0.binID == binID }) {
                        NeuralManifoldHoverCard(
                            target: neuralManifoldHoverTarget(
                                for: score,
                                method: embedding.method,
                                xAxis: document.neuralManifoldXAxis,
                                yAxis: document.neuralManifoldYAxis,
                                zAxis: document.neuralManifoldZAxis,
                                binInfo: binHoverInfo(matrix),
                                timeOrigin: matrix.timeOrigin,
                                l10n: l10n
                            )
                        )
                        .frame(width: 248, alignment: .leading)
                        .padding(.top, 10)
                        .padding(.leading, 14)
                    }
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .padding(.horizontal, 12)
                .padding(.bottom, 4)
                .onChange(of: document.neuralManifoldDisplayMode) { _, _ in
                    selected3DBinID = nil   // a stale 3-D selection should not carry across a 2-D/3-D switch
                }

                Text(plotCaption)
                    .font(.caption2)
                    .foregroundStyle(.secondary)
                    .padding(.horizontal, 12)
                    .padding(.bottom, 10)
                    .fixedSize(horizontal: false, vertical: true)
            }
        case .tooFewTrainsSelected:
            unavailable(l10n.t("选择不足"), l10n.t("请选择至少两条 spike train 才能构建群体流形。"), system: "checklist")
        case let .matrixFailure(error):
            unavailable(l10n.t("暂无法构建矩阵"), l10n.t(message(for: error)), system: "exclamationmark.triangle")
        case let .embeddingFailure(message):
            unavailable(l10n.t("暂无法计算嵌入"), message, system: "exclamationmark.triangle")
        case .embeddingComputing:
            VStack(spacing: 10) {
                ProgressView()
                    .controlSize(.large)
                Text(embeddingComputingMessage)
                    .font(.callout)
                    .foregroundStyle(.secondary)
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        case .noDataset:
            EmptyView()
        }
    }

    private func unavailable(_ title: String, _ message: String, system: String) -> some View {
        ContentUnavailableView(title, systemImage: system, description: Text(message))
            .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func patternSwatchRow(_ states: [SpikePatternState]) -> some View {
        HStack(spacing: 8) {
            ForEach(states, id: \.self) { state in
                HStack(spacing: 3) {
                    Circle().fill(NeuralManifoldView.patternColor(state)).frame(width: 8, height: 8)
                    Text(l10n.t(state.localizationSourceZH)).font(.caption2).foregroundStyle(.secondary)
                }
            }
        }
    }

    /// Task-event timing context for the legend (count + raw-time span). Annotation overlay only — task
    /// events are never binned into the population matrix or used as a PCA input (see `TaskEvent`). Shown
    /// in both 2-D and 3-D because the legend overlays the shared plot group.
    @ViewBuilder
    private var eventLegendRow: some View {
        let times = document.taskEvents.map(\.timeSec).filter(\.isFinite).sorted()
        HStack(spacing: 6) {
            Image(systemName: "mappin.and.ellipse")
                .font(.caption2)
                .foregroundStyle(.secondary)
            Text(l10n.t("事件"))
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(eventLegendText(count: document.taskEvents.count, times: times))
                .font(.caption2)
                .foregroundStyle(.secondary)
                .monospacedDigit()
        }
        .help(l10n.t("任务 / 刺激事件的时间上下文（原始时间）。事件仅为标注层——它们绝不进入群体 bin 或 PCA。"))
    }

    private func eventLegendText(count: Int, times: [Double]) -> String {
        guard let first = times.first, let last = times.last else { return "\(count)" }
        if times.count == 1 || abs(last - first) < 1e-9 {
            return "\(count) @ \(formatSeconds(first)) s · raw"
        }
        return "\(count) · \(formatSeconds(first))–\(formatSeconds(last)) s · raw"
    }

    private var plotLegend: some View {
        VStack(alignment: .leading, spacing: 7) {
            HStack(spacing: 8) {
                Text("Axes")
                    .font(.caption2.weight(.semibold))
                    .foregroundStyle(.secondary)
                Text(axisLegendText)
                    .font(.caption2)
                    .monospacedDigit()
                    .lineLimit(1)
            }

            HStack(spacing: 8) {
                Text("Line")
                    .font(.caption2.weight(.semibold))
                    .foregroundStyle(.secondary)
                Text(document.neuralManifoldShowsTrajectoryLine ? lineRangeLegendText : l10n.t("关闭"))
                    .font(.caption2)
                    .monospacedDigit()
            }

            if document.neuralManifoldColorMode.isPattern {
                VStack(alignment: .leading, spacing: 3) {
                    Text(document.neuralManifoldColorMode == .autoPattern ? l10n.t("自动模式") : l10n.t("最终模式"))
                        .font(.caption2.weight(.semibold))
                        .foregroundStyle(.secondary)
                    patternSwatchRow(Array(SpikePatternColor.legendStates.prefix(3)))
                    patternSwatchRow(Array(SpikePatternColor.legendStates.dropFirst(3)))
                }
            } else if document.neuralManifoldUsesTimeGradient {
                HStack(spacing: 7) {
                    Text(l10n.t("时间"))
                        .font(.caption2.weight(.semibold))
                        .foregroundStyle(.secondary)
                    Text(l10n.t("早"))
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                    RoundedRectangle(cornerRadius: 3)
                        .fill(
                            LinearGradient(
                                colors: [Self.timeGradientStart, Self.timeGradientEnd],
                                startPoint: .leading,
                                endPoint: .trailing
                            )
                        )
                        .frame(width: 72, height: 8)
                    Text(l10n.t("晚"))
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                }
            } else {
                HStack(spacing: 7) {
                    Text(l10n.t("颜色"))
                        .font(.caption2.weight(.semibold))
                        .foregroundStyle(.secondary)
                    Circle()
                        .fill(Color.accentColor)
                        .frame(width: 8, height: 8)
                    Text(l10n.t("统一"))
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                }
            }

            if !document.taskEvents.isEmpty {
                eventLegendRow
            }
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 8)
        .neuralManifoldGlassPanel()
    }

    // MARK: - Summary panels

    @ViewBuilder
    private func summaryColumn(_ outcome: NeuralManifoldOutcome) -> some View {
        if case let .ready(matrix, embedding) = outcome {
            VStack(alignment: .leading, spacing: 16) {
                matrixSummary(matrix)
                eventStateSummary(matrix)
                if let pca = embedding.pca {
                    varianceSummary(pca)
                    loadingsSummary(pca)
                }
                if let isomap = embedding.isomap {
                    isomapDiagnosticsSummary(isomap)
                }
                if let phate = embedding.phate {
                    phateDiagnosticsSummary(phate)
                }
                if let fa = embedding.fa {
                    factorDiagnosticsSummary(fa)
                    factorLoadingsSummary(fa)
                }
                Spacer(minLength: 0)
            }
        } else if case let .embeddingComputing(matrix) = outcome {
            // The population matrix + event-state are already built synchronously; only the Isomap embedding is
            // pending on the background task, so keep showing the matrix summary (the diagnostics fill in once
            // the result is `.ready`).
            VStack(alignment: .leading, spacing: 16) {
                matrixSummary(matrix)
                eventStateSummary(matrix)
                Spacer(minLength: 0)
            }
        } else {
            VStack(alignment: .leading, spacing: 8) {
                Text(l10n.t("摘要"))
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Text(l10n.t("调整训练选择或参数后，这里会显示矩阵、方差和载荷摘要。"))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
                    .fixedSize(horizontal: false, vertical: true)
                Spacer(minLength: 0)
            }
        }
    }

    /// Compact event-state occupancy summary for the Final (reviewed) layer: bins-by-dominant-state and the
    /// occupancy fraction. Annotation overlay only — never a PCA input.
    @ViewBuilder
    private func eventStateSummary(_ matrix: NeuralPopulationMatrix) -> some View {
        let layer = eventStateLayer(matrix, source: .final)
        let states = SpikePatternState.displayOrder.filter { (layer.binCountByDominantState[$0] ?? 0) > 0 }
        summaryPanel(l10n.t("事件状态占比 (最终)")) {
            if states.isEmpty {
                Text(l10n.t("尚无最终标注。"))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            } else {
                ForEach(states, id: \.self) { state in
                    HStack(spacing: 6) {
                        Circle()
                            .fill(NeuralManifoldView.patternColor(state))
                            .frame(width: 8, height: 8)
                        Text(l10n.t(state.localizationSourceZH))
                            .foregroundStyle(.secondary)
                        Spacer(minLength: 8)
                        Text("\(percent(layer.occupancyByDominantState[state] ?? 0)) · \(layer.binCountByDominantState[state] ?? 0) bins")
                            .monospacedDigit()
                    }
                    .font(.caption2)
                }
            }
        }
    }

    private func matrixSummary(_ matrix: NeuralPopulationMatrix) -> some View {
        summaryPanel(l10n.t("矩阵 (bins × trains)")) {
            keyValueRow(l10n.t("训练数"), "\(matrix.trainCount)")
            keyValueRow(l10n.t("分箱数"), "\(matrix.binCount)")
            keyValueRow(l10n.t("分箱大小"), String(format: "%.3g ms", matrix.binSec * 1000))
            keyValueRow(l10n.t("变换"), transformLabel(matrix.transform))
            keyValueRow(l10n.t("缩放"), scalingLabel(matrix.scaling))
            keyValueRow(l10n.t("平滑 σ"), String(format: "%.3g bins", matrix.smoothingSigmaBins))
            keyValueRow(l10n.t("时间原点"), timeOriginLabel(matrix.timeOrigin))
            keyValueRow(document.neuralManifoldDisplayMode == .threeD ? l10n.t("坐标轴") : l10n.t("平面"), planeLabel)
        }
    }

    /// Compact Isomap diagnostics panel (R `stpd_neural_generic_isomap` diagnostics). Shown when the active
    /// embedding is Isomap, in place of the PCA variance/loadings panels.
    private func isomapDiagnosticsSummary(_ diagnostics: NeuralPopulationIsomapDiagnostics) -> some View {
        summaryPanel(l10n.t("Isomap 诊断")) {
            keyValueRow(l10n.t("邻居数"), "\(diagnostics.neighborCount)")
            keyValueRow(l10n.t("嵌入分箱"), "\(diagnostics.embeddedCount) / \(diagnostics.inputBinCount)")
            keyValueRow(l10n.t("最大嵌入分箱"), "\(document.neuralManifoldMaxEmbeddedBins)")
            keyValueRow(l10n.t("连通分量"), "\(diagnostics.componentCount)")
            keyValueRow(l10n.t("神经元特征"), "\(diagnostics.featureNeuronCount)")
            if let residual = diagnostics.residualVariance {
                keyValueRow(l10n.t("残差方差"), String(format: "%.3f", residual))
            }
            if diagnostics.sampled {
                keyValueRow(l10n.t("采样分箱"), "\(diagnostics.sampledCount)")
            }
        }
    }

    /// Compact PHATE diagnostics panel (R `stpd_neural_generic_phate` diffusion-potential fallback). Shown when
    /// the active embedding is PHATE, in place of the PCA variance/loadings panels.
    private func phateDiagnosticsSummary(_ diagnostics: NeuralPopulationPhateDiagnostics) -> some View {
        summaryPanel(l10n.t("PHATE 诊断")) {
            keyValueRow(l10n.t("邻居数"), "\(diagnostics.neighborCount)")
            keyValueRow(l10n.t("扩散时间"), "\(diagnostics.diffusionTime)")
            keyValueRow(l10n.t("嵌入分箱"), "\(diagnostics.embeddedCount) / \(diagnostics.inputBinCount)")
            keyValueRow(l10n.t("最大嵌入分箱"), "\(document.neuralManifoldMaxEmbeddedBins)")
            keyValueRow(l10n.t("核带宽 ε"), String(format: "%.3g", diagnostics.kernelEpsilon))
            keyValueRow(l10n.t("神经元特征"), "\(diagnostics.featureNeuronCount)")
            keyValueRow(l10n.t("后端"), l10n.t("扩散势 + 经典 MDS"))
            if diagnostics.sampled {
                keyValueRow(l10n.t("采样分箱"), "\(diagnostics.sampledCount)")
            }
        }
    }

    /// Compact FA / GPFA-style diagnostics panel (R `stpd_run_neural_manifold_embedding` FA/GPFA branch). Shown
    /// when the active embedding is FA or GPFA-style, in place of the PCA variance panel. `max embedded bins` is
    /// intentionally NOT shown — FA/GPFA embed every population bin.
    private func factorDiagnosticsSummary(_ result: NeuralPopulationFactorResult) -> some View {
        let d = result.diagnostics
        return summaryPanel(l10n.t("因子分析诊断")) {
            keyValueRow(l10n.t("方法"), d.methodLabel)
            keyValueRow(l10n.t("分箱数"), "\(d.inputBinCount)")
            keyValueRow(l10n.t("输入特征"), "\(d.inputFeatureCount)")
            keyValueRow(l10n.t("保留特征"), "\(d.keptFeatureCount)")
            if d.droppedFeatureCount > 0 {
                keyValueRow(l10n.t("丢弃特征"), "\(d.droppedFeatureCount)")
            }
            keyValueRow(l10n.t("因子数"), "\(d.factorCount)")
            keyValueRow(l10n.t("平均唯一度"), String(format: "%.3f", d.meanUniqueness))
            if d.smoothed {
                keyValueRow(l10n.t("平滑 σ"), String(format: "%.3g bins", Swift.max(1, document.neuralManifoldSmoothingSigmaBins)))
            }
        }
    }

    /// Compact top factor-loadings table (per kept train: F1–F3 + uniqueness), analogous to the PCA loadings panel.
    private func factorLoadingsSummary(_ result: NeuralPopulationFactorResult) -> some View {
        let top = result.loadings
            .sorted { factorMagnitude($0) > factorMagnitude($1) }
            .prefix(8)
        return summaryPanel(l10n.t("Top 载荷 (|F1|+|F2|)")) {
            Grid(alignment: .leading, horizontalSpacing: 8, verticalSpacing: 2) {
                GridRow {
                    Text("train").foregroundStyle(.secondary)
                    Text("F1").foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                    Text("F2").foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                    Text("F3").foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                    Text(l10n.t("唯一度")).foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                }
                .font(.caption2)
                ForEach(Array(top), id: \.trainID) { loading in
                    GridRow {
                        Text(loading.trainName)
                            .lineLimit(1)
                            .truncationMode(.middle)
                        Text(num(loading.f1)).monospacedDigit()
                        Text(num(loading.f2)).monospacedDigit()
                        Text(num(loading.f3)).monospacedDigit()
                        Text(num(loading.uniqueness)).monospacedDigit()
                    }
                    .font(.caption2)
                }
            }
        }
    }

    private func factorMagnitude(_ loading: NeuralPopulationFactorLoading) -> Double {
        abs(loading.f1 ?? 0) + abs(loading.f2 ?? 0)
    }

    private func varianceSummary(_ pca: NeuralPopulationPCAResult) -> some View {
        summaryPanel(l10n.t("方差解释 (PC1–PC3)")) {
            if pca.variance.isEmpty {
                Text(l10n.t("无方差信息。"))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
            } else {
                Grid(alignment: .leading, horizontalSpacing: 10, verticalSpacing: 2) {
                    GridRow {
                        Text("PC").foregroundStyle(.secondary)
                        Text(l10n.t("占比")).foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                        Text(l10n.t("累计")).foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                    }
                    .font(.caption2)
                    ForEach(pca.variance, id: \.component) { row in
                        GridRow {
                            Text(row.component)
                            Text(percent(row.varianceExplained)).monospacedDigit()
                            Text(percent(row.cumulative)).monospacedDigit()
                        }
                        .font(.caption2)
                    }
                }
            }
        }
    }

    private func loadingsSummary(_ pca: NeuralPopulationPCAResult) -> some View {
        let top = pca.loadings
            .sorted { combinedMagnitude($0) > combinedMagnitude($1) }
            .prefix(8)
        return summaryPanel(l10n.t("Top 载荷 (|PC1|+|PC2|)")) {
            Grid(alignment: .leading, horizontalSpacing: 8, verticalSpacing: 2) {
                GridRow {
                    Text("train").foregroundStyle(.secondary)
                    Text("PC1").foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                    Text("PC2").foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                    Text("PC3").foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                }
                .font(.caption2)
                ForEach(Array(top), id: \.trainID) { loading in
                    GridRow {
                        Text(loading.trainName)
                            .lineLimit(1)
                            .truncationMode(.middle)
                        Text(num(loading.pc1)).monospacedDigit()
                        Text(num(loading.pc2)).monospacedDigit()
                        Text(num(loading.pc3)).monospacedDigit()
                    }
                    .font(.caption2)
                }
            }
        }
    }

    private func summaryPanel<Content: View>(
        _ title: String,
        @ViewBuilder content: () -> Content
    ) -> some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(title)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            content()
        }
    }

    private func keyValueRow(_ key: String, _ value: String) -> some View {
        HStack(spacing: 8) {
            Text(key)
                .font(.caption2)
                .foregroundStyle(.secondary)
            Spacer(minLength: 8)
            Text(value)
                .font(.caption2)
                .monospacedDigit()
        }
    }

    // MARK: - Formatting + labels

    private func combinedMagnitude(_ loading: NeuralPopulationPCALoading) -> Double {
        abs(loading.pc1 ?? 0) + abs(loading.pc2 ?? 0)
    }

    private func axisTitle(_ axis: NeuralManifoldPCAAxis) -> String {
        axis.title(for: document.neuralManifoldMethod)
    }

    private var plotTitle: String {
        switch document.neuralManifoldDisplayMode {
        case .twoD:
            return "群体轨迹 (\(axisTitle(document.neuralManifoldXAxis)) · \(axisTitle(document.neuralManifoldYAxis)))"
        case .threeD:
            return "群体轨迹 (\(axisTitle(document.neuralManifoldXAxis)) · \(axisTitle(document.neuralManifoldYAxis)) · \(axisTitle(document.neuralManifoldZAxis)))"
        }
    }

    private var plotCaption: String {
        let line = document.neuralManifoldShowsTrajectoryLine ? "连线连接 \(lineRangeCaptionText) 内的相邻 bin" : "连线关闭"
        let axes = document.neuralManifoldShowsAxes ? "坐标轴显示" : "坐标轴隐藏"
        let color: String
        switch document.neuralManifoldColorMode {
        case .autoPattern:
            color = "点按自动检测状态着色"
        case .finalPattern:
            color = "点按复核（最终）状态着色"
        case .time:
            color = document.neuralManifoldUsesTimeGradient ? "点按时间先后渐变" : "点使用统一颜色"
        }
        let events = document.taskEvents.isEmpty
            ? ""
            : "；\(document.taskEvents.count) 个任务事件（原始时间，仅图例标注，不参与 PCA）"
        switch document.neuralManifoldDisplayMode {
        case .twoD:
            return "\(color)，\(line)，\(axes)\(events)。状态着色为标注叠加层，PCA 输入不变。"
        case .threeD:
            return "X/Y/Z 轴按当前选择绘制；\(color)，\(line)，\(axes)\(events)。拖拽旋转 · 滚轮或双指捏合缩放。"
        }
    }

    private var planeLabel: String {
        switch document.neuralManifoldDisplayMode {
        case .twoD:
            return "\(axisTitle(document.neuralManifoldXAxis)) · \(axisTitle(document.neuralManifoldYAxis))"
        case .threeD:
            return "\(axisTitle(document.neuralManifoldXAxis)) · \(axisTitle(document.neuralManifoldYAxis)) · \(axisTitle(document.neuralManifoldZAxis))"
        }
    }

    private var axisLegendText: String {
        switch document.neuralManifoldDisplayMode {
        case .twoD:
            return "X \(axisTitle(document.neuralManifoldXAxis)) · Y \(axisTitle(document.neuralManifoldYAxis))"
        case .threeD:
            return "X \(axisTitle(document.neuralManifoldXAxis)) · Y \(axisTitle(document.neuralManifoldYAxis)) · Z \(axisTitle(document.neuralManifoldZAxis))"
        }
    }

    private var activeLineRange: (start: Double, end: Double)? {
        let start = max(0, document.neuralManifoldLineStartSec)
        let end = max(0, document.neuralManifoldLineEndSec)
        guard end > start else { return nil }
        return (start, end)
    }

    private var lineRangeLegendText: String {
        guard let range = activeLineRange else { return "full" }
        return "\(formatSeconds(range.start))–\(formatSeconds(range.end)) s"
    }

    private var lineRangeCaptionText: String {
        guard let range = activeLineRange else { return "全时段" }
        return "\(formatSeconds(range.start))–\(formatSeconds(range.end)) s"
    }

    private func headerSummary(_ outcome: NeuralManifoldOutcome) -> String {
        switch outcome {
        case .noDataset:
            return l10n.t("先加载 spike train 数据集。")
        case .tooFewTrainsSelected:
            return l10n.t("选择至少两条 spike train 以构建群体流形。")
        case let .matrixFailure(error):
            return message(for: error)
        case let .embeddingFailure(message):
            return message
        case let .ready(matrix, _), let .embeddingComputing(matrix):
            return "\(matrix.trainCount) trains · \(matrix.binCount) bins @ "
                + String(format: "%.3g ms", matrix.binSec * 1000)
                + " · \(transformLabel(matrix.transform)) · \(scalingLabel(matrix.scaling))"
        }
    }

    private func varianceCaption(_ pca: NeuralPopulationPCAResult) -> String {
        guard !pca.variance.isEmpty else { return l10n.t("无方差信息") }
        let parts = pca.variance.map { "\($0.component) \(percent($0.varianceExplained))" }
        let cumulative = pca.variance.last?.cumulative
        return parts.joined(separator: " · ") + " · cumulative " + percent(cumulative)
    }

    /// Plot-header caption for the active embedding: PCA variance, or a compact Isomap diagnostic line.
    private func embeddingCaption(_ embedding: NeuralManifoldEmbedding) -> String {
        if let pca = embedding.pca { return varianceCaption(pca) }
        if let isomap = embedding.isomap {
            let resid = isomap.residualVariance.map { String(format: "%.3f", $0) } ?? "—"
            let bins = isomap.sampled
                ? "\(isomap.embeddedCount)/\(isomap.inputBinCount) bins"
                : "\(isomap.embeddedCount) bins"
            return "\(bins) · k=\(isomap.neighborCount) · residual var \(resid)"
        }
        if let fa = embedding.fa {
            return "\(fa.diagnostics.factorCount) factors · mean uniqueness "
                + String(format: "%.3f", fa.diagnostics.meanUniqueness)
        }
        return ""
    }

    private func percent(_ ratio: Double?) -> String {
        guard let ratio, ratio.isFinite else { return "—" }
        return (ratio * 100).formatted(.number.precision(.fractionLength(1))) + "%"
    }

    private func num(_ value: Double?) -> String {
        guard let value, value.isFinite else { return "—" }
        return value.formatted(.number.precision(.fractionLength(2)))
    }

    private func formatSeconds(_ value: Double) -> String {
        value.formatted(.number.precision(.fractionLength(0...3)))
    }

    private func transformLabel(_ transform: NeuralPopulationTransform) -> String {
        switch transform {
        case .count: return "count"
        case .rate: return "rate (Hz)"
        case .sqrtCount: return "√count"
        case .log1pRate: return "log1p rate"
        }
    }

    private func scalingLabel(_ scaling: NeuralPopulationScaling) -> String {
        switch scaling {
        case .zscore: return "z-score"
        case .robust: return "robust"
        case .none: return "none"
        }
    }

    private func timeOriginLabel(_ origin: NeuralPopulationTimeOrigin) -> String {
        switch origin {
        case .raw: return "raw"
        case .aligned: return "aligned"
        }
    }

    private static let timeGradientStart = Color(
        red: Double(0x39) / 255,
        green: Double(0x9E) / 255,
        blue: Double(0x65) / 255
    )
    private static let timeGradientEnd = Color(
        red: Double(0xEC) / 255,
        green: Double(0x69) / 255,
        blue: Double(0x65) / 255
    )

    private func message(for error: NeuralPopulationMatrixError) -> String {
        switch error {
        case .noTrainsSelected:
            return l10n.t("请选择至少一条 spike train。")
        case .noValidTrains:
            return l10n.t("所选 spike train 都没有足够（≥2）的 spike，无法分箱。")
        }
    }

    private func message(for error: NeuralPopulationPCAError) -> String {
        switch error {
        case .tooFewBins:
            return l10n.t("有效时间 bin 太少（需要 ≥3 个）。请增大时间范围或减小 bin 宽度。")
        case .tooFewTrains:
            return l10n.t("需要至少两条 spike train 才能构建群体流形。")
        case .tooFewVaryingTrains:
            return l10n.t("至少需要两条有变化的 spike train（去掉常量 / 空 train）。")
        }
    }
}

/// Page state: every empty/error case the UI must render, plus the ready matrix + PCA.
private enum NeuralManifoldOutcome {
    case noDataset
    case tooFewTrainsSelected
    case matrixFailure(NeuralPopulationMatrixError)
    /// The selected embedding (PCA or Isomap) could not be computed; carries a localized message.
    case embeddingFailure(message: String)
    /// Isomap is being computed in the background; the matrix (for the matrix/event-state panels) is ready.
    case embeddingComputing(matrix: NeuralPopulationMatrix)
    case ready(matrix: NeuralPopulationMatrix, embedding: NeuralManifoldEmbedding)
}

/// A computed embedding for the plot: bin-aligned NM1–NM3 scores plus the method-specific summary payload (PCA
/// variance/loadings, or Isomap diagnostics). The 2-D/3-D plot, axis controls, color-by, hover, and event-state
/// layer all key on the shared `NeuralPopulationPCAScore` shape, so Isomap reuses them unchanged.
private struct NeuralManifoldEmbedding {
    let method: NeuralManifoldMethod
    let scores: [NeuralPopulationPCAScore]
    let pca: NeuralPopulationPCAResult?
    let isomap: NeuralPopulationIsomapDiagnostics?
    let phate: NeuralPopulationPhateDiagnostics?
    let fa: NeuralPopulationFactorResult?
}

/// Per-bin annotation context for the neural-manifold 2-D inspector. Display-only metadata:
/// the population matrix and embedding inputs remain unchanged.
struct NeuralManifoldBinHoverInfo: Equatable {
    let autoDominant: SpikePatternState
    let autoDominantFraction: Double
    let autoUnlabeledFraction: Double
    let finalDominant: SpikePatternState
    let finalDominantFraction: Double
    let finalUnlabeledFraction: Double
    let contributingTrains: Int
    let totalTrains: Int
}

/// Population trajectory on a selectable plane (X/Y = method-aware embedding axes or Time), with optional
/// time-gradient coloring and optional trajectory line. X and Y may be the same coordinate (self-comparison diagonal).
private struct NeuralManifoldScatterCanvas: View {
    let scores: [NeuralPopulationPCAScore]
    let method: NeuralManifoldMethod
    let xAxis: NeuralManifoldPCAAxis
    let yAxis: NeuralManifoldPCAAxis
    let showsAxes: Bool
    let showsTrajectoryLine: Bool
    let lineStartSec: Double
    let lineEndSec: Double
    let usesTimeGradient: Bool
    /// Optional per-bin (binID -> color) override for pattern/state coloring (annotation overlay only).
    /// When `nil` or missing a bin, the existing time-gradient / uniform coloring is used.
    var binColors: [Int: Color]? = nil
    /// Optional per-bin context for the hover inspector (annotation overlay only). Drawing is unaffected.
    var binInfo: [Int: NeuralManifoldBinHoverInfo]? = nil
    /// Whether the bin time range is in raw or aligned time (labels the hover's time-range row).
    var timeOrigin: NeuralPopulationTimeOrigin = .raw
    @Environment(\.l10n) private var l10n
    @State private var hoverLocation: CGPoint?

    private func xValue(_ score: NeuralPopulationPCAScore) -> Double { xAxis.value(in: score) }
    private func yValue(_ score: NeuralPopulationPCAScore) -> Double { yAxis.value(in: score) }
    private func axisTitle(_ axis: NeuralManifoldPCAAxis) -> String { axis.title(for: method) }
    private var finitePoints: [NeuralPopulationPCAScore] {
        scores.filter { xValue($0).isFinite && yValue($0).isFinite }
    }

    var body: some View {
        GeometryReader { proxy in
            // Project once from the GeometryReader size and share it with the Canvas draw, so the hover
            // hit-test maps points to the exact pixels they are drawn at (same pattern as ISI State Space P5A).
            let points = finitePoints
            let projection = NeuralManifoldScatterProjection(
                xs: points.map(xValue), ys: points.map(yValue), size: proxy.size
            )
            ZStack(alignment: .topLeading) {
                Canvas { context, _ in
                    guard points.count >= 2, let projection else { return }
                    draw(points: points, projection: projection, into: &context)
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)

                if points.count >= 2, let projection, let location = hoverLocation,
                   let index = nearestIndex(at: location, points: points, projection: projection) {
                    NeuralManifoldHoverCard(target: hoverTarget(for: points[index]))
                        .frame(width: 248, alignment: .leading)
                        .position(neuralManifoldHoverCardPosition(for: location, in: proxy.size, cardWidth: 248, cardHeight: 220))
                        .allowsHitTesting(false)
                }
            }
            .contentShape(Rectangle())
            .onContinuousHover { phase in
                switch phase {
                case .active(let location): hoverLocation = location
                case .ended: hoverLocation = nil
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func draw(points: [NeuralPopulationPCAScore], projection: NeuralManifoldScatterProjection, into context: inout GraphicsContext) {
        let plot = projection.plot
        context.stroke(
            Path(plot),
            with: .color(Color(nsColor: .separatorColor).opacity(0.6)),
            lineWidth: 1
        )

        if showsAxes {
            let dashed = StrokeStyle(lineWidth: 0.75, dash: [3, 3])
            if projection.xLo <= 0, 0 <= projection.xHi {
                let zx = projection.screen(x: 0, y: projection.yLo).x
                var axis = Path()
                axis.move(to: CGPoint(x: zx, y: plot.minY))
                axis.addLine(to: CGPoint(x: zx, y: plot.maxY))
                context.stroke(axis, with: .color(.secondary.opacity(0.3)), style: dashed)
            }
            if projection.yLo <= 0, 0 <= projection.yHi {
                let zy = projection.screen(x: projection.xLo, y: 0).y
                var axis = Path()
                axis.move(to: CGPoint(x: plot.minX, y: zy))
                axis.addLine(to: CGPoint(x: plot.maxX, y: zy))
                context.stroke(axis, with: .color(.secondary.opacity(0.3)), style: dashed)
            }
        }

        // Trajectory line connecting consecutive bins (drawn under the points).
        if showsTrajectoryLine {
            let linePoints = lineFiltered(points)
            if linePoints.count >= 2 {
                var trajectory = Path()
                for (index, point) in linePoints.enumerated() {
                    let cgPoint = projection.screen(x: xValue(point), y: yValue(point))
                    if index == 0 {
                        trajectory.move(to: cgPoint)
                    } else {
                        trajectory.addLine(to: cgPoint)
                    }
                }
                context.stroke(trajectory, with: .color(.secondary.opacity(0.28)), lineWidth: 1.2)
            }
        }

        // Points optionally colored by time progression (bin order).
        let count = points.count
        for (index, point) in points.enumerated() {
            let fraction = count > 1 ? Double(index) / Double(count - 1) : 0
            let p = projection.screen(x: xValue(point), y: yValue(point))
            let rect = CGRect(x: p.x - 2.4, y: p.y - 2.4, width: 4.8, height: 4.8)
            let color = binColors?[point.binID] ?? (usesTimeGradient ? Self.progressionColor(fraction) : Color.accentColor)
            context.fill(Path(ellipseIn: rect), with: .color(color.opacity(0.9)))
        }

        if showsAxes {
            context.draw(
                Text("X \(axisTitle(xAxis))").font(.caption2.weight(.medium)).foregroundStyle(.secondary),
                at: CGPoint(x: plot.maxX - 26, y: plot.maxY + 10)
            )
            context.draw(
                Text("Y \(axisTitle(yAxis))").font(.caption2.weight(.medium)).foregroundStyle(.secondary),
                at: CGPoint(x: plot.minX + 24, y: plot.minY + 8)
            )
        }
    }

    private func nearestIndex(at location: CGPoint, points: [NeuralPopulationPCAScore], projection: NeuralManifoldScatterProjection) -> Int? {
        let positions = points.map { point -> ISIStateSpaceScreenPoint in
            let screen = projection.screen(x: xValue(point), y: yValue(point))
            return ISIStateSpaceScreenPoint(x: Double(screen.x), y: Double(screen.y))
        }
        return ISIStateSpaceHitTesting.nearestIndex(
            positions: positions,
            to: ISIStateSpaceScreenPoint(x: Double(location.x), y: Double(location.y)),
            maxDistance: 12
        )
    }

    private func hoverTarget(for score: NeuralPopulationPCAScore) -> NeuralManifoldHoverTarget {
        // 2-D has no Z axis (zAxis: nil), so the shared builder produces exactly the prior 2-D rows.
        neuralManifoldHoverTarget(
            for: score,
            method: method,
            xAxis: xAxis,
            yAxis: yAxis,
            zAxis: nil,
            binInfo: binInfo,
            timeOrigin: timeOrigin,
            l10n: l10n
        )
    }

    private func lineFiltered(_ points: [NeuralPopulationPCAScore]) -> [NeuralPopulationPCAScore] {
        let start = max(0, lineStartSec)
        let end = max(0, lineEndSec)
        guard end > start else { return points }
        return points.filter { start <= $0.midSec && $0.midSec <= end }
    }

    /// Green (#399E65) → coral (#EC6965) gradient encoding bin order (time). No detector-state semantics.
    private static func progressionColor(_ fraction: Double) -> Color {
        let t = max(0, min(1, fraction))
        let start = (r: Double(0x39) / 255, g: Double(0x9E) / 255, b: Double(0x65) / 255)
        let end = (r: Double(0xEC) / 255, g: Double(0x69) / 255, b: Double(0x65) / 255)
        return Color(
            red: start.r + (end.r - start.r) * t,
            green: start.g + (end.g - start.g) * t,
            blue: start.b + (end.b - start.b) * t
        )
    }
}

struct NeuralManifoldHoverTarget {
    let title: String
    let rows: [(label: String, value: String)]
}

/// Builds the hover/inspect card rows for a population bin, shared by the 2-D canvas hover and the 3-D
/// click-to-inspect overlay so both render identical content from the SAME `binHoverInfo`. `zAxis` is `nil`
/// for 2-D (no Z row) and the Z axis for 3-D. Pattern labels go through the existing localization helper;
/// bin id and numbers are not translated. Annotation/display only.
func neuralManifoldHoverTarget(
    for score: NeuralPopulationPCAScore,
    method: NeuralManifoldMethod,
    xAxis: NeuralManifoldPCAAxis,
    yAxis: NeuralManifoldPCAAxis,
    zAxis: NeuralManifoldPCAAxis?,
    binInfo: [Int: NeuralManifoldBinHoverInfo]?,
    timeOrigin: NeuralPopulationTimeOrigin,
    l10n: STPDLocalizer
) -> NeuralManifoldHoverTarget {
    func num(_ value: Double) -> String {
        value.isFinite ? value.formatted(.number.precision(.fractionLength(2))) : "—"
    }
    func sec(_ value: Double) -> String {
        value.formatted(.number.precision(.fractionLength(0...3)))
    }
    func percent(_ ratio: Double) -> String {
        ratio.isFinite ? (ratio * 100).formatted(.number.precision(.fractionLength(0))) + "%" : "—"
    }
    func dominant(_ state: SpikePatternState, _ fraction: Double, _ unlabeled: Double) -> String {
        state == .unlabeled
            ? l10n.t(SpikePatternState.unlabeled.localizationSourceZH) + " · " + percent(unlabeled)
            : l10n.t(state.localizationSourceZH) + " · " + percent(fraction)
    }
    func axisTitle(_ axis: NeuralManifoldPCAAxis) -> String { axis.title(for: method) }

    var rows: [(label: String, value: String)] = [
        (l10n.t("分箱"), "\(score.binID)"),
        (timeOrigin == .aligned ? l10n.t("对齐区间") : l10n.t("原始区间"),
         "\(sec(score.startSec))–\(sec(score.endSec)) s"),
        ("X \(axisTitle(xAxis))", num(xAxis.value(in: score))),
        ("Y \(axisTitle(yAxis))", num(yAxis.value(in: score))),
    ]
    if let zAxis {
        rows.append(("Z \(axisTitle(zAxis))", num(zAxis.value(in: score))))
    }
    if let info = binInfo?[score.binID] {
        rows.append((l10n.t("自动"), dominant(info.autoDominant, info.autoDominantFraction, info.autoUnlabeledFraction)))
        rows.append((l10n.t("最终"), dominant(info.finalDominant, info.finalDominantFraction, info.finalUnlabeledFraction)))
        rows.append((l10n.t("训练数"), "\(info.contributingTrains) / \(info.totalTrains)"))
    }
    return NeuralManifoldHoverTarget(title: l10n.t("群体分箱"), rows: rows)
}

struct NeuralManifoldHoverCard: View {
    let target: NeuralManifoldHoverTarget

    var body: some View {
        VStack(alignment: .leading, spacing: 7) {
            Text(target.title)
                .font(.caption.weight(.semibold))

            Divider()

            ForEach(Array(target.rows.enumerated()), id: \.offset) { _, row in
                HStack(alignment: .firstTextBaseline, spacing: 8) {
                    Text(row.label)
                        .foregroundStyle(.secondary)
                        .frame(width: 76, alignment: .leading)
                    Text(row.value)
                        .fontWeight(.semibold)
                        .monospacedDigit()
                        .frame(maxWidth: .infinity, alignment: .leading)
                }
            }
        }
        .font(.caption)
        .padding(10)
        .neuralManifoldGlassPanel()
    }
}

/// Liquid Glass-style panel for the Neural Manifold plot overlays — the legend and the shared 2-D/3-D inspector
/// card. Visual only (no content or layout change): a translucent material background, a subtle gradient rim
/// highlight, a soft shadow, and a rounded radius matching the app's glass cards. Follows the same
/// availability-gate structure as the app's `RasterHoverGlassPanel` — native `glassEffect` on macOS 26+, a
/// material fallback on the package's macOS 14 target (so the deployment target is not raised) — but the
/// fallback uses the more opaque `regularMaterial` (rather than `ultraThinMaterial`) so text stays legible over
/// the scatter.
private struct NeuralManifoldGlassPanel: ViewModifier {
    var cornerRadius: CGFloat = 12
    private var shape: RoundedRectangle { RoundedRectangle(cornerRadius: cornerRadius, style: .continuous) }

    @ViewBuilder
    func body(content: Content) -> some View {
        if #available(macOS 26.0, *) {
            content
                .background { shape.fill(Color(nsColor: .textBackgroundColor).opacity(0.18)) }
                .glassEffect(.regular, in: shape)
                .overlay { rim }
                .shadow(color: .black.opacity(0.10), radius: 16, y: 6)
        } else {
            content
                .background(.regularMaterial, in: shape)
                .overlay { rim }
                .shadow(color: .black.opacity(0.10), radius: 16, y: 6)
        }
    }

    /// Subtle top-leading highlight fading to a separator edge — the glass "rim light".
    private var rim: some View {
        shape.stroke(
            LinearGradient(
                colors: [Color.white.opacity(0.40), Color(nsColor: .separatorColor).opacity(0.28)],
                startPoint: .topLeading,
                endPoint: .bottomTrailing
            ),
            lineWidth: 1
        )
    }
}

private extension View {
    /// Wrap a Neural Manifold plot overlay (legend / inspector card) in the shared glass panel.
    func neuralManifoldGlassPanel(cornerRadius: CGFloat = 12) -> some View {
        modifier(NeuralManifoldGlassPanel(cornerRadius: cornerRadius))
    }
}

private struct NeuralManifoldScatterProjection {
    let plot: CGRect
    let xLo: Double
    let xHi: Double
    let yLo: Double
    let yHi: Double

    init?(xs: [Double], ys: [Double], size: CGSize) {
        guard let xMin = xs.min(), let xMax = xs.max(), let yMin = ys.min(), let yMax = ys.max() else {
            return nil
        }
        let plot = CGRect(x: 16, y: 12, width: size.width - 28, height: size.height - 34)
        guard plot.width > 1, plot.height > 1 else { return nil }
        func span(_ lo: Double, _ hi: Double) -> (Double, Double) {
            guard hi - lo > 1e-9 else { return (lo - 1, hi + 1) }
            let pad = (hi - lo) * 0.06
            return (lo - pad, hi + pad)
        }
        (xLo, xHi) = span(xMin, xMax)
        (yLo, yHi) = span(yMin, yMax)
        self.plot = plot
    }

    func screen(x: Double, y: Double) -> CGPoint {
        CGPoint(
            x: plot.minX + CGFloat((x - xLo) / (xHi - xLo)) * plot.width,
            y: plot.maxY - CGFloat((y - yLo) / (yHi - yLo)) * plot.height
        )
    }
}

private func neuralManifoldHoverCardPosition(
    for location: CGPoint,
    in size: CGSize,
    cardWidth: CGFloat,
    cardHeight: CGFloat
) -> CGPoint {
    let margin: CGFloat = 12
    let xCandidate = location.x + cardWidth / 2 + 16
    let x: CGFloat
    if xCandidate + cardWidth / 2 + margin > size.width {
        x = max(cardWidth / 2 + margin, location.x - cardWidth / 2 - 16)
    } else {
        x = min(max(cardWidth / 2 + margin, xCandidate), size.width - cardWidth / 2 - margin)
    }

    let yCandidate = location.y - cardHeight / 2 - 14
    let y: CGFloat
    if yCandidate - cardHeight / 2 - margin < 0 {
        y = min(size.height - cardHeight / 2 - margin, location.y + cardHeight / 2 + 14)
    } else {
        y = min(max(cardHeight / 2 + margin, yCandidate), size.height - cardHeight / 2 - margin)
    }
    return CGPoint(x: max(cardWidth / 2 + margin, x), y: max(cardHeight / 2 + margin, y))
}
