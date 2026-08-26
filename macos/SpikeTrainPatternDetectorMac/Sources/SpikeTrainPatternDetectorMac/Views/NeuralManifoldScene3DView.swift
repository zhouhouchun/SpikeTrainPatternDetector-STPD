import AppKit
import SceneKit
import STPDCore
import SwiftUI

/// Isolated SceneKit-backed 3-D view of the active population trajectory. It reuses the existing
/// `NeuralPopulationPCAScore` coordinate shape — no embedding recomputation — and lets SwiftUI choose which
/// display coordinate maps to X/Y/Z (method-aware embedding axes or Time), whether to draw the trajectory
/// line, and whether points use a time gradient. Camera control (drag to rotate, scroll/pinch to zoom) is
/// handled by SceneKit plus a small trackpad magnification bridge.
///
/// SceneKit usage is contained entirely in this file. The scene is only rebuilt when the scores actually
/// change, so unrelated SwiftUI re-renders do not reset the user's camera.
struct NeuralManifoldScene3DView: NSViewRepresentable {
    let scores: [NeuralPopulationPCAScore]
    let method: NeuralManifoldMethod
    let xAxis: NeuralManifoldPCAAxis
    let yAxis: NeuralManifoldPCAAxis
    let zAxis: NeuralManifoldPCAAxis
    let showsAxes: Bool
    let showsTrajectoryLine: Bool
    let lineStartSec: Double
    let lineEndSec: Double
    let usesTimeGradient: Bool
    /// Optional per-bin (binID -> color) override for pattern/state coloring (annotation overlay only).
    /// When `nil` or missing a bin, the existing time-gradient / accent coloring is used.
    var binColors: [Int: Color]? = nil
    /// Click-to-inspect selection (binID of the picked point, or `nil`). Written by the click hit-test; the
    /// parent shows the inspector card and this view highlights the matching point. NOT part of
    /// `RenderedConfiguration`, so selecting never rebuilds the scene (the camera is preserved).
    var selectedBinID: Binding<Int?> = .constant(nil)

    /// Spheres are bounded for performance; beyond this the points (and the line through them) are
    /// uniformly strided. Typical analysis windows stay well under the cap, so all bins usually render.
    private static let maxRenderedPoints = 1500

    struct RenderedConfiguration: Equatable {
        let scores: [NeuralPopulationPCAScore]
        let method: NeuralManifoldMethod
        let xAxis: NeuralManifoldPCAAxis
        let yAxis: NeuralManifoldPCAAxis
        let zAxis: NeuralManifoldPCAAxis
        let showsAxes: Bool
        let showsTrajectoryLine: Bool
        let lineStartSec: Double
        let lineEndSec: Double
        let usesTimeGradient: Bool
        let binColors: [Int: Color]?
    }

    private var configuration: RenderedConfiguration {
        RenderedConfiguration(
            scores: scores,
            method: method,
            xAxis: xAxis,
            yAxis: yAxis,
            zAxis: zAxis,
            showsAxes: showsAxes,
            showsTrajectoryLine: showsTrajectoryLine,
            lineStartSec: lineStartSec,
            lineEndSec: lineEndSec,
            usesTimeGradient: usesTimeGradient,
            binColors: binColors
        )
    }

    final class Coordinator: NSObject, NSGestureRecognizerDelegate {
        private var renderedConfiguration: RenderedConfiguration?
        /// Set by `make/updateNSView` to write the picked bin back to the SwiftUI binding.
        var onSelect: ((Int?) -> Void)?

        func needsRender(for configuration: RenderedConfiguration) -> Bool {
            renderedConfiguration != configuration
        }

        func markRendered(_ configuration: RenderedConfiguration) {
            renderedConfiguration = configuration
        }

        /// A plain click (no drag) picks the nearest point under the cursor. `hitTest` returns hits sorted
        /// nearest-first; we take the first that is a population-bin point node (ignoring the trajectory line,
        /// axes, labels, and the selection marker). An empty / non-point click clears the selection.
        @MainActor @objc func handleClick(_ gesture: NSClickGestureRecognizer) {
            guard let view = gesture.view as? SCNView else { return }
            let location = gesture.location(in: view)
            let hits = view.hitTest(location, options: [
                .searchMode: NSNumber(value: SCNHitTestSearchMode.all.rawValue),
                .ignoreHiddenNodes: true,
            ])
            let binID = hits
                .lazy
                .compactMap { NeuralManifoldScene3DNodeNaming.binID(fromNodeName: $0.node.name) }
                .first
            onSelect?(binID)
        }

        /// Coexist with `allowsCameraControl`'s drag-to-rotate recognizers: a click and a camera drag are
        /// distinct gestures, but allowing simultaneous recognition keeps the click reliable.
        func gestureRecognizer(
            _ gestureRecognizer: NSGestureRecognizer,
            shouldRecognizeSimultaneouslyWith otherGestureRecognizer: NSGestureRecognizer
        ) -> Bool {
            true
        }

        static let cameraNodeName = "NeuralManifoldMainCamera"
    }

    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeNSView(context: Context) -> NeuralManifoldSCNView {
        let view = NeuralManifoldSCNView()
        // `allowsCameraControl` keeps SceneKit's drag-to-rotate (and option/right-drag pan); the subclass
        // intercepts magnify/scrollWheel for zoom, so this is the only camera behavior SceneKit drives.
        view.allowsCameraControl = true
        view.autoenablesDefaultLighting = true
        view.antialiasingMode = .multisampling4X
        view.backgroundColor = .clear
        view.scene = Self.buildScene(configuration)
        view.pointOfView = view.scene?.rootNode.childNode(withName: Coordinator.cameraNodeName, recursively: true)
        context.coordinator.markRendered(configuration)

        // Click-to-inspect: a click recognizer that coexists with `allowsCameraControl` (drags still rotate).
        let click = NSClickGestureRecognizer(
            target: context.coordinator, action: #selector(Coordinator.handleClick(_:))
        )
        click.delegate = context.coordinator
        view.addGestureRecognizer(click)
        context.coordinator.onSelect = { [binding = selectedBinID] in binding.wrappedValue = $0 }
        Self.applySelectionMarker(in: view, binID: selectedBinID.wrappedValue)
        return view
    }

    func updateNSView(_ nsView: NeuralManifoldSCNView, context: Context) {
        // Refresh the selection callback so the click handler writes to the current binding.
        context.coordinator.onSelect = { [binding = selectedBinID] in binding.wrappedValue = $0 }

        // Only rebuild (which resets the camera) when the data or drawing configuration changed.
        let didRebuild = context.coordinator.needsRender(for: configuration)
        if didRebuild {
            nsView.scene = Self.buildScene(configuration)
            nsView.pointOfView = nsView.scene?.rootNode.childNode(withName: Coordinator.cameraNodeName, recursively: true)
            context.coordinator.markRendered(configuration)
            // A rebuilt scene re-lays the points; drop any stale selection (deferred so we don't mutate
            // SwiftUI state during the view update).
            if selectedBinID.wrappedValue != nil {
                let binding = selectedBinID
                DispatchQueue.main.async { binding.wrappedValue = nil }
            }
        }

        // Highlight the selected point (nil right after a rebuild, until the deferred clear lands).
        Self.applySelectionMarker(in: nsView, binID: didRebuild ? nil : selectedBinID.wrappedValue)
    }

    // MARK: - Scene construction

    private static func buildScene(_ configuration: RenderedConfiguration) -> SCNScene {
        let scene = SCNScene()

        func xValue(_ score: NeuralPopulationPCAScore) -> Double { configuration.xAxis.value(in: score) }
        func yValue(_ score: NeuralPopulationPCAScore) -> Double { configuration.yAxis.value(in: score) }
        func zValue(_ score: NeuralPopulationPCAScore) -> Double { configuration.zAxis.value(in: score) }
        func axisTitle(_ axis: NeuralManifoldPCAAxis) -> String { axis.title(for: configuration.method) }

        let finite = configuration.scores.filter {
            xValue($0).isFinite && yValue($0).isFinite && zValue($0).isFinite
        }
        guard finite.count >= 2 else { return scene }

        let stride = finite.count > maxRenderedPoints
            ? Int(ceil(Double(finite.count) / Double(maxRenderedPoints)))
            : 1
        let points = stride == 1
            ? finite
            : finite.enumerated().filter { $0.offset % stride == 0 }.map(\.element)

        // Center at the origin and scale to a fixed cube (half-size `targetHalf`), preserving aspect ratio.
        let xs = points.map(xValue)
        let ys = points.map(yValue)
        let zs = points.map(zValue)
        let centerX = midpoint(xs), centerY = midpoint(ys), centerZ = midpoint(zs)
        let rawScale = max(halfRange(xs), halfRange(ys), halfRange(zs))
        let scale = rawScale > 1e-9 ? rawScale : 1
        let targetHalf = 6.0
        func norm(_ value: Double, _ center: Double) -> Double { (value - center) / scale * targetHalf }
        func position(_ score: NeuralPopulationPCAScore) -> SCNVector3 {
            vector(norm(xValue(score), centerX), norm(yValue(score), centerY), norm(zValue(score), centerZ))
        }

        // Trajectory line through consecutive bins (drawn under the points).
        if configuration.showsTrajectoryLine {
            let linePoints = lineFiltered(points, startSec: configuration.lineStartSec, endSec: configuration.lineEndSec)
            if linePoints.count >= 2 {
                let vertices = linePoints.map(position)
                let lineNode = SCNNode(geometry: lineGeometry(
                    vertices: vertices,
                    color: NSColor.secondaryLabelColor.withAlphaComponent(0.5)
                ))
                scene.rootNode.addChildNode(lineNode)
            }
        }

        // Points optionally colored by time progression (bin order).
        let pointsNode = SCNNode()
        let count = points.count
        for (index, score) in points.enumerated() {
            let fraction = count > 1 ? Double(index) / Double(count - 1) : 0
            let sphere = SCNSphere(radius: 0.1)
            sphere.segmentCount = 12
            let material = SCNMaterial()
            let patternColor = configuration.binColors?[score.binID].map { NSColor($0) }
            material.diffuse.contents = patternColor ?? (configuration.usesTimeGradient ? progressionColor(fraction) : STPDAppTheme.nsAccent)
            sphere.firstMaterial = material
            let node = SCNNode(geometry: sphere)
            node.name = NeuralManifoldScene3DNodeNaming.nodeName(forBinID: score.binID)
            node.position = position(score)
            pointsNode.addChildNode(node)
        }
        scene.rootNode.addChildNode(pointsNode)

        // Faint scene axes for orientation. Their semantic mapping is controlled by the X/Y/Z selectors.
        if configuration.showsAxes {
            scene.rootNode.addChildNode(axisNode(
                targetHalf: targetHalf,
                xLabel: "X \(axisTitle(configuration.xAxis))",
                yLabel: "Y \(axisTitle(configuration.yAxis))",
                zLabel: "Z \(axisTitle(configuration.zAxis))"
            ))
        }

        // Camera looking down −Z at the origin-centered cloud.
        let cameraNode = SCNNode()
        let camera = SCNCamera()
        camera.zNear = 0.1
        camera.zFar = 1000
        cameraNode.camera = camera
        cameraNode.name = Coordinator.cameraNodeName
        cameraNode.position = vector(0, 0, 24)
        scene.rootNode.addChildNode(cameraNode)

        return scene
    }

    // MARK: - Geometry helpers

    private static func lineGeometry(vertices: [SCNVector3], color: NSColor) -> SCNGeometry {
        let source = SCNGeometrySource(vertices: vertices)
        var indices: [Int32] = []
        indices.reserveCapacity(max(0, (vertices.count - 1) * 2))
        for i in 0..<max(0, vertices.count - 1) {
            indices.append(Int32(i))
            indices.append(Int32(i + 1))
        }
        let element = SCNGeometryElement(indices: indices, primitiveType: .line)
        let geometry = SCNGeometry(sources: [source], elements: [element])
        let material = SCNMaterial()
        material.diffuse.contents = color
        material.lightingModel = .constant
        material.isDoubleSided = true
        geometry.firstMaterial = material
        return geometry
    }

    private static func axisNode(targetHalf: Double, xLabel: String, yLabel: String, zLabel: String) -> SCNNode {
        let length = targetHalf + 0.8
        let node = SCNNode()
        func axis(_ from: SCNVector3, _ to: SCNVector3, _ color: NSColor) -> SCNNode {
            SCNNode(geometry: lineGeometry(vertices: [from, to], color: color.withAlphaComponent(0.55)))
        }
        node.addChildNode(axis(vector(-length, 0, 0), vector(length, 0, 0), .systemRed))
        node.addChildNode(axis(vector(0, -length, 0), vector(0, length, 0), .systemGreen))
        node.addChildNode(axis(vector(0, 0, -length), vector(0, 0, length), .systemBlue))
        node.addChildNode(axisLabel(xLabel, color: .systemRed, position: vector(length + 0.28, 0, 0)))
        node.addChildNode(axisLabel(yLabel, color: .systemGreen, position: vector(0, length + 0.28, 0)))
        node.addChildNode(axisLabel(zLabel, color: .systemBlue, position: vector(0, 0, length + 0.28)))
        return node
    }

    private static func axisLabel(_ text: String, color: NSColor, position: SCNVector3) -> SCNNode {
        let geometry = SCNText(string: text, extrusionDepth: 0.01)
        geometry.font = NSFont.systemFont(ofSize: 0.42, weight: .semibold)
        geometry.flatness = 0.2
        let material = SCNMaterial()
        material.diffuse.contents = color.withAlphaComponent(0.82)
        material.lightingModel = .constant
        geometry.firstMaterial = material
        let node = SCNNode(geometry: geometry)
        node.position = position
        node.scale = SCNVector3(0.9, 0.9, 0.9)
        node.constraints = [SCNBillboardConstraint()]
        return node
    }

    private static func vector(_ x: Double, _ y: Double, _ z: Double) -> SCNVector3 {
        SCNVector3(CGFloat(x), CGFloat(y), CGFloat(z))
    }

    private static func midpoint(_ values: [Double]) -> Double {
        ((values.min() ?? 0) + (values.max() ?? 0)) / 2
    }

    private static func halfRange(_ values: [Double]) -> Double {
        ((values.max() ?? 0) - (values.min() ?? 0)) / 2
    }

    private static func lineFiltered(
        _ points: [NeuralPopulationPCAScore],
        startSec: Double,
        endSec: Double
    ) -> [NeuralPopulationPCAScore] {
        let start = max(0, startSec)
        let end = max(0, endSec)
        guard end > start else { return points }
        return points.filter { start <= $0.midSec && $0.midSec <= end }
    }

    /// Green (#399E65) → coral (#EC6965) gradient encoding bin order (time). Matches the 2-D canvas; no
    /// detector-state semantics.
    private static func progressionColor(_ fraction: Double) -> NSColor {
        let t = max(0, min(1, fraction))
        let start = (r: Double(0x39) / 255, g: Double(0x9E) / 255, b: Double(0x65) / 255)
        let end = (r: Double(0xEC) / 255, g: Double(0x69) / 255, b: Double(0x65) / 255)
        return NSColor(
            srgbRed: start.r + (end.r - start.r) * t,
            green: start.g + (end.g - start.g) * t,
            blue: start.b + (end.b - start.b) * t,
            alpha: 1
        )
    }

    // MARK: - Selection highlight

    private static let selectionMarkerName = "stpd.nm.selectionMarker"

    /// Place (or clear) a bright highlight on the selected bin's point. The marker is a separate node added to
    /// the scene root (so the point's own geometry/material is untouched) and is drawn on top — depth-test
    /// off — so the selection stays visible even when occluded by nearer points. Idempotent: it removes any
    /// previous marker first, so repeated calls (every `updateNSView`) never accumulate nodes.
    private static func applySelectionMarker(in view: SCNView, binID: Int?) {
        guard let scene = view.scene else { return }
        scene.rootNode.childNode(withName: selectionMarkerName, recursively: false)?.removeFromParentNode()
        guard
            let binID,
            let target = scene.rootNode.childNode(
                withName: NeuralManifoldScene3DNodeNaming.nodeName(forBinID: binID), recursively: true
            )
        else { return }
        let marker = SCNNode(geometry: selectionMarkerGeometry())
        marker.name = selectionMarkerName
        marker.position = target.worldPosition   // marker is a root child; worldPosition is in root space
        marker.renderingOrder = 10
        scene.rootNode.addChildNode(marker)
    }

    private static func selectionMarkerGeometry() -> SCNGeometry {
        let sphere = SCNSphere(radius: 0.2)
        sphere.segmentCount = 16
        let material = SCNMaterial()
        material.diffuse.contents = NSColor.systemYellow
        material.emission.contents = NSColor.systemYellow
        material.lightingModel = .constant
        material.transparency = 0.9
        material.readsFromDepthBuffer = false
        material.writesToDepthBuffer = false
        sphere.firstMaterial = material
        return sphere
    }
}
/// `SCNView` subclass that implements reliable zoom on macOS by dollying the named camera node directly.
///
/// Why this is needed: with `allowsCameraControl`, SceneKit handles zoom inside its own
/// `magnify(with:)` / `scrollWheel(with:)` responder methods, which an added `NSMagnificationGestureRecognizer`
/// competes with and does not reliably receive. By overriding those two responder methods here — and **not**
/// calling `super` — we replace SceneKit's zoom while leaving drag-to-rotate / pan (`mouseDragged` etc.,
/// which we do not override) to `allowsCameraControl`. The result works for Magic Trackpad pinch, two-finger
/// scroll, and mouse wheel alike. Camera distance is clamped to a sane range. PCA/data and all rendering are
/// unchanged; this only adjusts the camera transform.
final class NeuralManifoldSCNView: SCNView {
    private static let minDistance = 2.0
    private static let maxDistance = 120.0

    /// Magic Trackpad pinch. Positive magnification (spread / pinch-out) zooms in.
    override func magnify(with event: NSEvent) {
        let magnification = Double(event.magnification)
        guard magnification.isFinite, abs(magnification) > 1e-6 else { return }
        applyZoom(scaleFactor: exp(-magnification))
    }

    /// Two-finger trackpad scroll and mouse wheel. Scrolling up zooms in. (We consume the event — do not
    /// call `super` — so SceneKit's own zoom does not also fire and the scroll does not bubble to a parent.)
    override func scrollWheel(with event: NSEvent) {
        let raw = Double(event.scrollingDeltaY)
        guard raw != 0, raw.isFinite else { return }
        // Trackpad reports fine pixel deltas (precise); a mouse wheel reports coarse line deltas. Scale the
        // coarse case up so both feel similar.
        let step = event.hasPreciseScrollingDeltas ? raw : raw * 10
        applyZoom(scaleFactor: exp(-step * 0.01))
    }

    /// Dolly the camera toward / away from the origin (the cloud is origin-centered), clamping the distance.
    private func applyZoom(scaleFactor: Double) {
        guard scaleFactor.isFinite, scaleFactor > 0,
              let camera = pointOfView ?? scene?.rootNode.childNode(
                  withName: NeuralManifoldScene3DView.Coordinator.cameraNodeName, recursively: true)
        else { return }
        let position = camera.position
        let distance = sqrt(Double(position.x * position.x + position.y * position.y + position.z * position.z))
        guard distance > 1e-6 else { return }
        let target = min(max(distance * scaleFactor, Self.minDistance), Self.maxDistance)
        let scale = CGFloat(target / distance)
        camera.position = SCNVector3(position.x * scale, position.y * scale, position.z * scale)
    }
}
