// swift-tools-version: 6.0

import PackageDescription

let package = Package(
    name: "SpikeTrainPatternDetectorMac",
    platforms: [
        .macOS(.v14)
    ],
    products: [
        .executable(name: "SpikeTrainPatternDetectorMac", targets: ["SpikeTrainPatternDetectorMac"])
    ],
    targets: [
        .target(
            name: "STPDCore",
            path: "Sources/STPDCore"
        ),
        .executableTarget(
            name: "SpikeTrainPatternDetectorMac",
            dependencies: ["STPDCore"],
            path: "Sources/SpikeTrainPatternDetectorMac"
        ),
        .testTarget(
            name: "STPDCoreTests",
            dependencies: ["STPDCore"],
            path: "Tests/STPDCoreTests"
        )
    ]
)
