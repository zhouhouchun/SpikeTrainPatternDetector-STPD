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
            path: "Sources/STPDCore",
            swiftSettings: [
                // Use Accelerate's modern (non-deprecated) LAPACK headers (`__LAPACK_int`, `dsyevd_`, …) for the
                // shared symmetric eigensolver. Default 32-bit LAPACK ints (no ILP64).
                .unsafeFlags(["-Xcc", "-DACCELERATE_NEW_LAPACK=1"])
            ]
        ),
        .executableTarget(
            name: "SpikeTrainPatternDetectorMac",
            dependencies: ["STPDCore"],
            path: "Sources/SpikeTrainPatternDetectorMac"
        ),
        .testTarget(
            name: "STPDCoreTests",
            dependencies: ["STPDCore"],
            path: "Tests/STPDCoreTests",
            // P6B-0: the 5x5 characterization dataset is loaded at runtime via #filePath, not compiled/bundled.
            exclude: ["Fixtures"]
        ),
        .testTarget(
            name: "SpikeTrainPatternDetectorMacTests",
            dependencies: ["SpikeTrainPatternDetectorMac", "STPDCore"],
            path: "Tests/SpikeTrainPatternDetectorMacTests"
        )
    ]
)
