import AppKit
import CoreGraphics
import Foundation

guard CommandLine.arguments.count == 3 else {
    fputs("usage: create_logo_mark.swift <source.png> <output.png>\n", stderr)
    exit(2)
}

let sourceURL = URL(fileURLWithPath: CommandLine.arguments[1])
let outputURL = URL(fileURLWithPath: CommandLine.arguments[2])
guard
    let sourceImage = NSImage(contentsOf: sourceURL),
    let sourceCGImage = sourceImage.cgImage(forProposedRect: nil, context: nil, hints: nil),
    let sourceData = sourceCGImage.dataProvider?.data
else {
    fputs("could not read source image\n", stderr)
    exit(1)
}

let width = sourceCGImage.width
let height = sourceCGImage.height
let colorSpace = CGColorSpaceCreateDeviceRGB()
var sourceRGBA = [UInt8](repeating: 0, count: width * height * 4)
var outputRGBA = [UInt8](repeating: 0, count: width * height * 4)

guard let sourceContext = CGContext(
    data: &sourceRGBA,
    width: width,
    height: height,
    bitsPerComponent: 8,
    bytesPerRow: width * 4,
    space: colorSpace,
    bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue
) else {
    fputs("could not create source context\n", stderr)
    exit(1)
}

sourceContext.draw(sourceCGImage, in: CGRect(x: 0, y: 0, width: width, height: height))

let background = (r: Double(sourceRGBA[0]), g: Double(sourceRGBA[1]), b: Double(sourceRGBA[2]))
let green = (r: UInt8(background.r.rounded()), g: UInt8(background.g.rounded()), b: UInt8(background.b.rounded()))

for index in stride(from: 0, to: sourceRGBA.count, by: 4) {
    let r = Double(sourceRGBA[index])
    let g = Double(sourceRGBA[index + 1])
    let b = Double(sourceRGBA[index + 2])
    let aR = (r - background.r) / max(1, 255 - background.r)
    let aG = (g - background.g) / max(1, 255 - background.g)
    let aB = (b - background.b) / max(1, 255 - background.b)
    let alpha = UInt8((min(1, max(0, (aR + aG + aB) / 3)) * 255).rounded())

    outputRGBA[index] = UInt8((Double(green.r) * Double(alpha) / 255).rounded())
    outputRGBA[index + 1] = UInt8((Double(green.g) * Double(alpha) / 255).rounded())
    outputRGBA[index + 2] = UInt8((Double(green.b) * Double(alpha) / 255).rounded())
    outputRGBA[index + 3] = alpha
}

guard let outputContext = CGContext(
    data: &outputRGBA,
    width: width,
    height: height,
    bitsPerComponent: 8,
    bytesPerRow: width * 4,
    space: colorSpace,
    bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue
), let outputCGImage = outputContext.makeImage() else {
    fputs("could not create output image\n", stderr)
    exit(1)
}

let outputRep = NSBitmapImageRep(cgImage: outputCGImage)
guard let outputData = outputRep.representation(using: .png, properties: [:]) else {
    fputs("could not encode output image\n", stderr)
    exit(1)
}
try outputData.write(to: outputURL, options: .atomic)
