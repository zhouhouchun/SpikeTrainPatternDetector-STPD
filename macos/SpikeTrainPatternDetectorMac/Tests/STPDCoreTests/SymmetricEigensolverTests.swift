import Foundation
import Testing
@testable import STPDCore

// The shared LAPACK-backed eigensolver must match the Jacobi fallback (and known analytic eigenpairs) to
// floating-point tolerance, preserving descending order + the deterministic sign convention so PCA / MDS /
// Isomap stay R-parity-stable.

/// A symmetric tridiagonal matrix with distinct (non-degenerate) eigenvalues `4 + 2cos(kπ/5)`.
private let tridiagonal: [[Double]] = [
    [4, 1, 0, 0],
    [1, 4, 1, 0],
    [0, 1, 4, 1],
    [0, 0, 1, 4],
]

private func reconstruct(_ values: [Double], _ vectors: [[Double]]) -> [[Double]] {
    let n = values.count
    var out = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
    for i in 0..<n {
        for j in 0..<n {
            var sum = 0.0
            for c in 0..<n { sum += values[c] * vectors[i][c] * vectors[j][c] }
            out[i][j] = sum
        }
    }
    return out
}

@Test
func eigensolverMatchesJacobiFallbackToTolerance() {
    let lapack = SymmetricEigensolver.decompose(tridiagonal)
    let jacobi = SymmetricEigensolver.jacobi(tridiagonal)

    #expect(lapack.values.count == 4)
    for c in 0..<4 {
        #expect(abs(lapack.values[c] - jacobi.values[c]) < 1e-9)
        // Both paths apply the same sign convention, so non-degenerate eigenvectors agree component-wise.
        for r in 0..<4 {
            #expect(abs(lapack.vectors[r][c] - jacobi.vectors[r][c]) < 1e-7)
        }
    }
}

@Test
func eigensolverReturnsDescendingKnownEigenvalues() {
    let result = SymmetricEigensolver.decompose(tridiagonal)
    // 4 + 2cos(kπ/5) for k = 1..4, descending.
    let expected = [1, 2, 3, 4].map { 4 + 2 * Foundation.cos(Double($0) * Double.pi / 5) }
    for c in 0..<4 {
        #expect(abs(result.values[c] - expected[c]) < 1e-9)
    }
    // Descending.
    for c in 1..<4 { #expect(result.values[c - 1] >= result.values[c] - 1e-12) }
    // Trace is preserved.
    #expect(abs(result.values.reduce(0, +) - 16) < 1e-9)
}

@Test
func eigensolverAppliesDeterministicSignConvention() {
    let result = SymmetricEigensolver.decompose(tridiagonal)
    for c in 0..<4 {
        let column = (0..<4).map { result.vectors[$0][c] }
        // Largest-magnitude component is non-negative (the shared convention).
        let maxIndex = (0..<4).max { abs(column[$0]) < abs(column[$1]) }!
        #expect(column[maxIndex] >= 0)
    }
}

@Test
func eigensolverReconstructsTheMatrix() {
    let result = SymmetricEigensolver.decompose(tridiagonal)
    let m = reconstruct(result.values, result.vectors)
    for i in 0..<4 {
        for j in 0..<4 {
            #expect(abs(m[i][j] - tridiagonal[i][j]) < 1e-9)
        }
    }
}

@Test
func eigensolverHandlesNegativeEigenvalues() {
    // [[1,2],[2,1]] -> eigenvalues 3 and -1 (relevant: classical MDS keeps only the positive eigenpairs).
    let result = SymmetricEigensolver.decompose([[1, 2], [2, 1]])
    #expect(abs(result.values[0] - 3) < 1e-12)
    #expect(abs(result.values[1] - (-1)) < 1e-12)
}

@Test
func eigensolverHandlesTinyMatrices() {
    let one = SymmetricEigensolver.decompose([[7]])
    #expect(one.values == [7])
    #expect(one.vectors == [[1]])

    let two = SymmetricEigensolver.decompose([[2, 0], [0, 5]])
    #expect(abs(two.values[0] - 5) < 1e-12)
    #expect(abs(two.values[1] - 2) < 1e-12)
}
