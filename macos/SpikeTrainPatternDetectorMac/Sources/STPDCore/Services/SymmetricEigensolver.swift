import Accelerate
import Foundation

/// Shared symmetric eigendecomposition for the R-compatible PCA / classical-MDS / Isomap paths.
///
/// Output contract (identical regardless of backend, so every caller and its tests stay stable):
/// - eigenvalues sorted **descending**;
/// - eigenvectors returned **as columns** (`vectors[i][c]` = the i-th component of the c-th eigenvector);
/// - a deterministic **sign convention**: each eigenvector is flipped so its largest-magnitude component is
///   positive (first index wins on a magnitude tie), matching the previous pure-Swift Jacobi solver.
///
/// Backend: LAPACK `dsyevd` (divide-and-conquer) via Accelerate for `n >= 2`, with a self-contained cyclic
/// Jacobi solver as the fallback for `n <= 1` and for any LAPACK failure (`info != 0`). For non-degenerate
/// eigenvalues LAPACK and Jacobi return the same eigenvectors up to sign, so the sign convention makes the two
/// paths agree to floating-point tolerance; for a degenerate eigenvalue (repeated) either basis is valid and
/// the callers' results are basis-invariant (PCA reconstruction / variance, Isomap residual-variance geometry).
enum SymmetricEigensolver {
    /// Eigendecompose a symmetric matrix `matrix` (row-major `n × n`, assumed symmetric).
    static func decompose(_ matrix: [[Double]]) -> (values: [Double], vectors: [[Double]]) {
        let n = matrix.count
        if n == 0 { return ([], []) }
        if n == 1 { return ([matrix[0][0]], [[1.0]]) }
        if let lapack = lapack(matrix) { return lapack }
        return jacobi(matrix)
    }

    // MARK: - LAPACK backend (dsyevd via Accelerate)

    /// LAPACK `dsyevd`. Returns `nil` (so the caller falls back to Jacobi) on a workspace-query or solve failure.
    private static func lapack(_ matrix: [[Double]]) -> (values: [Double], vectors: [[Double]])? {
        let n = matrix.count
        guard n > 1 else { return nil }

        // Column-major packing. The matrix is symmetric, so we can fill the full array and let LAPACK read the
        // upper triangle (`uplo = U`); a[col * n + row].
        var a = [Double](repeating: 0, count: n * n)
        for row in 0..<n {
            let r = matrix[row]
            guard r.count == n else { return nil }
            for col in 0..<n { a[col * n + row] = r[col] }
        }

        var jobz = CChar(UInt8(ascii: "V"))   // compute eigenvalues + eigenvectors
        var uplo = CChar(UInt8(ascii: "U"))
        var order = __LAPACK_int(n)
        var lda = __LAPACK_int(n)
        var eigenvalues = [Double](repeating: 0, count: n)   // ascending on output
        var info = __LAPACK_int(0)

        // 1. Workspace query (lwork = liwork = -1): LAPACK writes the optimal sizes into work[0] / iwork[0].
        var workSize = Double(0)
        var iworkSize = __LAPACK_int(0)
        var lworkQuery = __LAPACK_int(-1)
        var liworkQuery = __LAPACK_int(-1)
        dsyevd_(&jobz, &uplo, &order, &a, &lda, &eigenvalues,
                &workSize, &lworkQuery, &iworkSize, &liworkQuery, &info)
        guard info == 0, workSize.isFinite, workSize >= 1 else { return nil }

        let lwork = __LAPACK_int(workSize)
        let liwork = max(__LAPACK_int(1), iworkSize)
        var work = [Double](repeating: 0, count: Int(lwork))
        var iwork = [__LAPACK_int](repeating: 0, count: Int(liwork))
        var lworkVar = lwork
        var liworkVar = liwork

        // 2. Solve.
        dsyevd_(&jobz, &uplo, &order, &a, &lda, &eigenvalues,
                &work, &lworkVar, &iwork, &liworkVar, &info)
        guard info == 0 else { return nil }

        // LAPACK returns eigenvalues ascending with eigenvectors as the columns of `a` (column-major). Reverse
        // to descending and apply the shared sign convention.
        var values = [Double]()
        values.reserveCapacity(n)
        var vectors = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for outColumn in 0..<n {
            let source = n - 1 - outColumn
            values.append(eigenvalues[source])
            var vector = (0..<n).map { a[source * n + $0] }
            normalizeSign(&vector)
            for row in 0..<n { vectors[row][outColumn] = vector[row] }
        }
        return (values, vectors)
    }

    // MARK: - Jacobi fallback (self-contained; bit-for-bit the previous PCA / Isomap solver)

    /// Cyclic Jacobi eigendecomposition of a symmetric matrix. Eigenvalues descending; eigenvectors as columns
    /// with the shared sign convention. Used for `n <= 1` and as the LAPACK-failure fallback.
    static func jacobi(_ matrix: [[Double]]) -> (values: [Double], vectors: [[Double]]) {
        let n = matrix.count
        if n == 0 { return ([], []) }
        var a = matrix
        var v = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for i in 0..<n { v[i][i] = 1 }
        if n == 1 { return ([a[0][0]], v) }

        let frobenius = a.reduce(0.0) { rowAcc, row in rowAcc + row.reduce(0.0) { $0 + $1 * $1 } }
        let threshold = 1e-28 * Swift.max(1.0, frobenius)

        for _ in 0..<100 {
            var off = 0.0
            for p in 0..<n {
                for q in (p + 1)..<n { off += a[p][q] * a[p][q] }
            }
            if off <= threshold { break }
            for p in 0..<n {
                for q in (p + 1)..<n {
                    let apq = a[p][q]
                    if apq == 0 { continue }
                    let theta = (a[q][q] - a[p][p]) / (2 * apq)
                    let t = (theta >= 0 ? 1.0 : -1.0) / (abs(theta) + (theta * theta + 1).squareRoot())
                    let c = 1 / (t * t + 1).squareRoot()
                    let s = t * c
                    for k in 0..<n {                       // columns: B = A · J
                        let akp = a[k][p], akq = a[k][q]
                        a[k][p] = c * akp - s * akq
                        a[k][q] = s * akp + c * akq
                    }
                    for k in 0..<n {                       // rows: A' = Jᵀ · B
                        let apk = a[p][k], aqk = a[q][k]
                        a[p][k] = c * apk - s * aqk
                        a[q][k] = s * apk + c * aqk
                    }
                    for k in 0..<n {                       // accumulate eigenvectors: V' = V · J
                        let vkp = v[k][p], vkq = v[k][q]
                        v[k][p] = c * vkp - s * vkq
                        v[k][q] = s * vkp + c * vkq
                    }
                }
            }
        }

        let order = (0..<n).sorted { a[$0][$0] > a[$1][$1] }
        var values = [Double]()
        var vectors = [[Double]](repeating: [Double](repeating: 0, count: n), count: n)
        for (column, source) in order.enumerated() {
            var vector = (0..<n).map { v[$0][source] }
            normalizeSign(&vector)
            values.append(a[source][source])
            for k in 0..<n { vectors[k][column] = vector[k] }
        }
        return (values, vectors)
    }

    // MARK: - Shared deterministic sign convention

    /// Flip the eigenvector so its largest-magnitude component is positive (first index wins on a tie).
    private static func normalizeSign(_ vector: inout [Double]) {
        guard !vector.isEmpty else { return }
        var maxIndex = 0
        for k in 1..<vector.count where abs(vector[k]) > abs(vector[maxIndex]) { maxIndex = k }
        if vector[maxIndex] < 0 {
            for k in 0..<vector.count { vector[k] = -vector[k] }
        }
    }
}
