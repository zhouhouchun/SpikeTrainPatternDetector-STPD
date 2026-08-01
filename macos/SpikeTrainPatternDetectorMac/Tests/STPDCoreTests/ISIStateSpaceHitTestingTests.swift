import Testing
@testable import STPDCore

private func screen(_ x: Double, _ y: Double) -> ISIStateSpaceScreenPoint {
    ISIStateSpaceScreenPoint(x: x, y: y)
}

@Test
func isiStateSpaceHitTestingReturnsNilForEmptyInput() {
    let index = ISIStateSpaceHitTesting.nearestIndex(positions: [], to: screen(0, 0), maxDistance: 10)
    #expect(index == nil)
}

@Test
func isiStateSpaceHitTestingReturnsNilWhenAllBeyondRadius() {
    let positions = [screen(100, 100), screen(-50, 30), screen(0, 40)]
    let index = ISIStateSpaceHitTesting.nearestIndex(positions: positions, to: screen(0, 0), maxDistance: 12)
    #expect(index == nil)
}

@Test
func isiStateSpaceHitTestingPicksTheNearestPoint() {
    // Point 1 (3,4) is 5 away; point 2 (1,1) is ~1.41 away; point 0 (10,10) is far.
    let positions = [screen(10, 10), screen(3, 4), screen(1, 1)]
    let index = ISIStateSpaceHitTesting.nearestIndex(positions: positions, to: screen(0, 0), maxDistance: 12)
    #expect(index == 2)
}

@Test
func isiStateSpaceHitTestingResolvesTiesToLowerIndex() {
    // Two points equidistant from the cursor: the lower index must win (stable selection).
    let positions = [screen(5, 0), screen(-5, 0)]
    let index = ISIStateSpaceHitTesting.nearestIndex(positions: positions, to: screen(0, 0), maxDistance: 12)
    #expect(index == 0)
}

@Test
func isiStateSpaceHitTestingTreatsRadiusAsInclusive() {
    // Distance exactly equal to maxDistance is a hit (<=), not a miss.
    let positions = [screen(10, 0)]
    let index = ISIStateSpaceHitTesting.nearestIndex(positions: positions, to: screen(0, 0), maxDistance: 10)
    #expect(index == 0)
}

@Test
func isiStateSpaceHitTestingReturnsNilForNegativeRadius() {
    let positions = [screen(0, 0)]
    let index = ISIStateSpaceHitTesting.nearestIndex(positions: positions, to: screen(0, 0), maxDistance: -1)
    #expect(index == nil)
}

@Test
func isiStateSpaceHitTestingIgnoresNonFinitePositions() {
    // A NaN-coordinate point must never be selected, even though a real point is also in range.
    let positions = [screen(.nan, .nan), screen(2, 0)]
    let index = ISIStateSpaceHitTesting.nearestIndex(positions: positions, to: screen(0, 0), maxDistance: 12)
    #expect(index == 1)
}

@Test
func isiStateSpaceHitTestingHitsExactlyOnAPoint() {
    let positions = [screen(7, 7), screen(3, 3)]
    let index = ISIStateSpaceHitTesting.nearestIndex(positions: positions, to: screen(3, 3), maxDistance: 12)
    #expect(index == 1)
}
