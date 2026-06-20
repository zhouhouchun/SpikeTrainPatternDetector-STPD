# SpikeTrainPatternDetectorMac Raster-First Prototype

This prototype intentionally starts with the previous failure point: loading a
wide spike matrix CSV and drawing the aligned spike train raster in a native
macOS window.

Current scope:

- Loads `inst/extdata/Grechishnikova_STN_2017_subset.csv` as the bundled sample.
- Supports opening a raw CSV where each column is one spike train.
- Parses spike timestamps into `STPDCore` value models.
- Aligns each train to its first timestamp, matching the Shiny raster baseline.
- Draws lane axes and black vertical spike ticks with SwiftUI `Canvas`.
- Keeps train labels fixed while the plot pane scrolls horizontally.
- Provides aligned-time and raw-timestamp raster modes.
- Provides a zoom slider expressed as points per second.
- Keeps R/Shiny out of the runtime path.

Run from the repository root:

```bash
./script/build_and_run.sh --verify
```

Verified smoke checks:

- `swift test` passes for CSV parsing and aligned timestamp behavior.
- The app bundle launches as `dist/SpikeTrainPatternDetectorMac.app`.
- The sample CSV is copied into `Contents/Resources`.
- A screenshot showed 4 trains and 480 visible spike ticks.
- Pixel sanity check on the raster screenshot found nonblank dark raster content.

Next migration steps should stay raster-first:

1. Stress-test scrolling with a long synthetic or real spike matrix.
2. Add a dedicated visible-window readout and jump-to-time control.
3. Add lightweight level-of-detail reporting for large spike matrices.
4. Port label/segment overlays only after the base raster remains stable.
5. Start detector migration after the raster can load real user CSV files reliably.
