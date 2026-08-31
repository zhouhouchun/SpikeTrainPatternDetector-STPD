#!/bin/sh
set -eu
ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
Rscript "$ROOT_DIR/generate_benchmark_spike_trains.R"
Rscript "$ROOT_DIR/tests/verify_v2_2.R"
