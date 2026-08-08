#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root"

binary="src/rbm10_VIC_nc"
control="examples/netcdf_grid_remap/example.control"
output="examples/netcdf_grid_remap/output/example"
expected="examples/netcdf_grid_remap/expected/example"

if [[ ! -x "$binary" ]]; then
    echo "Missing $binary. Build it with: cd src && make rbm10_VIC_nc" >&2
    exit 1
fi

rm -f "$output.Temp" "$output.Spat" "$output.Temp.nc"
"./$binary" "$control"

python3 tools/compare_rbm_outputs.py "$expected.Temp" "$output.Temp" --temperature-tolerance 0 --flow-tolerance 0
cmp "$expected.Spat" "$output.Spat"
cmp "$expected.Temp.nc" "$output.Temp.nc"

echo "PASS: netCDF example matches the included expected outputs."
