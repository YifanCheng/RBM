#!/usr/bin/env sh
set -eu

repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$repo_dir"

binary=./src/rbm10_VIC_nc
control=examples/netcdf_reservoir/example.control
output=examples/netcdf_reservoir/output/example
expected=examples/netcdf_reservoir/expected/example

if [ ! -x "$binary" ]; then
    echo "Missing $binary. Build it with: cd src && make rbm10_VIC_nc" >&2
    exit 1
fi

rm -f "$output.Temp" "$output.Spat" "$output.Temp.nc" "$output.Resv"
"$binary" "$control"

for suffix in Temp Spat Temp.nc Resv; do
    if ! cmp -s "$output.$suffix" "$expected.$suffix"; then
        echo "Validation failed: $output.$suffix differs from expected output." >&2
        exit 1
    fi
done

echo "Reservoir netCDF integration example passed."
