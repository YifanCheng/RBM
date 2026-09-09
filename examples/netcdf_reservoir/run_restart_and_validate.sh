#!/usr/bin/env sh
set -eu

repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$repo_dir"

binary=./src/rbm10_VIC_nc
example_dir=examples/netcdf_reservoir
output_dir=$example_dir/output

if [ ! -x "$binary" ]; then
    echo "Missing $binary. Build it with: cd src && make rbm10_VIC_nc" >&2
    exit 1
fi

rm -f \
    "$output_dir/restart_full.Temp" \
    "$output_dir/restart_full.Spat" \
    "$output_dir/restart_full.Resv" \
    "$output_dir/restart_full_state.nc" \
    "$output_dir/restart_part1.Temp" \
    "$output_dir/restart_part1.Spat" \
    "$output_dir/restart_part1.Resv" \
    "$output_dir/restart_part1_state.nc" \
    "$output_dir/restart_part2.Temp" \
    "$output_dir/restart_part2.Spat" \
    "$output_dir/restart_part2.Resv" \
    "$output_dir/restart_part2_state.nc" \
    "$output_dir/restart_combined.Temp" \
    "$output_dir/restart_combined.Resv"

"$binary" "$example_dir/restart_full.control"
"$binary" "$example_dir/restart_part1.control"
"$binary" "$example_dir/restart_part2.control"

cat "$output_dir/restart_part1.Temp" "$output_dir/restart_part2.Temp" \
    > "$output_dir/restart_combined.Temp"
cat "$output_dir/restart_part1.Resv" "$output_dir/restart_part2.Resv" \
    > "$output_dir/restart_combined.Resv"

cmp "$output_dir/restart_full.Temp" "$output_dir/restart_combined.Temp"
cmp "$output_dir/restart_full.Resv" "$output_dir/restart_combined.Resv"
cmp "$output_dir/restart_full.Spat" "$output_dir/restart_part1.Spat"
cmp "$output_dir/restart_full.Spat" "$output_dir/restart_part2.Spat"
cmp "$output_dir/restart_full_state.nc" "$output_dir/restart_part2_state.nc"

echo "Reservoir restart example passed: continuous and split runs are byte-identical."
