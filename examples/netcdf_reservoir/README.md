# netCDF Reservoir Integration Example

This two-day case exercises the complete netCDF reservoir path:

- netCDF river topology with reservoir cell assignments;
- raw gridded streamflow and meteorology;
- internal runoff-plus-baseflow conversion using grid-cell area;
- consolidated `reservoir_data.nc` parameters and daily forcing;
- ASCII and netCDF temperature output.

It is intentionally short so users can verify an installation without running
the full multi-decade reservoir simulation.

## Run

Build the netCDF executable, then run from the repository root:

```sh
cd src
make rbm10_VIC_nc
cd ..

examples/netcdf_reservoir/run_and_validate.sh
```

The script runs RBM and compares `.Temp`, `.Spat`, `.Temp.nc`, and `.Resv`
byte-for-byte with the packaged expected outputs.

## Restart Validation

The same fixture includes a continuous 30-day run and two 15-day runs linked
by a restart file:

```sh
examples/netcdf_reservoir/run_restart_and_validate.sh
```

The script verifies that concatenated `.Temp` and `.Resv` outputs, both
`.Spat` files, and the final serialized model state are byte-identical to the
continuous run. The controls are `restart_full.control`,
`restart_part1.control`, and `restart_part2.control`.
All three controls use `output_format=both` and produce `.Temp.nc` files.

## Inputs

| File | Purpose |
| --- | --- |
| `input/topology.nc` | Completed river topology and reservoir cell IDs |
| `input/streamflow.nc` | Thirty daily gridded streamflow records |
| `input/meteorology_and_runoff.nc` | Thirty daily meteorology, runoff, and baseflow records |
| `input/grid_area.nc` | Grid-cell area for local-flow conversion |
| `input/reservoir_data.nc` | Parameters and thirty daily storage and withdrawal records |
| `example.control` | Ready-to-run centralized configuration |

The fixture is a compact subset of the project reservoir validation case. It is
a software regression and installation test, not an observational calibration
dataset.

## Expected Outputs

| File | Purpose |
| --- | --- |
| `expected/example.Temp` | ASCII-format temperatures |
| `expected/example.Spat` | ASCII spatial metadata |
| `expected/example.Temp.nc` | Postprocess-style temperature netCDF |
| `expected/example.Resv` | Reservoir diagnostic output |

Generated files are written under `output/` and are ignored by Git except for
`output/.gitkeep`.
