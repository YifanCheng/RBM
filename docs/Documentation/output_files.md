# Output-File Reference

All output files use the configured `output_prefix`. For example:

```text
<output_prefix> /data/output/basin_run
```

may create `basin_run.Temp`, `basin_run.Spat`, `basin_run.Temp.nc`, and, for reservoir simulations, `basin_run.Resv`.

The parent output directory must exist before RBM starts.

## Output Selection

| `output_format` | Stream-temperature files |
| --- | --- |
| `ascii` | `.Temp` and `.Spat` |
| `netcdf` | `.Temp.nc` |
| `both` | `.Temp`, `.Spat`, and `.Temp.nc` |
| omitted | Same format as `input_format` |

Reservoir simulations additionally write `.Resv`; this is separate from the stream-temperature format choice.

## ASCII Temperature File: `.Temp`

The file has no header. Each row is one reported model segment at one time step:

| Column | Name | Meaning |
| --- | --- | --- |
| 1 | `time` | Decimal simulation year |
| 2 | `day` | Day of year |
| 3 | `reach` | Internal reach number |
| 4 | `cell` | Internal topology-cell number |
| 5 | `segment` | Computational segment number within the reach |
| 6 | `water_temperature_C` | Simulated stream temperature, degrees C |
| 7 | `headwater_temperature_C` | Headwater temperature, degrees C |
| 8 | `air_temperature_C` | Air temperature used by RBM, degrees C |
| 9 | `Qin_cfs` | Inflow discharge, cfs |
| 10 | `Qout_cfs` | Outflow discharge, cfs |

Temperatures are printed to two decimal places. Flow is printed to one decimal place. The file preserves the established ASCII ordering and formatting.

## ASCII Spatial File: `.Spat`

The file has no header. It maps each reported temperature row location to topology metadata:

| Column | Name | Meaning |
| --- | --- | --- |
| 1 | `reach` | Internal reach number |
| 2 | `cell` | Internal topology-cell number |
| 3 | `row` | Topology row/index metadata |
| 4 | `column` | Topology column/index metadata |
| 5 | `latitude` | Grid latitude metadata |
| 6 | `longitude` | Grid longitude metadata |
| 7 | `output_segment` | Output-segment index |

Use `.Spat` with `.Temp` when translating internal RBM reach/cell/segment positions to basin locations.

## netCDF Temperature File: `.Temp.nc`

The netCDF file is the integrated equivalent of the former postprocessing output. It contains:

| Dimension/coordinate | Meaning |
| --- | --- |
| `time` | Simulation days |
| `no_seg` | Output-segment indices |
| `hru` | Sorted topology HRU/reach identifiers |

| Variable | Dimensions | Meaning |
| --- | --- | --- |
| `T_stream` | `time, no_seg, hru` as viewed by netCDF tools | Stream temperature, degrees C |
| `T_air` | `time, no_seg, hru` | Air temperature, degrees C |
| `T_headwater` | `time, no_seg, hru` | Headwater temperature, degrees C |

`time` uses `days since <start date> 00:00:00` with the
`proleptic_gregorian` calendar. Gregorian forcing produces consecutive day
offsets. For `noleap` or `365_day` forcing, RBM preserves the forcing dates on
that Gregorian axis by omitting February 29. Missing HRU/segment combinations
are stored as NaN/fill values. Temperatures are rounded to two decimals to
reproduce postprocessed ASCII precision.

The current netCDF temperature schema does not include `Qin` or `Qout`. Select `output_format=both` when those flow columns are required together with netCDF temperatures.

Inspect a generated file with:

```bash
ncdump -h /data/output/basin_run.Temp.nc
```

## Reservoir File: `.Resv`

Reservoir simulations write one line per simulation day. The file has no header and contains:

1. simulation year;
2. day of year;
3. `nres` epilimnion temperatures;
4. `nres` hypolimnion temperatures;
5. `nres` reservoir-inflow temperatures;
6. `nres` epilimnion depths;
7. `nres` hypolimnion depths;
8. `nres` reservoir inflows; and
9. `nres` reservoir outflows.

Reservoir values are written at high decimal precision. Reservoir IDs follow the order defined by the topology and reservoir input. See [netCDF reservoir input](netcdf_reservoir.md).

## Comparing ASCII and netCDF Output

When `output_format=both`, `.Temp` and `.Temp.nc` represent the same simulated stream temperatures but use different layouts:

- `.Temp` is a flat sequence in model execution order;
- `.Temp.nc` is indexed by time, output segment, and sorted HRU ID; and
- unmodeled netCDF index combinations are missing rather than represented as rows.

Comparison must therefore align records by day, HRU/cell mapping, and output segment rather than comparing file positions directly. Temperature agreement is expected at the two-decimal output precision.

For regression comparisons between flat RBM files, the repository utility
checks record count and ordering and reports maximum/mean differences:

```bash
python3 tools/compare_rbm_outputs.py \
  /path/to/reference.Temp /path/to/candidate.Temp
```

Set comparison limits with `--temperature-tolerance` and `--flow-tolerance`.

## Output Checks

For each production run, verify:

- the first and last dates;
- the expected number of daily records and HRUs;
- that IDs correspond to the supplied topology;
- that required locations are not entirely missing;
- that stream temperatures are finite and physically plausible; and
- that a known reference case remains within the project's accepted tolerance.

A successful exit confirms that RBM completed its internal checks; it does not by itself establish scientific validity for a new basin.
