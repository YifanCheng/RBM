# Grid-to-HRU Meteorology Remapping

RBM can optionally remap raw grid meteorology netCDF data to the HRU/reach order
used internally by the model.

This mode is enabled by adding `<heat_remap_file>` to the RBM control file:

```text
<heat_file> /path/to/grid_meteorology.nc
<heat_remap_file> /path/to/spatialweights_basin.nc
<input_format> netcdf
```

When `<heat_remap_file>` is omitted, RBM assumes `<heat_file>` is already
HRU/reach-remapped.

## Required Grid Meteorology Variables

The grid meteorology file must contain raw CLM-style variables:

| Variable | Meaning |
| --- | --- |
| `TSA` | Air temperature in K |
| `RH2M` | Relative humidity in percent |
| `FSDS` | Incoming shortwave radiation in W/m2 |
| `FLDS` | Incoming longwave radiation in W/m2 |
| `PBOT` | Surface pressure in Pa |
| `U10` | 10 m wind speed in m/s |

The variables are expected on a grid with dimensions equivalent to
`time, lat, lon`.

## Required Weight File Variables

The spatial weight file must contain:

| Variable | Meaning |
| --- | --- |
| `polyid(polyid)` | HRU/reach IDs in the remapping file |
| `overlaps(polyid)` | Number of source grid cells used by each HRU/reach |
| `weight(data)` | Overlap weights |
| `i_index(data)` | Source grid x/lon index, 1-based |
| `j_index(data)` | Source grid y/lat index, 1-based |

RBM normalizes the positive weights for each HRU/reach before computing the
weighted average, matching the existing Python remapping workflow.

## Notes

- This remapping currently covers meteorology/heat inputs.
- Streamflow is still read from the flow netCDF file.
- The implementation follows the old preprocessing workflow's positional
  reach-order behavior so generated RBM results match the existing reference
  workflow as closely as possible.
