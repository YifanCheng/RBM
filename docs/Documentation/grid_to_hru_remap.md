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

For preprocessing compatibility, this weight-file schema is consumed
positionally in the mizuRoute reach order. The historical notebook and its
ASCII energy output use that ordering even when `polyid` is stored in a
different order. RBM validates IDs and indices, but changing this path to
`polyid` lookup changes the established reference results. Weight files should
therefore be generated for the same reach set and order as the flow file.

## Direct Grid-Cell Mapping

When flow or meteorology variables have `time`, `lat`, and `lon` dimensions and
no heat remap file is supplied, RBM resolves each topology `grid_lat` and
`grid_lon` against each source grid once during initialization. Flow and
meteorology are mapped independently, so their grid extents and index positions
may differ.

Flow and meteorology time axes are also resolved independently. Each file must
provide one consecutive daily record and a `time` coordinate with units of
`days since YYYY-MM-DD`.

The older VIC-style meteorology schema can be selected with control entries such
as `OUT_AIR_TEMP`, `OUT_VP`, `OUT_SWDOWN`, `OUT_LWDOWN`, `OUT_PRESSURE`,
`OUT_WIND`, and optional `OUT_DENSITY`.

## Optional Local Flow

Routed streamflow alone does not contain the local runoff contribution used by
all legacy RBM cases. To reproduce a nonzero ASCII `Q_local` column, supply both
of these control entries:

```text
<local_flow_file> /path/to/vic_runoff_and_baseflow.nc
<area_file> /path/to/vic_grid_area.nc
```

The local-flow file must contain daily gridded `OUT_RUNOFF` and `OUT_BASEFLOW`
in mm. The area file must contain a static gridded `area` variable in m2. The
variable names can be overridden with `<local_runoff_var>`,
`<local_baseflow_var>`, and `<area_var>`.

RBM maps both grids independently using topology latitude/longitude and applies
the preprocessing formula:

```text
Q_local [cfs] = (runoff + baseflow) [mm/day] * area [m2]
                * (1 / 0.3048)^3 / 86400 / 1000
```

The result is rounded to 0.1 cfs, matching the legacy ASCII forcing format.
`Q_diff` remains zero, as in the supplied preprocessing script. If the two
optional files are omitted, `Q_local` remains zero for compatibility with the
previous integrated netCDF behavior.

## Notes

- This remapping currently covers meteorology/heat inputs.
- Streamflow is still read from the flow netCDF file.
- RBM reads each required netCDF variable once per simulation day and then
  performs cell mapping from an in-memory daily slice.
- Variables must use `(time, hru)` or `(time, lat, lon)` in the netCDF file.
  Required raw-variable units and missing values are validated before values
  enter the model.
- Direct gridded streamflow supplies routed discharge. Nonzero `Q_local`
  requires the separate runoff/baseflow and grid-area inputs described above;
  it cannot be reconstructed exactly from regulated routed streamflow alone.
- The implementation follows the old preprocessing workflow's positional
  reach-order behavior so generated RBM results match the existing reference
  workflow as closely as possible.
