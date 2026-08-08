# Control-File Reference

RBM control-file mode is invoked with one argument:

```bash
./src/rbm10_VIC_nc /path/to/run.control
```

The control file centralizes file paths, simulation dates, format choices, variable mappings, hydraulics, and reservoir options.

## Syntax

Each setting uses:

```text
<name> value
```

- Blank lines and lines beginning with `!` are ignored.
- Text after `!` is an inline comment.
- Do not quote file paths.
- Relative paths are resolved from the directory where the executable is launched.
- Control names and format values are case-sensitive; use the lowercase names documented here.
- Boolean values beginning with `T`, `t`, `Y`, `y`, or `1` are true. Other values are false. Use `True` and `False` for clarity.
- Unknown control names produce a warning and are ignored.
- Dates must be valid calendar dates in `YYYYMMDD` format.

## Required Run Controls

| Control | Required | Default | Meaning |
| --- | --- | --- | --- |
| `topology_file` | One topology input required | none | River-topology netCDF; requires `rbm10_VIC_nc` |
| `network_file` | Alternative to `topology_file` | none | ASCII Network file |
| `flow_file` | Yes | none | ASCII or netCDF routed streamflow, according to `input_format` |
| `heat_file` | Yes | none | ASCII energy forcing or netCDF meteorology, according to `input_format` |
| `output_prefix` | Yes | none | Path and base name for generated files; parent directory must exist |
| `start_date` | Yes | none | First simulated day, inclusive, as `YYYYMMDD` |
| `end_date` | Yes | none | Last simulated day, inclusive, as `YYYYMMDD` |
| `input_format` | No | `ascii` | `ascii` or `netcdf` |
| `output_format` | No | same as `input_format` | `ascii`, `netcdf`, or `both` |
| `verbose` | No | `True` | Enables initialization and input diagnostics |

Provide exactly one of `topology_file` and `network_file`. A raw flow-direction grid is not a substitute for the completed topology file. See [netCDF topology](netcdf_topology.md).

## Model Options

| Control | Default | Meaning |
| --- | --- | --- |
| `source` | `False` | ASCII point-source option. Point-source file ingestion is currently a placeholder; keep `False` for supported runs. |
| `reservoir` | `False` | Enables reservoir simulation |
| `nres` | `0` | Number of reservoirs; must be positive when reservoirs are enabled and zero otherwise |
| `select_withdraw` | `False` | Enables selective withdrawal; requires `reservoir=True` |

## Input and Output Formats

`input_format=ascii` reads the established preprocessed flow and energy text files. `input_format=netcdf` reads forcing directly and applies the documented unit conversions internally.

| Input | Output omitted | `output_format=ascii` | `output_format=netcdf` | `output_format=both` |
| --- | --- | --- | --- | --- |
| `ascii` | `.Temp`, `.Spat` | `.Temp`, `.Spat` | `.Temp.nc` | all three |
| `netcdf` | `.Temp.nc` | `.Temp`, `.Spat` | `.Temp.nc` | all three |

Any netCDF input, topology, reservoir file, or output requires `rbm10_VIC_nc`. See [output files](output_files.md).

## Optional Grid and Local-Flow Inputs

| Control | Default | Meaning |
| --- | --- | --- |
| `heat_remap_file` | omitted | netCDF sparse grid-to-HRU weights for gridded meteorology |
| `local_flow_file` | omitted | Daily runoff and baseflow netCDF used to calculate `Q_local` |
| `area_file` | omitted | Static grid-cell-area netCDF used with `local_flow_file` |
| `local_runoff_var` | `OUT_RUNOFF` | Runoff variable in `local_flow_file` |
| `local_baseflow_var` | `OUT_BASEFLOW` | Baseflow variable in `local_flow_file` |
| `area_var` | `area` | Cell-area variable in `area_file` |

`local_flow_file` and `area_file` apply only to netCDF input and must be supplied together. If omitted, `Q_local` is zero. RBM sets `Q_diff` to zero for netCDF forcing. Use consistent runoff, baseflow, and area dataset versions across runs because these inputs determine the calculated local discharge. The formula and remap schema are documented in [grid-to-HRU remapping](grid_to_hru_remap.md).

## Reservoir Inputs

Consolidated netCDF input:

| Control | Meaning |
| --- | --- |
| `reservoir_data_file` | Consolidated netCDF parameters, daily storage, and withdrawal fractions |

ASCII alternative:

| Control | Meaning |
| --- | --- |
| `reservoir_file` | ASCII static reservoir parameters |
| `reservoir_storage_file` | ASCII daily storage |
| `reservoir_sel_withdraw_file` | ASCII daily selective-withdrawal fractions |

When `reservoir=True`, provide either `reservoir_data_file` or all three ASCII files, never both. See [netCDF reservoir input](netcdf_reservoir.md).

## Flow Variables and Hydraulics

| Control | Default | Meaning |
| --- | --- | --- |
| `flow_var` | `KWroutedRunoff` | Routed-flow variable |
| `flow_id_var` | `reachID` | Flow-location identifier variable |
| `flow_units` | `m3/s` | Routed-flow units; accepted values are `m3/s` and `cfs` |
| `a_d` | `0.408` | Depth coefficient in `depth = a_d * Q^b_d` |
| `b_d` | `0.392` | Depth exponent |
| `a_w` | `4.346` | Width coefficient in `width = a_w * Q^b_w` |
| `b_w` | `0.520` | Width exponent |
| `min_flow` | `5.0` | Minimum flow, in cfs after conversion |
| `min_velocity` | `0.5` | Minimum velocity in ft/s |
| `min_depth` | `5.0` | Minimum depth in ft |

The default hydraulic coefficients are general configuration values and should be reviewed for each basin. Coefficients `a_d` and `a_w` must be positive; exponents must be nonnegative. Limits must be nonnegative for flow and positive for velocity and depth.

## Meteorology Variables

### RBM-ready data

These defaults read meteorology that has already been converted to RBM units:

| Control | Default variable | Expected quantity |
| --- | --- | --- |
| `heat_id_var` | `hruId` | HRU/location metadata used to validate vector forcing |
| `heat_tair_var` | `Tair_degC` | Air temperature in degrees C |
| `heat_vp_var` | `vp_mb` | Vapor pressure in mb |
| `heat_sw_var` | `SW_mmKs` | Shortwave radiation in mm K/s |
| `heat_lw_var` | `LW_mmKs` | Longwave radiation in mm K/s |
| `heat_press_var` | `P_mb` | Pressure in mb |
| `heat_wind_var` | `U10` | Wind speed in m/s |
| `heat_density_var` | empty | Optional air density variable |

RBM-ready vector forcing must be stored in routed-flow location order. `heat_id_var` is validated for uniqueness,
but it does not reorder vector values because converted vector files use
positional flow ordering even when their `hruId` metadata is ordered differently.

### Raw CLM-style data

Set:

```text
<heat_tair_var> TSA
```

RBM then selects the raw schema and reads `TSA`, `RH2M`, `FSDS`, `FLDS`, `PBOT`, and `U10`, applying the established conversions internally. For three-dimensional grid variables, provide either topology grid coordinates for direct mapping or `heat_remap_file` for weighted HRU remapping.

For VIC-style grid data, set the controls to the file's actual variables, for example:

```text
<heat_tair_var> OUT_AIR_TEMP
<heat_vp_var> OUT_VP
<heat_sw_var> OUT_SWDOWN
<heat_lw_var> OUT_LWDOWN
<heat_press_var> OUT_PRESSURE
<heat_wind_var> OUT_WIND
<heat_density_var> OUT_DENSITY
```

Inspect unfamiliar files with `ncdump -h file.nc` before configuring the variables.

## Example: netCDF Run

```text
<topology_file> /data/basin/topology.nc
<flow_file> /data/basin/streamflow.nc
<heat_file> /data/basin/met_grid.nc
<heat_remap_file> /data/basin/grid_to_hru.nc
<local_flow_file> /data/basin/vic_fluxes.nc
<area_file> /data/basin/domain.nc
<output_prefix> /data/basin/output/run

<start_date> 19790101
<end_date> 19811231
<source> False
<reservoir> False
<nres> 0
<select_withdraw> False
<verbose> False

<input_format> netcdf
<output_format> both
<flow_units> m3/s
<heat_tair_var> TSA
```

## Example: Reservoir Run

```text
<topology_file> /data/basin/topology.nc
<flow_file> /data/basin/streamflow.nc
<heat_file> /data/basin/met_grid.nc
<local_flow_file> /data/basin/vic_fluxes.nc
<area_file> /data/basin/domain.nc
<reservoir_data_file> /data/basin/reservoir_data.nc
<output_prefix> /data/basin/output/reservoir_run

<start_date> 19790101
<end_date> 19811231
<source> False
<reservoir> True
<nres> 38
<select_withdraw> True

<input_format> netcdf
<output_format> netcdf
<heat_tair_var> OUT_AIR_TEMP
<heat_vp_var> OUT_VP
<heat_sw_var> OUT_SWDOWN
<heat_lw_var> OUT_LWDOWN
<heat_press_var> OUT_PRESSURE
<heat_wind_var> OUT_WIND
<heat_density_var> OUT_DENSITY
```

## Example: Existing ASCII Workflow

```text
<network_file> /data/basin/network
<flow_file> /data/basin/flow_ascii
<heat_file> /data/basin/energy_ascii
<output_prefix> /data/basin/output/ascii_run
<start_date> 20000101
<end_date> 20001230
<source> False
<reservoir> False
<nres> 0
<select_withdraw> False
<input_format> ascii
<output_format> ascii
```

## Validation Rules

RBM stops before simulation when:

- neither or both topology controls are supplied;
- required forcing, output, or date controls are missing;
- a format or flow-unit value is unsupported;
- a date is invalid or the end precedes the start;
- only one local-flow input is supplied;
- reservoir options or file combinations conflict; or
- hydraulic coefficients and limits are invalid.

File-specific netCDF validation additionally checks required variables, dimensions, IDs, units, time coverage, missing values, remap indices and weights, and reservoir consistency.
