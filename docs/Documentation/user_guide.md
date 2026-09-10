# RBM User Guide

This guide is the starting point for running the River Basin Model (RBM). The netCDF workflow reads netCDF forcing and topology data directly, performs the required forcing conversion and optional grid-to-HRU remapping inside RBM, and writes stream temperatures as ASCII, netCDF, or both.

The ASCII workflow remains available for existing applications.

## What RBM Does

For a netCDF run, RBM:

1. reads one centralized control file;
2. reads a completed river-topology netCDF file;
3. reads streamflow and meteorological forcing from netCDF;
4. optionally remaps gridded meteorology and local runoff/baseflow to RBM HRUs;
5. computes hydraulic and energy inputs using the configured variables and coefficients;
6. simulates stream temperature, including reservoirs when configured; and
7. writes ASCII output, netCDF output, or both; and
8. optionally saves restart state for a later forecast or run chunk.

RBM does **not** construct a complete river topology from a raw flow-direction grid. The user must provide a completed topology containing connectivity, river lengths, model parameters, cell/HRU identifiers, and reservoir locations when applicable. See [netCDF topology](netcdf_topology.md).

## Choose a Workflow

| Workflow | Binary | Main inputs | Output choices |
| --- | --- | --- | --- |
| netCDF | `rbm10_VIC_nc` | control, topology, flow, meteorology | ASCII, netCDF, both |
| netCDF with grid remapping | `rbm10_VIC_nc` | above plus remap weights; optional runoff/baseflow and area | ASCII, netCDF, both |
| netCDF with reservoirs | `rbm10_VIC_nc` | above plus `reservoir_data.nc` | ASCII, netCDF, both |
| Restarted or chunked | `rbm10_VIC_nc` | normal inputs plus an optional restart state | ASCII, netCDF, both |
| ASCII control-file | `rbm10_VIC` or `rbm10_VIC_nc` | control, Network, flow, energy | ASCII; netCDF with netCDF binary |
| Two-argument ASCII | `rbm10_VIC` | input prefix and output prefix | ASCII |

Detailed option definitions are in the [control-file reference](control_file.md).

## Install and Build

### Required software

For netCDF support, install:

- a Fortran compiler compatible with `gfortran`;
- the netCDF C library; and
- the netCDF Fortran library, including `nf-config`.

No Python, xarray, pandas, GDAL, or separate preprocessing/postprocessing scripts are required to run RBM.

On CCR, the tested environment is:

```bash
module load gcc/11.2.0 openmpi/4.1.1 netcdf-fortran/4.5.3 netcdf/4.8.1
export LD_LIBRARY_PATH="$(nf-config --prefix)/lib:$(nc-config --prefix)/lib64:$LD_LIBRARY_PATH"
```

Common package names on other systems are:

| System | Packages |
| --- | --- |
| Ubuntu/Debian | `gfortran make libnetcdf-dev libnetcdff-dev` |
| Fedora/RHEL | `gcc-gfortran make netcdf-devel netcdf-fortran-devel` |
| macOS with Homebrew | `gcc make netcdf netcdf-fortran` |

Build from the repository:

```bash
cd src
make clean
make rbm10_VIC_nc
```

This creates `src/rbm10_VIC_nc`. To build the non-netCDF binary instead:

```bash
cd src
make clean
make rbm10_VIC
```

Use `make network_to_netcdf_topology` only when converting an existing ASCII Network file to the new topology format. It is a migration utility, not a required step for every run.

## Prepare the Inputs

A typical netCDF run uses:

| Input | Required | Purpose |
| --- | --- | --- |
| Control file | Yes | Paths, dates, formats, variable names, and model options |
| Topology netCDF | Yes | River connectivity, reaches/cells, lengths, HRU IDs, parameters, reservoir locations |
| Streamflow netCDF | Yes | Routed discharge for each modeled flow location |
| Meteorology netCDF | Yes | Air temperature, vapor pressure or humidity, radiation, pressure, and wind |
| Grid-to-HRU remap file | When meteorology IDs do not directly match topology HRUs | Sparse weights from source grid cells to topology HRUs |
| Local runoff/baseflow netCDF | Optional | Supplies runoff and baseflow used to calculate nonzero `Q_local` |
| Grid-cell area netCDF | Required with local runoff/baseflow | Converts runoff depth to local discharge |
| `reservoir_data.nc` | Required when reservoirs are enabled | Reservoir parameters, daily storage, and selective-withdrawal fractions |
| Restart netCDF | Optional after the first chunk | Continues river and reservoir state from the immediately preceding date |

### Meteorological inputs

RBM supports two netCDF layouts:

- **RBM-ready variables**, such as `Tair_degC`, `vp_mb`, `SW_mmKs`, `LW_mmKs`, `P_mb`, and `U10`.
- **Raw variables**, such as CLM `TSA`, `RH2M`, `FSDS`, `FLDS`, `PBOT`, and `U10`. RBM performs the documented unit conversions internally.

Variable names are configurable; they are not restricted to a single example dataset. Dimensions, IDs, units, dates, and missing values still need to satisfy the contracts described in the [control-file reference](control_file.md) and [grid-to-HRU guide](grid_to_hru_remap.md).

### Local inflow

If `local_flow_file` and `area_file` are supplied, RBM calculates `Q_local` from runoff plus baseflow and grid-cell area. If both are omitted, `Q_local` is zero. The two files must be supplied together. Include them whenever local runoff is part of the forcing dataset, and use consistent dataset versions across runs.

## Create the Control File

Paths are interpreted relative to the directory from which RBM is run unless they are absolute. Do not put quotation marks around paths.

A minimal netCDF control file is:

```text
<topology_file> /data/basin/topology.nc
<flow_file> /data/basin/streamflow.nc
<heat_file> /data/basin/meteorology.nc
<output_prefix> /data/basin/output/run

<start_date> 20000101
<end_date> 20001231

<source> False
<reservoir> False
<nres> 0
<select_withdraw> False

<input_format> netcdf
<output_format> both
```

For gridded meteorology, add a remap file:

```text
<heat_remap_file> /data/basin/grid_to_hru_weights.nc
```

For local inflow, add:

```text
<local_flow_file> /data/basin/vic_fluxes.nc
<area_file> /data/basin/domain.nc
<local_runoff_var> OUT_RUNOFF
<local_baseflow_var> OUT_BASEFLOW
<area_var> area
```

For reservoirs, add the reservoir file and enable the options described in [reservoir input](netcdf_reservoir.md).

Complete examples are included in the repository:

- `examples/netcdf_grid_remap/example.control`
- `examples/netcdf_reservoir/example.control`
- `examples/netcdf_reservoir/restart_part1.control`
- `examples/netcdf_reservoir/restart_part2.control`
- [`example_netcdf_control_file.control`](example_netcdf_control_file.control)

## Run RBM

Run from the repository root or use an absolute binary path:

```bash
./src/rbm10_VIC_nc /path/to/run.control
```

The output directory in `output_prefix` must already exist. Use `<verbose> False` for quiet production runs or `<verbose> True` while diagnosing inputs.

Two-argument ASCII execution remains supported:

```bash
./src/rbm10_VIC <input_prefix> <output_prefix>
```

## Continue a Run

Set `restart_output_file` to save state after the configured `end_date`.
Set `restart_input_file` in the next control file and begin on the immediately
following day. The netCDF-enabled binary checks that dates, calendars,
topology, model settings, and reservoir metadata are compatible before loading
the state. Use separate output prefixes for each chunk.

Run the packaged continuous-versus-split reservoir check with:

```bash
examples/netcdf_reservoir/run_restart_and_validate.sh
```

See [restart files](restart_files.md) for the full workflow and ASCII forcing
requirements.

## Select the Outputs

`output_format` controls stream-temperature output:

| `input_format` | `output_format` | Files |
| --- | --- | --- |
| `ascii` | omitted | `.Temp`, `.Spat` |
| `netcdf` | omitted | `.Temp.nc` |
| either | `ascii` | `.Temp`, `.Spat` |
| either | `netcdf` | `.Temp.nc` |
| either | `both` | `.Temp`, `.Spat`, `.Temp.nc` |

Reservoir runs additionally write `.Resv`. See [output-file reference](output_files.md) for schemas and units.

## Validate an Installation

The standard-library unit tests require Python 3 but no third-party packages:

```bash
cd src
make test
```

The maintained end-to-end netCDF examples require the netCDF build and can be
run together with:

```bash
cd src
make test_netcdf
```

From the repository root, either example can also be run directly:

```bash
examples/netcdf_grid_remap/run_and_validate.sh
examples/netcdf_reservoir/run_and_validate.sh
examples/netcdf_reservoir/run_restart_and_validate.sh
```

Validation should check more than successful execution:

- output record count and simulation dates;
- topology/segment ordering;
- missing and non-finite values;
- plausible stream-temperature range;
- ASCII/netCDF parity when `output_format=both`; and
- agreement with an established basin reference when one is available.

## Troubleshooting

### `nf-config` is not found

Install the netCDF Fortran development library or load the appropriate environment module. The netCDF C package alone is insufficient.

### Shared netCDF library cannot be loaded

Add the netCDF library directories to `LD_LIBRARY_PATH`, or configure your system linker. The CCR command above shows the tested layout.

### netCDF output requested from `rbm10_VIC`

Use `rbm10_VIC_nc`. The regular binary intentionally fails with a clear diagnostic when netCDF input or output is requested.

### Variable not found or incompatible dimensions

Inspect the file with `ncdump -h file.nc`, then set the corresponding `*_var` and ID controls. Confirm the time dimension and ID arrays match the topology and remap contracts.

### Wrong dates or no data

Use exact `YYYYMMDD` dates. Both endpoints are inclusive. All forcing files must cover the requested period.

### Grid IDs do not match topology HRUs

Provide `heat_remap_file`; do not assume array position is a valid HRU mapping. See [grid-to-HRU remapping](grid_to_hru_remap.md).

### Reservoir validation fails

Confirm `reservoir=True`, `nres` matches the reservoir dimension, the topology identifies the same reservoir locations, and `reservoir_data.nc` covers every requested date.

## ASCII Support

RBM continues to support:

- the two-argument ASCII interface;
- ASCII Network, flow, and energy files;
- ASCII `.Temp` and `.Spat` output; and
- control-file execution with either input format.

The VIC, routing, Perl, and ASCII preparation procedure is documented in the
[ASCII VIC-RBM tutorial](tutorial.md) and the ASCII section of
[Running RBM](run_RBM.md). These preparation steps are not required for a
netCDF run.
