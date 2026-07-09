# RBM netCDF Topology Files

RBM supports two topology inputs:

- `<network_file>`: the legacy ASCII Network file. This remains supported.
- `<topology_file>`: a netCDF topology file read by `rbm10_VIC_nc`.

In `<topology_file>` mode, run configuration that used to live in the Network
header is supplied by the central RBM control file instead. The topology file
contains only river topology/reach/cell data.

## Build

The netCDF topology reader and converter require the netCDF-capable build:

```sh
cd src
module load gcc/11.2.0 openmpi/4.1.1 netcdf-fortran/4.5.3 netcdf/4.8.1
export LD_LIBRARY_PATH="$(nf-config --prefix)/lib:$(nc-config --prefix)/lib64:$LD_LIBRARY_PATH"

make rbm10_VIC_nc
make network_to_netcdf_topology
```

The regular `rbm10_VIC` binary does not link netCDF and cannot read
`<topology_file>`.

## Control File

Use `<topology_file>` instead of `<network_file>`:

```text
<topology_file> /path/to/basin_topology.nc
<flow_file> /path/to/flow.nc
<heat_file> /path/to/met.nc
<reservoir_file> /path/to/reservoir_parameters.txt
<reservoir_storage_file> /path/to/reservoir_storage.txt
<reservoir_sel_withdraw_file> /path/to/reservoir_withdrawal.txt
<output_prefix> /path/to/output/run_name
<start_date> 20000101
<end_date> 20001230
<source> False
<reservoir> False
<nres> 0
<select_withdraw> False
<input_format> netcdf
<output_format> both
```

Run:

```sh
./rbm10_VIC_nc /path/to/control_file
```

## netCDF Schema

Required dimensions:

| Dimension | Meaning |
| --- | --- |
| `reach` | Number of reaches |
| `cell` | Number of heat/topology cells in RBM read order |

Required scalar variables:

| Variable | Type | Meaning |
| --- | --- | --- |
| `nreach` | int | Number of reaches |
| `flow_cells` | int | Number of flow records per day |
| `heat_cells` | int | Number of heat/topology cells |

Required `reach` variables:

| Variable | Type | Meaning |
| --- | --- | --- |
| `no_cells(reach)` | int | Number of cells in each reach |
| `head_name(reach)` | int | Headwater/reach identifier |
| `trib_cell(reach)` | int | Cell where this reach enters the main stem |
| `main_stem(reach)` | int | Main-stem headwater identifier |
| `rmile_head(reach)` | float | River mile at the reach headwater |
| `alphamu(reach)` | float | Mohseni parameter |
| `beta(reach)` | float | Mohseni parameter |
| `gmma(reach)` | float | Mohseni parameter |
| `mu(reach)` | float | Mohseni parameter |
| `smooth_param(reach)` | float | Headwater smoothing parameter |

Required `cell` variables:

| Variable | Type | Meaning |
| --- | --- | --- |
| `node(cell)` | int | Node number from the old topology |
| `row(cell)` | int | Row/index metadata used in `.Spat` |
| `col(cell)` | int | Column/index metadata used in `.Spat` |
| `hru_id(cell)` | int | HRU/reach ID used to map forcing IDs |
| `downstream_id(cell)` | int | Downstream/next ID used in `.Spat` |
| `rmile(cell)` | float | River mile at this cell |
| `ndelta(cell)` | float | Number of computational elements |
| `res_num(cell)` | int | Reservoir number, or zero if none |

## Transition Utility

Existing legacy Network files can be converted to the netCDF topology schema:

```sh
./network_to_netcdf_topology /path/to/old_Network /path/to/topology.nc
```

This is a transition/helper utility. The RBM run itself does not read the old
Network file when `<topology_file>` is used.
