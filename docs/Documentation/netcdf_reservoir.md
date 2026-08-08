# RBM netCDF Reservoir Input

Reservoir runs separate river structure from reservoir forcing:

- `topology.nc` identifies reservoir cells with `res_num(cell)`;
- `reservoir_data.nc` contains reservoir parameters, daily storage, and
  selective-withdrawal fractions.

RBM does not construct either dataset. Users must provide a completed topology
and reservoir data appropriate for their basin.

## Control File

Use the netCDF-capable executable and provide:

```text
<reservoir> True
<nres> 38
<select_withdraw> True
<reservoir_data_file> /path/to/reservoir_data.nc
```

Do not also set `reservoir_file`, `reservoir_storage_file`, or
`reservoir_sel_withdraw_file`. Those fields remain available together for
ASCII reservoir input.

`reservoir_data.nc` is independent of `input_format`: it can be used with
ASCII or netCDF forcing when running `rbm10_VIC_nc`.

## Schema

Required dimensions:

| Dimension | Meaning |
| --- | --- |
| `reservoir` | Number of reservoirs; must equal `nres` |
| `time` | Consecutive daily records |

Required static variables:

| Variable | Type | Dimensions | Meaning |
| --- | --- | --- | --- |
| `reservoir_id` | int | `reservoir` | IDs ordered from 1 through `nres` |
| `grid_lat` | numeric | `reservoir` | Reservoir latitude |
| `grid_lon` | numeric | `reservoir` | Reservoir longitude |
| `depth_m` | numeric | `reservoir` | Reservoir depth in meters |
| `width_m` | numeric | `reservoir` | Reservoir width in meters |
| `area_km2` | numeric | `reservoir` | Surface area in square kilometers |
| `capacity_mcm` | numeric | `reservoir` | Capacity in million cubic meters |
| `start_node` | int | `reservoir` | First topology cell in the reservoir |
| `end_node` | int | `reservoir` | Last topology cell in the reservoir |

Required daily variables:

| Variable | Type | Dimensions | Meaning |
| --- | --- | --- | --- |
| `time` | numeric | `time` | Daily offsets |
| `storage` | numeric | `time, reservoir` | Storage in cubic meters |
| `withdraw_epilimnion` | numeric | `time, reservoir` | Upper-layer fraction |
| `withdraw_hypolimnion` | numeric | `time, reservoir` | Lower-layer fraction |

`time` must use `days since YYYY-MM-DD` and a Gregorian calendar. Records must
be daily and consecutive. Withdrawal fractions must be finite, nonnegative,
and sum to one for every reservoir and day.

RBM validates dimensions, IDs, dates, positive physical parameters, storage,
and withdrawal fractions before using them.

## Convert ASCII Inputs

The native Fortran converter requires only the netCDF C and Fortran libraries
already used by `rbm10_VIC_nc`:

```sh
cd src
make reservoir_to_netcdf

./reservoir_to_netcdf \
  /path/to/reservoir_parameters.txt \
  /path/to/reservoir_storage.txt \
  /path/to/reservoir_withdrawal.txt \
  /path/to/reservoir_data.nc
```

The established input formats contain one parameter header and one row per
reservoir, then one date followed by `nres` values for every storage or
withdrawal day.

## Backward Compatibility

The original fields remain supported:

```text
<reservoir_file> /path/to/reservoir_parameters.txt
<reservoir_storage_file> /path/to/reservoir_storage.txt
<reservoir_sel_withdraw_file> /path/to/reservoir_withdrawal.txt
```

Two-argument ASCII execution and ASCII reservoir reads are unchanged. The
regular `rbm10_VIC` binary reports a clear error if `reservoir_data_file` is
requested because it intentionally does not link against netCDF.
