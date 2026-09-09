# Restart Files

RBM can save model state at the end of a successful run and restore that state
in a later run. This supports forecast initialization and divides long
simulations into shorter jobs without restarting river or reservoir
temperatures from default values.

Restart files require the netCDF-enabled executable:

```sh
./src/rbm10_VIC_nc /path/to/chunk.control
```

## Control Fields

| Control | Meaning |
| --- | --- |
| `restart_input_file` | Optional restart file produced by the immediately preceding run |
| `restart_output_file` | Optional path for model state after `end_date` |

Omit `restart_input_file` for the first chunk. Set
`restart_output_file` when a later chunk will continue from the run.

The saved state represents the model after the complete `end_date`.
A run that reads it must set `start_date` to the immediately following
calendar day. RBM rejects gaps, overlaps, incompatible calendars, and
incompatible model configurations.

## Example

First chunk:

```text
<start_date> 19790101
<end_date> 19791231
<restart_output_file> /data/output/state_19791231.nc
```

Second chunk:

```text
<start_date> 19800101
<end_date> 19801231
<restart_input_file> /data/output/state_19791231.nc
<restart_output_file> /data/output/state_19801231.nc
```

Use a different `output_prefix` for each chunk. Temperature and reservoir
output files remain separate by chunk; the restart file continues model state
but does not append or merge output files.

## Forcing Requirements

For netCDF forcing, restart files retain the exact next daily position for
flow, heat, and optional local-flow inputs. A resumed chunk may use the same
multi-year archive or a later archive with a different time origin, provided
that each file contains the saved next record. This also preserves record
continuity when a `noleap` forcing calendar crosses a Gregorian leap date.

Restart files also retain the next netCDF output date. Consequently, netCDF
temperature files written by consecutive chunks have continuous time
coordinates, including when a `noleap` run crosses February 29.

ASCII forcing files do not contain a calendar origin. They are read
sequentially, so the flow and energy files supplied to each chunk must begin
with that chunk `start_date`. This is the same requirement as any standalone
ASCII control-file run that starts partway through a forcing archive.

## Saved State and Validation

The restart file stores the active and inactive river-temperature buffers,
river storage and storage temperature, headwater and smoothed air
temperatures, tributary state, and reservoir thermal/storage state when
reservoirs are enabled.

Before loading state, RBM verifies:

- restart schema version and state date;
- input mode, forcing-calendar classes, local-flow configuration, and next
  netCDF forcing positions;
- next netCDF output date;
- reach, cell, reservoir, and within-day time-step counts;
- topology IDs, segment mapping, distances, and Mohseni parameters;
- reservoir IDs, node assignments, area, and capacity.

A restart file is tied to the topology and model configuration that produced
it. It is not a forcing file and should not be edited manually.

RBM writes `restart_output_file` only after the requested simulation finishes
successfully. The current implementation writes
an end-of-run checkpoint; it does not create periodic checkpoints during a
single run.

## Included Validation

The reservoir example includes a continuous two-day run and the same run split
into one-day chunks:

```sh
examples/netcdf_reservoir/run_restart_and_validate.sh
```

The script compares concatenated temperature and reservoir outputs, spatial
metadata, and final restart state byte-for-byte.
