# netCDF Grid-Remapping Example

This two-day example demonstrates direct netCDF input without an ASCII Network
file or external preprocessing:

- river topology is read from netCDF;
- routed streamflow is read from netCDF;
- raw gridded meteorology is read from netCDF;
- meteorology is remapped to RBM HRUs internally;
- ASCII and netCDF temperature outputs are written in one run.

The example is intentionally small so it can be stored with the source code and
used as an installation check.

## Build

Install a Fortran compiler plus the netCDF C and Fortran development libraries.
Both `nc-config` and `nf-config` must be available. Then run:

~~~sh
cd src
make clean
make rbm10_VIC_nc
cd ..
~~~

On CCR, load the tested modules first:

~~~sh
module load gcc/11.2.0 openmpi/4.1.1 netcdf-fortran/4.5.3 netcdf/4.8.1
export LD_LIBRARY_PATH="$(nf-config --prefix)/lib:$(nc-config --prefix)/lib64:$LD_LIBRARY_PATH"
~~~

## Run

Run from the repository root:

~~~sh
./src/rbm10_VIC_nc examples/netcdf_grid_remap/example.control
~~~

The run creates:

~~~text
examples/netcdf_grid_remap/output/example.Temp
examples/netcdf_grid_remap/output/example.Spat
examples/netcdf_grid_remap/output/example.Temp.nc
~~~

Reservoir files are not needed because this example sets `<reservoir> False`.

## Validate

Run the complete example and compare its output with the included reference:

~~~sh
examples/netcdf_grid_remap/run_and_validate.sh
~~~

The validation script uses Python 3 only for the dependency-free ASCII output
comparator. Python, xarray, and numpy are not required to run RBM itself.

## Files

| File | Purpose |
| --- | --- |
| `example.control` | Ready-to-run centralized configuration |
| `input/topology.nc` | River topology without an ASCII Network header |
| `input/streamflow.nc` | Two days of routed streamflow |
| `input/meteorology_grid.nc` | Two days of raw gridded meteorology |
| `input/grid_to_hru_weights.nc` | Grid-to-HRU remapping weights |
| `expected/example.Temp` | Expected ASCII temperature records |
| `expected/example.Spat` | Expected segment-to-location mapping |
| `expected/example.Temp.nc` | Expected netCDF output schema and values |

The compact dataset provides a deterministic installation and workflow check.
