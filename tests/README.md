# Validation Tests

Run the dependency-free comparator unit tests from `src`:

```sh
make test
```

With the netCDF C and Fortran libraries loaded, build RBM and run the complete
packaged integration example with:

```sh
make test_netcdf
```

The integration fixture is stored in `examples/netcdf_grid_remap` so users can
also run it as an installation example. It validates netCDF topology,
streamflow input, raw-grid meteorology remapping, ASCII output, netCDF output,
and reference-output parity.

A second fixture in `examples/netcdf_reservoir` validates a two-day reservoir
run with netCDF topology, gridded streamflow and meteorology, nonzero
`Q_local` from runoff/baseflow and area, consolidated `reservoir_data.nc`,
selective withdrawal, and all expected output files.

`make test_netcdf` runs both complete integration fixtures.
