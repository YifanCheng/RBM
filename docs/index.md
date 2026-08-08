# River Basin Model (RBM)

RBM simulates river temperature from river-network, streamflow, and
meteorological data. RBM can read netCDF data directly, perform
optional grid-to-HRU remapping and local-flow conversion internally, represent
reservoirs from a consolidated netCDF input, and write ASCII, netCDF, or both
output formats.

The ASCII workflow and two-argument command remain supported.

## Start Here

1. Read the [RBM User Guide](Documentation/user_guide.md) for software,
   input, build, run, and validation instructions.
2. Configure a run with the [Control-File Reference](Documentation/control_file.md).
3. Check the required [netCDF topology](Documentation/netcdf_topology.md),
   [grid-to-HRU remapping](Documentation/grid_to_hru_remap.md), and
   [reservoir](Documentation/netcdf_reservoir.md) schemas that apply to the run.
4. Interpret generated files with the [Output-File Reference](Documentation/output_files.md).

## Workflows

| Workflow | Description |
| --- | --- |
| netCDF | Completed topology, routed flow, meteorology, and optional remap/local-flow/reservoir netCDF files are consumed directly by RBM. |
| ASCII | Preprocessed flow and energy inputs and the ASCII Network file remain supported. |
| ASCII VIC-RBM | The VIC, routing, Perl, and two-argument RBM workflow is documented in the [ASCII tutorial](Documentation/tutorial.md). |

## Additional Information

- [Model overview](Overview/ModelOverview.md)
- [References](Documentation/References.md)
- [FAQ](FAQ/FAQ.md)
- [Development](Development/ModelDevelopment.md)
- [Datasets](Datasets/Datasets.md)
