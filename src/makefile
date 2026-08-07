#
# Makefile for the grid-based semi-Lagrangian water temperature model, RBM10_VIC
#
# Start of the makefile
#
# Defining variables
#
objects = rbm10_VIC.o Begin.o Systmm.o Particle_Track.o \
          Energy.o Julian.o tntrp.o Read_Forcing.o \
          Block_Energy.o Block_Hydro.o Block_Network.o \
          data_type.o Block_Reservoir.o Block_Control.o Read_Control.o \
          density_subroutine.o flow_subroutine.o \
          Error_estimate.o \
          reservoir_subroutine_implicit.o \
          Water_Balance.o Write.o
objects_nc = rbm10_VIC_nc.o Begin_nc.o Systmm_nc.o Particle_Track.o \
          Energy.o Julian.o tntrp.o Read_Forcing_nc.o \
          Block_Energy.o Block_Hydro.o Block_Network.o \
          data_type.o Block_Reservoir.o Block_Control.o Read_Control.o \
          density_subroutine.o flow_subroutine.o \
          Error_estimate.o \
          reservoir_subroutine_implicit.o \
          Water_Balance.o Write_nc.o Netcdf_Output.o Netcdf_Input.o Netcdf_Topology.o
f90comp = gfortran # -O0 -g #-O2
NC_FFLAGS = $(shell nf-config --fflags 2>/dev/null)
NC_LIBS = $(shell nf-config --flibs 2>/dev/null) $(shell nc-config --libs 2>/dev/null)
.PHONY: check_netcdf test clean
# Makefile
rbm10_VIC: $(objects)
	$(f90comp) -o rbm10_VIC $(objects)
rbm10_VIC_nc: check_netcdf $(objects_nc)
	$(f90comp) -o rbm10_VIC_nc $(objects_nc) $(NC_LIBS)
network_to_netcdf_topology: check_netcdf Network_To_Netcdf_Topology.o
	$(f90comp) -o network_to_netcdf_topology Network_To_Netcdf_Topology.o $(NC_LIBS)
check_netcdf:
	@command -v nf-config >/dev/null 2>&1 || { echo "nf-config not found. Load netcdf-fortran before building rbm10_VIC_nc."; exit 1; }
	@command -v nc-config >/dev/null 2>&1 || { echo "nc-config not found. Load netcdf before building rbm10_VIC_nc."; exit 1; }
test:
	cd .. && python3 -m unittest discover -s tests -v
Block_Energy.o: Block_Energy.f90
	$(f90comp) -c Block_Energy.f90
block_energy.mod: Block_Energy.f90
	$(f90comp) -c Block_Energy.f90
Block_Hydro.o: Block_Hydro.f90
	$(f90comp) -c Block_Hydro.f90
block_hydro.mod: Block_Hydro.o Block_Hydro.f90
	$(f90comp) -c Block_Hydro.f90
Block_Network.o: Block_Network.f90
	$(f90comp) -c Block_Network.f90
block_network.mod: Block_Network.o Block_Network.f90
	$(f90comp) -c Block_Network.f90
data_type.o: data_type.f90
	$(f90comp) -c data_type.f90
data_type.mod: data_type.o data_type.f90
	$(f90comp) -c data_type.f90
Block_Reservoir.o: data_type.mod Block_Reservoir.f90
	$(f90comp) -c Block_Reservoir.f90
block_reservoir.mod: data_type.mod Block_Reservoir.o Block_Reservoir.f90
	$(f90comp) -c Block_Reservoir.f90
Block_Control.o: Block_Control.f90
	$(f90comp) -c Block_Control.f90
block_control.mod: Block_Control.o
Read_Control.o: block_control.mod Read_Control.f90
	$(f90comp) -c Read_Control.f90
Begin.o: block_energy.mod block_hydro.mod block_network.mod block_reservoir.mod block_control.mod Begin.f90
	$(f90comp) -cpp -c Begin.f90
Begin_nc.o: block_energy.mod block_hydro.mod block_network.mod block_reservoir.mod block_control.mod netcdf_topology.mod Begin.f90
	$(f90comp) -cpp -DUSE_NETCDF $(NC_FFLAGS) -c Begin.f90 -o Begin_nc.o
Read_Forcing.o: block_energy.mod block_hydro.mod block_network.mod block_reservoir.mod block_control.mod Read_Forcing.f90
	$(f90comp) -cpp -c Read_Forcing.f90 
Read_Forcing_nc.o: block_energy.mod block_hydro.mod block_network.mod block_reservoir.mod block_control.mod netcdf_input.mod Read_Forcing.f90
	$(f90comp) -cpp -DUSE_NETCDF $(NC_FFLAGS) -c Read_Forcing.f90 -o Read_Forcing_nc.o
Systmm.o: block_network.mod block_energy.mod block_hydro.mod block_reservoir.mod block_control.mod Systmm.f90
	$(f90comp) -cpp -c Systmm.f90
Systmm_nc.o: block_network.mod block_energy.mod block_hydro.mod block_reservoir.mod block_control.mod netcdf_output.mod netcdf_input.mod Systmm.f90
	$(f90comp) -cpp -DUSE_NETCDF $(NC_FFLAGS) -c Systmm.f90 -o Systmm_nc.o
Energy.o: block_energy.mod Energy.f90
	$(f90comp) -c Energy.f90
Particle_Track.o: block_hydro.mod block_network.mod block_reservoir.mod Particle_Track.f90
	$(f90comp) -c Particle_Track.f90
Write.o: block_control.mod Write.f90
	$(f90comp) -cpp -c Write.f90
Write_nc.o: block_control.mod netcdf_output.mod Write.f90
	$(f90comp) -cpp -DUSE_NETCDF $(NC_FFLAGS) -c Write.f90 -o Write_nc.o
Netcdf_Output.o: Netcdf_Output.f90
	$(f90comp) $(NC_FFLAGS) -c Netcdf_Output.f90
netcdf_output.mod: Netcdf_Output.o
Netcdf_Input.o: block_control.mod block_energy.mod block_hydro.mod block_network.mod block_reservoir.mod Netcdf_Input.f90
	$(f90comp) $(NC_FFLAGS) -c Netcdf_Input.f90
netcdf_input.mod: Netcdf_Input.o
Netcdf_Topology.o: block_control.mod block_energy.mod block_hydro.mod block_network.mod block_reservoir.mod Netcdf_Topology.f90
	$(f90comp) $(NC_FFLAGS) -c Netcdf_Topology.f90
netcdf_topology.mod: Netcdf_Topology.o
Network_To_Netcdf_Topology.o: Network_To_Netcdf_Topology.f90
	$(f90comp) $(NC_FFLAGS) -c Network_To_Netcdf_Topology.f90
density_subroutine.o: data_type.mod density_subroutine.f90
	$(f90comp) -c density_subroutine.f90
flow_subroutine.o: block_hydro.mod block_network.mod block_reservoir.mod flow_subroutine.f90
	$(f90comp) -c flow_subroutine.f90
Error_estimate.o: data_type.mod block_network.mod block_reservoir.mod Error_estimate.f90
	$(f90comp) -c Error_estimate.f90
reservoir_subroutine_implicit.o: data_type.mod block_network.mod block_reservoir.mod reservoir_subroutine_implicit.f90
	$(f90comp) -c reservoir_subroutine_implicit.f90
Julian.o: Julian.f90
	$(f90comp) -c Julian.f90
tntrp.o: tntrp.f90
	$(f90comp) -c tntrp.f90
Water_Balance.o: block_hydro.mod block_network.mod Water_Balance.f90
	$(f90comp) -c Water_Balance.f90
rbm10_VIC.o: block_control.mod rbm10_VIC.f90
	$(f90comp) -cpp -c rbm10_VIC.f90
rbm10_VIC_nc.o: block_control.mod rbm10_VIC.f90
	$(f90comp) -cpp -DUSE_NETCDF $(NC_FFLAGS) -c rbm10_VIC.f90 -o rbm10_VIC_nc.o

# Cleaning everything
clean:
	rm -f block_energy.mod block_hydro.mod block_network.mod block_reservoir.mod \
           block_control.mod \
           rbm10_VIC
	rm -f $(objects)
	rm -f rbm10_VIC_nc rbm10_VIC_nc.o Begin_nc.o Systmm_nc.o Write_nc.o Read_Forcing_nc.o Netcdf_Output.o Netcdf_Input.o Netcdf_Topology.o netcdf_output.mod netcdf_input.mod netcdf_topology.mod
	rm -f network_to_netcdf_topology Network_To_Netcdf_Topology.o
