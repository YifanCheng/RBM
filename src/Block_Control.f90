Module Block_Control
!
! Centralized run-control values. Legacy two-argument runs leave these
! defaults untouched and continue to read configuration from the Network file.
!
    implicit none
    logical :: control_file_mode = .FALSE.
    character(len=200) :: control_network_file = ""
    character(len=200) :: control_flow_file = ""
    character(len=200) :: control_heat_file = ""
    character(len=200) :: control_reservoir_file = ""
    character(len=200) :: control_reservoir_storage_file = ""
    character(len=200) :: control_reservoir_sel_withdraw_file = ""
    character(len=200) :: control_output_prefix = ""
    character(len=8) :: control_start_date = ""
    character(len=8) :: control_end_date = ""
    logical :: control_source = .FALSE.
    logical :: control_reservoir = .FALSE.
    integer :: control_nres = 0
    logical :: control_select_withdraw = .FALSE.
    character(len=32) :: control_input_format = "ascii"
    character(len=32) :: control_output_format = ""
    logical :: control_write_ascii = .TRUE.
    logical :: control_write_netcdf = .FALSE.
    character(len=64) :: control_flow_var = "KWroutedRunoff"
    character(len=64) :: control_flow_id_var = "reachID"
    character(len=64) :: control_heat_id_var = "hruId"
    character(len=64) :: control_heat_tair_var = "Tair_degC"
    character(len=64) :: control_heat_vp_var = "vp_mb"
    character(len=64) :: control_heat_sw_var = "SW_mmKs"
    character(len=64) :: control_heat_lw_var = "LW_mmKs"
    character(len=64) :: control_heat_press_var = "P_mb"
    character(len=64) :: control_heat_wind_var = "U10"
!
end module Block_Control
