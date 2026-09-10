module Netcdf_Restart
    use netcdf
    use Block_Control
    use Block_Energy
    use Block_Hydro
    use Block_Network
    use Block_Reservoir
    use Netcdf_Input, only: GET_NEXT_FORCING_POSITION
    use Netcdf_Output, only: GET_NEXT_OUTPUT_POSITION
    use, intrinsic :: ieee_arithmetic
    implicit none

    integer, parameter :: restart_schema_version = 2

contains

    subroutine RESTART_CHECK(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in) :: context

        if (status .ne. nf90_noerr) then
            write(*,*) "NetCDF restart error: ", trim(context), ": ", &
                trim(nf90_strerror(status))
            stop 1
        end if
    end subroutine RESTART_CHECK

    subroutine RESTART_ERROR(message)
        character(len=*), intent(in) :: message

        write(*,*) "NetCDF restart error: ", trim(message)
        stop 1
    end subroutine RESTART_ERROR

    subroutine WRITE_RESTART_NETCDF(path, state_year, state_month, state_day, &
        calendar, active_index, T_head, T_smth, T_trib)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: calendar
        integer, intent(in) :: state_year, state_month, state_day, active_index
        real, intent(in) :: T_head(:), T_smth(:), T_trib(:)
        integer :: ncid, reach_dimid, cell_dimid, river_state_dimid
        integer :: river_segment_dimid, reservoir_dimid
        integer :: river_state_count, river_segment_count
        integer :: nr, ns, state_index, segment_index, inactive_index
        integer :: input_netcdf, flow_next_day, heat_next_day, local_next_day
        integer :: output_next_year, output_next_month, output_next_day
        logical :: local_flow_enabled
        character(len=32) :: flow_calendar, heat_calendar, local_calendar
        integer, allocatable :: topology_segment_cell(:)
        integer, allocatable :: initial_storage_int(:)
        integer, allocatable :: stratified_int(:), turnover_int(:)
        real, allocatable :: temperature_state(:), temperature_inactive(:)
        real, allocatable :: storage_state(:), storage_inactive(:)
        real, allocatable :: storage_temperature_state(:)
        real, allocatable :: storage_temperature_inactive(:)
        real, allocatable :: topology_x_dist(:)

        river_segment_count = sum(no_celm)
        river_state_count = river_segment_count + nreach
        inactive_index = 3 - active_index
        input_netcdf = merge(1,0,TRIM(control_input_format) .eq. "netcdf")
        flow_next_day = 0
        heat_next_day = 0
        local_next_day = 0
        local_flow_enabled = .false.
        flow_calendar = "standard"
        heat_calendar = "standard"
        local_calendar = "standard"
        call GET_NEXT_OUTPUT_POSITION(ndays,output_next_year,output_next_month, &
            output_next_day)
        if (input_netcdf .eq. 1) then
            call GET_NEXT_FORCING_POSITION(ndays,flow_next_day,heat_next_day, &
                local_flow_enabled,local_next_day,flow_calendar,heat_calendar, &
                local_calendar)
        end if
        allocate(temperature_state(river_state_count))
        allocate(temperature_inactive(river_state_count))
        allocate(topology_x_dist(river_state_count))
        allocate(storage_state(river_segment_count))
        allocate(storage_inactive(river_segment_count))
        allocate(storage_temperature_state(river_segment_count))
        allocate(storage_temperature_inactive(river_segment_count))
        allocate(topology_segment_cell(river_segment_count))

        state_index = 0
        segment_index = 0
        do nr = 1, nreach
            do ns = 0, no_celm(nr)
                state_index = state_index + 1
                temperature_state(state_index) = temp(nr,ns,active_index)
                temperature_inactive(state_index) = temp(nr,ns,inactive_index)
                topology_x_dist(state_index) = x_dist(nr,ns)
            end do
            do ns = 1, no_celm(nr)
                segment_index = segment_index + 1
                storage_state(segment_index) = sto(nr,ns,active_index)
                storage_inactive(segment_index) = sto(nr,ns,inactive_index)
                storage_temperature_state(segment_index) = &
                    temp_sto(nr,ns,active_index)
                storage_temperature_inactive(segment_index) = &
                    temp_sto(nr,ns,inactive_index)
                topology_segment_cell(segment_index) = segment_cell(nr,ns)
            end do
        end do

        call RESTART_CHECK(nf90_create(trim(path), nf90_clobber, ncid), &
            "create restart file")
        call RESTART_CHECK(nf90_put_att(ncid, nf90_global, "title", &
            "RBM model restart state"), "write title")
        call PUT_GLOBAL_INT(ncid, "restart_schema_version", restart_schema_version)
        call PUT_GLOBAL_INT(ncid, "state_year", state_year)
        call PUT_GLOBAL_INT(ncid, "state_month", state_month)
        call PUT_GLOBAL_INT(ncid, "state_day", state_day)
        call PUT_GLOBAL_INT(ncid, "nreach", nreach)
        call PUT_GLOBAL_INT(ncid, "heat_cells", heat_cells)
        call PUT_GLOBAL_INT(ncid, "nres", nres)
        call PUT_GLOBAL_INT(ncid, "nwpd", nwpd)
        call PUT_GLOBAL_INT(ncid, "reservoir_enabled", merge(1,0,reservoir))
        call PUT_GLOBAL_INT(ncid, "input_netcdf", input_netcdf)
        call PUT_GLOBAL_TEXT(ncid, "calendar", CANONICAL_CALENDAR(calendar))
        call PUT_GLOBAL_INT(ncid, "output_next_year", output_next_year)
        call PUT_GLOBAL_INT(ncid, "output_next_month", output_next_month)
        call PUT_GLOBAL_INT(ncid, "output_next_day", output_next_day)
        if (input_netcdf .eq. 1) then
            call PUT_GLOBAL_INT(ncid, "flow_next_day", flow_next_day)
            call PUT_GLOBAL_INT(ncid, "heat_next_day", heat_next_day)
            call PUT_GLOBAL_INT(ncid, "local_flow_enabled", &
                merge(1,0,local_flow_enabled))
            call PUT_GLOBAL_INT(ncid, "local_next_day", local_next_day)
            call PUT_GLOBAL_TEXT(ncid, "flow_calendar", flow_calendar)
            call PUT_GLOBAL_TEXT(ncid, "heat_calendar", heat_calendar)
            call PUT_GLOBAL_TEXT(ncid, "local_calendar", local_calendar)
        end if

        call RESTART_CHECK(nf90_def_dim(ncid, "reach", nreach, reach_dimid), &
            "define reach dimension")
        call RESTART_CHECK(nf90_def_dim(ncid, "cell", heat_cells, cell_dimid), &
            "define cell dimension")
        call RESTART_CHECK(nf90_def_dim(ncid, "river_state", &
            river_state_count, river_state_dimid), "define river_state dimension")
        call RESTART_CHECK(nf90_def_dim(ncid, "river_segment", &
            river_segment_count, river_segment_dimid), &
            "define river_segment dimension")

        call DEFINE_INT_VAR(ncid, "no_celm", reach_dimid)
        call DEFINE_INT_VAR(ncid, "hru_id", cell_dimid)
        call DEFINE_INT_VAR(ncid, "topology_segment_cell", river_segment_dimid)
        call DEFINE_REAL_VAR(ncid, "topology_x_dist", river_state_dimid)
        call DEFINE_REAL_VAR(ncid, "topology_ndelta", cell_dimid)
        call DEFINE_REAL_VAR(ncid, "topology_alphamu", reach_dimid)
        call DEFINE_REAL_VAR(ncid, "topology_beta", reach_dimid)
        call DEFINE_REAL_VAR(ncid, "topology_gmma", reach_dimid)
        call DEFINE_REAL_VAR(ncid, "topology_mu", reach_dimid)
        call DEFINE_REAL_VAR(ncid, "topology_smooth_param", reach_dimid)
        call DEFINE_REAL_VAR(ncid, "river_temperature", river_state_dimid)
        call DEFINE_REAL_VAR(ncid, "river_temperature_inactive", river_state_dimid)
        call DEFINE_REAL_VAR(ncid, "river_storage", river_segment_dimid)
        call DEFINE_REAL_VAR(ncid, "river_storage_inactive", river_segment_dimid)
        call DEFINE_REAL_VAR(ncid, "river_storage_temperature", &
            river_segment_dimid)
        call DEFINE_REAL_VAR(ncid, "river_storage_temperature_inactive", &
            river_segment_dimid)
        call DEFINE_REAL_VAR(ncid, "headwater_temperature", reach_dimid)
        call DEFINE_REAL_VAR(ncid, "smoothed_air_temperature", reach_dimid)
        call DEFINE_REAL_VAR(ncid, "tributary_temperature", reach_dimid)

        if (nres .gt. 0) then
            call RESTART_CHECK(nf90_def_dim(ncid, "reservoir", nres, &
                reservoir_dimid), "define reservoir dimension")
            call DEFINE_INT_VAR(ncid, "reservoir_id", reservoir_dimid)
            call DEFINE_INT_VAR(ncid, "reservoir_start_node", reservoir_dimid)
            call DEFINE_INT_VAR(ncid, "reservoir_end_node", reservoir_dimid)
            call DEFINE_REAL_VAR(ncid, "reservoir_surface_area", reservoir_dimid)
            call DEFINE_REAL_VAR(ncid, "reservoir_capacity", reservoir_dimid)
            call DEFINE_INT_VAR(ncid, "initial_storage", reservoir_dimid)
            call DEFINE_INT_VAR(ncid, "stratification_started", reservoir_dimid)
            call DEFINE_INT_VAR(ncid, "turnover_reached", reservoir_dimid)
            call DEFINE_REAL_VAR(ncid, "epilimnion_temperature", reservoir_dimid)
            call DEFINE_REAL_VAR(ncid, "hypolimnion_temperature", reservoir_dimid)
            call DEFINE_REAL_VAR(ncid, "epilimnion_depth", reservoir_dimid)
            call DEFINE_REAL_VAR(ncid, "hypolimnion_depth", reservoir_dimid)
            call DEFINE_REAL_VAR(ncid, "epilimnion_volume", reservoir_dimid)
            call DEFINE_REAL_VAR(ncid, "hypolimnion_volume", reservoir_dimid)
            call DEFINE_REAL_VAR(ncid, "previous_storage", reservoir_dimid)
        end if

        call RESTART_CHECK(nf90_enddef(ncid), "end restart define mode")
        call PUT_INT_VAR(ncid, "no_celm", no_celm)
        call PUT_INT_VAR(ncid, "hru_id", hru_id)
        call PUT_INT_VAR(ncid, "topology_segment_cell", topology_segment_cell)
        call PUT_REAL_VAR(ncid, "topology_x_dist", topology_x_dist)
        call PUT_REAL_VAR(ncid, "topology_ndelta", ndelta)
        call PUT_REAL_VAR(ncid, "topology_alphamu", alphaMu)
        call PUT_REAL_VAR(ncid, "topology_beta", beta)
        call PUT_REAL_VAR(ncid, "topology_gmma", gmma)
        call PUT_REAL_VAR(ncid, "topology_mu", mu)
        call PUT_REAL_VAR(ncid, "topology_smooth_param", smooth_param)
        call PUT_REAL_VAR(ncid, "river_temperature", temperature_state)
        call PUT_REAL_VAR(ncid, "river_temperature_inactive", temperature_inactive)
        call PUT_REAL_VAR(ncid, "river_storage", storage_state)
        call PUT_REAL_VAR(ncid, "river_storage_inactive", storage_inactive)
        call PUT_REAL_VAR(ncid, "river_storage_temperature", &
            storage_temperature_state)
        call PUT_REAL_VAR(ncid, "river_storage_temperature_inactive", &
            storage_temperature_inactive)
        call PUT_REAL_VAR(ncid, "headwater_temperature", T_head)
        call PUT_REAL_VAR(ncid, "smoothed_air_temperature", T_smth)
        call PUT_REAL_VAR(ncid, "tributary_temperature", T_trib)

        if (nres .gt. 0) then
            allocate(initial_storage_int(nres), stratified_int(nres))
            allocate(turnover_int(nres))
            initial_storage_int = merge(1,0,initial_storage)
            stratified_int = merge(1,0,res_stratif_start)
            turnover_int = merge(1,0,res_turnover)
            call PUT_INT_VAR(ncid, "reservoir_id", dam_number)
            call PUT_INT_VAR(ncid, "reservoir_start_node", res_start_node)
            call PUT_INT_VAR(ncid, "reservoir_end_node", res_end_node)
            call PUT_REAL_VAR(ncid, "reservoir_surface_area", surface_area)
            call PUT_REAL_VAR(ncid, "reservoir_capacity", res_capacity_mcm)
            call PUT_INT_VAR(ncid, "initial_storage", initial_storage_int)
            call PUT_INT_VAR(ncid, "stratification_started", stratified_int)
            call PUT_INT_VAR(ncid, "turnover_reached", turnover_int)
            call PUT_REAL_VAR(ncid, "epilimnion_temperature", T_epil)
            call PUT_REAL_VAR(ncid, "hypolimnion_temperature", T_hypo)
            call PUT_REAL_VAR(ncid, "epilimnion_depth", depth_e)
            call PUT_REAL_VAR(ncid, "hypolimnion_depth", depth_h)
            call PUT_REAL_VAR(ncid, "epilimnion_volume", volume_e_x)
            call PUT_REAL_VAR(ncid, "hypolimnion_volume", volume_h_x)
            call PUT_REAL_VAR(ncid, "previous_storage", res_storage(:,1))
            deallocate(initial_storage_int, stratified_int, turnover_int)
        end if

        call RESTART_CHECK(nf90_close(ncid), "close restart file")
        deallocate(temperature_state, temperature_inactive, topology_x_dist)
        deallocate(storage_state, storage_inactive)
        deallocate(storage_temperature_state, storage_temperature_inactive)
        deallocate(topology_segment_cell)

        write(*,*) "Restart file written: ", trim(path)
        write(*,"(A,I4.4,2I2.2)") "  State date: ", state_year, state_month, state_day
    end subroutine WRITE_RESTART_NETCDF

    subroutine READ_RESTART_NETCDF(path, calendar, active_index, inactive_index, &
        T_head, T_smth, T_trib)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: calendar
        integer, intent(out) :: active_index, inactive_index
        real, intent(out) :: T_head(:), T_smth(:), T_trib(:)
        integer :: ncid, file_version, file_year, file_month, file_day
        integer :: file_nreach, file_heat_cells, file_nres, file_nwpd
        integer :: file_reservoir, file_input_netcdf
        integer :: river_state_count, river_segment_count
        integer :: nr, ns, state_index, segment_index
        character(len=32) :: file_calendar
        integer, allocatable :: file_no_celm(:), file_hru_id(:)
        integer, allocatable :: file_segment_cell(:)
        integer, allocatable :: initial_storage_int(:)
        integer, allocatable :: stratified_int(:), turnover_int(:)
        integer, allocatable :: file_reservoir_id(:)
        integer, allocatable :: file_reservoir_start(:)
        integer, allocatable :: file_reservoir_end(:)
        real, allocatable :: temperature_state(:), temperature_inactive(:)
        real, allocatable :: storage_state(:), storage_inactive(:)
        real, allocatable :: storage_temperature_state(:)
        real, allocatable :: storage_temperature_inactive(:)
        real, allocatable :: file_x_dist(:), file_values(:)

        call RESTART_CHECK(nf90_open(trim(path), nf90_nowrite, ncid), &
            "open restart file")
        call GET_GLOBAL_INT(ncid, "restart_schema_version", file_version)
        if (file_version .ne. restart_schema_version) &
            call RESTART_ERROR("unsupported restart_schema_version")
        call GET_GLOBAL_INT(ncid, "state_year", file_year)
        call GET_GLOBAL_INT(ncid, "state_month", file_month)
        call GET_GLOBAL_INT(ncid, "state_day", file_day)
        call GET_GLOBAL_INT(ncid, "nreach", file_nreach)
        call GET_GLOBAL_INT(ncid, "heat_cells", file_heat_cells)
        call GET_GLOBAL_INT(ncid, "nres", file_nres)
        call GET_GLOBAL_INT(ncid, "nwpd", file_nwpd)
        call GET_GLOBAL_INT(ncid, "reservoir_enabled", file_reservoir)
        call GET_GLOBAL_INT(ncid, "input_netcdf", file_input_netcdf)
        call GET_GLOBAL_TEXT(ncid, "calendar", file_calendar)

        if (file_nreach .ne. nreach) call RESTART_ERROR("nreach does not match topology")
        if (file_heat_cells .ne. heat_cells) &
            call RESTART_ERROR("heat_cells does not match topology")
        if (file_nres .ne. nres) call RESTART_ERROR("nres does not match control file")
        if (file_nwpd .ne. nwpd) call RESTART_ERROR("nwpd does not match model configuration")
        if (file_reservoir .ne. merge(1,0,reservoir)) &
            call RESTART_ERROR("reservoir setting does not match control file")
        if (file_input_netcdf .ne. &
            merge(1,0,TRIM(control_input_format) .eq. "netcdf")) &
            call RESTART_ERROR("input_format does not match restart file")
        if (CALENDAR_IS_NOLEAP(file_calendar) .neqv. CALENDAR_IS_NOLEAP(calendar)) &
            call RESTART_ERROR("calendar does not match the current forcing")

        if (.not. DATES_ARE_CONSECUTIVE(file_year,file_month,file_day, &
            start_year,start_month,start_day)) then
            write(*,"(A,I4.4,2I2.2,A,I4.4,2I2.2,A)") &
                "NetCDF restart error: state date ", file_year, file_month, &
                file_day, " must immediately precede start_date ", start_year, &
                start_month, start_day, "."
            stop 1
        end if

        river_segment_count = sum(no_celm)
        river_state_count = river_segment_count + nreach
        call CHECK_DIMENSION(ncid, "reach", nreach)
        call CHECK_DIMENSION(ncid, "cell", heat_cells)
        call CHECK_DIMENSION(ncid, "river_state", river_state_count)
        call CHECK_DIMENSION(ncid, "river_segment", river_segment_count)

        allocate(file_no_celm(nreach), file_hru_id(heat_cells))
        allocate(file_segment_cell(river_segment_count))
        allocate(file_x_dist(river_state_count))
        call GET_INT_VAR(ncid, "no_celm", file_no_celm)
        call GET_INT_VAR(ncid, "hru_id", file_hru_id)
        call GET_INT_VAR(ncid, "topology_segment_cell", file_segment_cell)
        call GET_REAL_VAR(ncid, "topology_x_dist", file_x_dist)
        if (any(file_no_celm .ne. no_celm)) &
            call RESTART_ERROR("reach segment counts do not match topology")
        if (any(file_hru_id .ne. hru_id)) &
            call RESTART_ERROR("hru_id values do not match topology")

        state_index = 0
        segment_index = 0
        do nr = 1, nreach
            do ns = 0, no_celm(nr)
                state_index = state_index + 1
                if (file_x_dist(state_index) .ne. x_dist(nr,ns)) &
                    call RESTART_ERROR("river distances do not match topology")
            end do
            do ns = 1, no_celm(nr)
                segment_index = segment_index + 1
                if (file_segment_cell(segment_index) .ne. segment_cell(nr,ns)) &
                    call RESTART_ERROR("river segment mapping does not match topology")
            end do
        end do
        deallocate(file_no_celm, file_hru_id, file_segment_cell, file_x_dist)

        allocate(file_values(heat_cells))
        call GET_REAL_VAR(ncid, "topology_ndelta", file_values)
        call CHECK_REAL_VALUES("ndelta", file_values, ndelta)
        deallocate(file_values)
        allocate(file_values(nreach))
        call GET_REAL_VAR(ncid, "topology_alphamu", file_values)
        call CHECK_REAL_VALUES("alphamu", file_values, alphaMu)
        call GET_REAL_VAR(ncid, "topology_beta", file_values)
        call CHECK_REAL_VALUES("beta", file_values, beta)
        call GET_REAL_VAR(ncid, "topology_gmma", file_values)
        call CHECK_REAL_VALUES("gmma", file_values, gmma)
        call GET_REAL_VAR(ncid, "topology_mu", file_values)
        call CHECK_REAL_VALUES("mu", file_values, mu)
        call GET_REAL_VAR(ncid, "topology_smooth_param", file_values)
        call CHECK_REAL_VALUES("smooth_param", file_values, smooth_param)
        deallocate(file_values)

        allocate(temperature_state(river_state_count))
        allocate(temperature_inactive(river_state_count))
        allocate(storage_state(river_segment_count))
        allocate(storage_inactive(river_segment_count))
        allocate(storage_temperature_state(river_segment_count))
        allocate(storage_temperature_inactive(river_segment_count))
        call GET_REAL_VAR(ncid, "river_temperature", temperature_state)
        call GET_REAL_VAR(ncid, "river_temperature_inactive", temperature_inactive)
        call GET_REAL_VAR(ncid, "river_storage", storage_state)
        call GET_REAL_VAR(ncid, "river_storage_inactive", storage_inactive)
        call GET_REAL_VAR(ncid, "river_storage_temperature", &
            storage_temperature_state)
        call GET_REAL_VAR(ncid, "river_storage_temperature_inactive", &
            storage_temperature_inactive)
        call GET_REAL_VAR(ncid, "headwater_temperature", T_head)
        call GET_REAL_VAR(ncid, "smoothed_air_temperature", T_smth)
        call GET_REAL_VAR(ncid, "tributary_temperature", T_trib)

        active_index = 1
        inactive_index = 2
        state_index = 0
        segment_index = 0
        do nr = 1, nreach
            do ns = 0, no_celm(nr)
                state_index = state_index + 1
                temp(nr,ns,active_index) = temperature_state(state_index)
                temp(nr,ns,inactive_index) = temperature_inactive(state_index)
            end do
            do ns = 1, no_celm(nr)
                segment_index = segment_index + 1
                sto(nr,ns,active_index) = storage_state(segment_index)
                sto(nr,ns,inactive_index) = storage_inactive(segment_index)
                temp_sto(nr,ns,active_index) = &
                    storage_temperature_state(segment_index)
                temp_sto(nr,ns,inactive_index) = &
                    storage_temperature_inactive(segment_index)
            end do
        end do
        deallocate(temperature_state, temperature_inactive)
        deallocate(storage_state, storage_inactive)
        deallocate(storage_temperature_state, storage_temperature_inactive)

        if (nres .gt. 0) then
            call CHECK_DIMENSION(ncid, "reservoir", nres)
            allocate(file_reservoir_id(nres), file_reservoir_start(nres))
            allocate(file_reservoir_end(nres), file_values(nres))
            allocate(initial_storage_int(nres), stratified_int(nres))
            allocate(turnover_int(nres))
            call GET_INT_VAR(ncid, "reservoir_id", file_reservoir_id)
            call GET_INT_VAR(ncid, "reservoir_start_node", file_reservoir_start)
            call GET_INT_VAR(ncid, "reservoir_end_node", file_reservoir_end)
            if (any(file_reservoir_id .ne. dam_number)) &
                call RESTART_ERROR("reservoir IDs do not match reservoir data")
            if (any(file_reservoir_start .ne. res_start_node) .or. &
                any(file_reservoir_end .ne. res_end_node)) &
                call RESTART_ERROR("reservoir node assignments do not match")
            call GET_REAL_VAR(ncid, "reservoir_surface_area", file_values)
            call CHECK_REAL_VALUES("reservoir surface area", file_values, surface_area)
            call GET_REAL_VAR(ncid, "reservoir_capacity", file_values)
            call CHECK_REAL_VALUES("reservoir capacity", file_values, res_capacity_mcm)
            call GET_INT_VAR(ncid, "initial_storage", initial_storage_int)
            call GET_INT_VAR(ncid, "stratification_started", stratified_int)
            call GET_INT_VAR(ncid, "turnover_reached", turnover_int)
            call CHECK_LOGICAL_VALUES("initial_storage", initial_storage_int)
            call CHECK_LOGICAL_VALUES("stratification_started", stratified_int)
            call CHECK_LOGICAL_VALUES("turnover_reached", turnover_int)
            initial_storage = initial_storage_int .eq. 1
            res_stratif_start = stratified_int .eq. 1
            res_turnover = turnover_int .eq. 1
            call GET_REAL_VAR(ncid, "epilimnion_temperature", T_epil)
            call GET_REAL_VAR(ncid, "hypolimnion_temperature", T_hypo)
            call GET_REAL_VAR(ncid, "epilimnion_depth", depth_e)
            call GET_REAL_VAR(ncid, "hypolimnion_depth", depth_h)
            call GET_REAL_VAR(ncid, "epilimnion_volume", volume_e_x)
            call GET_REAL_VAR(ncid, "hypolimnion_volume", volume_h_x)
            call GET_REAL_VAR(ncid, "previous_storage", res_storage(:,1))
            res_storage(:,2) = res_storage(:,1)
            deallocate(file_reservoir_id, file_reservoir_start)
            deallocate(file_reservoir_end, file_values)
            deallocate(initial_storage_int, stratified_int, turnover_int)
        end if

        call RESTART_CHECK(nf90_close(ncid), "close restart file")
        write(*,*) "Restart file loaded: ", trim(path)
        write(*,"(A,I4.4,2I2.2)") "  State date: ", file_year, file_month, file_day
    end subroutine READ_RESTART_NETCDF

    subroutine PUT_GLOBAL_INT(ncid, name, value)
        integer, intent(in) :: ncid, value
        character(len=*), intent(in) :: name

        call RESTART_CHECK(nf90_put_att(ncid, nf90_global, trim(name), value), &
            "write "//trim(name))
    end subroutine PUT_GLOBAL_INT

    subroutine GET_GLOBAL_INT(ncid, name, value)
        integer, intent(in) :: ncid
        integer, intent(out) :: value
        character(len=*), intent(in) :: name

        call RESTART_CHECK(nf90_get_att(ncid, nf90_global, trim(name), value), &
            "read "//trim(name))
    end subroutine GET_GLOBAL_INT

    subroutine PUT_GLOBAL_TEXT(ncid, name, value)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name, value

        call RESTART_CHECK(nf90_put_att(ncid, nf90_global, trim(name), &
            trim(value)), "write "//trim(name))
    end subroutine PUT_GLOBAL_TEXT

    subroutine GET_GLOBAL_TEXT(ncid, name, value)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: value

        value = ""
        call RESTART_CHECK(nf90_get_att(ncid, nf90_global, trim(name), value), &
            "read "//trim(name))
    end subroutine GET_GLOBAL_TEXT

    character(len=32) function CANONICAL_CALENDAR(calendar)
        character(len=*), intent(in) :: calendar

        if (CALENDAR_IS_NOLEAP(calendar)) then
            CANONICAL_CALENDAR = "noleap"
        else
            CANONICAL_CALENDAR = "proleptic_gregorian"
        end if
    end function CANONICAL_CALENDAR

    logical function CALENDAR_IS_NOLEAP(calendar)
        character(len=*), intent(in) :: calendar

        CALENDAR_IS_NOLEAP = trim(calendar) .eq. "noleap" .or. &
            trim(calendar) .eq. "365_day"
    end function CALENDAR_IS_NOLEAP

    logical function DATES_ARE_CONSECUTIVE(previous_year,previous_month, &
        previous_day,next_year,next_month,next_day)
        integer, intent(in) :: previous_year, previous_month, previous_day
        integer, intent(in) :: next_year, next_month, next_day
        integer, external :: Julian

        DATES_ARE_CONSECUTIVE = Julian(next_year,next_month,next_day) - &
            Julian(previous_year,previous_month,previous_day) .eq. 1
    end function DATES_ARE_CONSECUTIVE

    subroutine DEFINE_INT_VAR(ncid, name, dimid)
        integer, intent(in) :: ncid, dimid
        character(len=*), intent(in) :: name
        integer :: varid

        call RESTART_CHECK(nf90_def_var(ncid, trim(name), nf90_int, &
            (/dimid/), varid), "define "//trim(name))
    end subroutine DEFINE_INT_VAR

    subroutine DEFINE_REAL_VAR(ncid, name, dimid)
        integer, intent(in) :: ncid, dimid
        character(len=*), intent(in) :: name
        integer :: varid

        call RESTART_CHECK(nf90_def_var(ncid, trim(name), nf90_float, &
            (/dimid/), varid), "define "//trim(name))
    end subroutine DEFINE_REAL_VAR

    subroutine PUT_INT_VAR(ncid, name, values)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(in) :: values(:)
        integer :: varid

        call RESTART_CHECK(nf90_inq_varid(ncid, trim(name), varid), &
            "find "//trim(name))
        call RESTART_CHECK(nf90_put_var(ncid, varid, values), &
            "write "//trim(name))
    end subroutine PUT_INT_VAR

    subroutine PUT_REAL_VAR(ncid, name, values)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        real, intent(in) :: values(:)
        integer :: varid

        call RESTART_CHECK(nf90_inq_varid(ncid, trim(name), varid), &
            "find "//trim(name))
        call RESTART_CHECK(nf90_put_var(ncid, varid, values), &
            "write "//trim(name))
    end subroutine PUT_REAL_VAR

    subroutine GET_INT_VAR(ncid, name, values)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: values(:)
        integer :: varid

        call RESTART_CHECK(nf90_inq_varid(ncid, trim(name), varid), &
            "find "//trim(name))
        call RESTART_CHECK(nf90_get_var(ncid, varid, values), &
            "read "//trim(name))
    end subroutine GET_INT_VAR

    subroutine GET_REAL_VAR(ncid, name, values)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        real, intent(out) :: values(:)
        integer :: varid

        call RESTART_CHECK(nf90_inq_varid(ncid, trim(name), varid), &
            "find "//trim(name))
        call RESTART_CHECK(nf90_get_var(ncid, varid, values), &
            "read "//trim(name))
        if (any(.not. ieee_is_finite(values))) &
            call RESTART_ERROR(trim(name)//" contains non-finite values")
    end subroutine GET_REAL_VAR

    subroutine CHECK_DIMENSION(ncid, name, expected_length)
        integer, intent(in) :: ncid, expected_length
        character(len=*), intent(in) :: name
        integer :: dimid, actual_length

        call RESTART_CHECK(nf90_inq_dimid(ncid, trim(name), dimid), &
            "find "//trim(name)//" dimension")
        call RESTART_CHECK(nf90_inquire_dimension(ncid, dimid, &
            len=actual_length), "read "//trim(name)//" dimension")
        if (actual_length .ne. expected_length) then
            write(*,*) "NetCDF restart error: ", trim(name), &
                " dimension is ", actual_length, "; expected ", expected_length, "."
            stop 1
        end if
    end subroutine CHECK_DIMENSION

    subroutine CHECK_REAL_VALUES(label, actual, expected)
        character(len=*), intent(in) :: label
        real, intent(in) :: actual(:), expected(:)

        if (size(actual) .ne. size(expected)) &
            call RESTART_ERROR(trim(label)//" size does not match")
        if (any(actual .ne. expected)) &
            call RESTART_ERROR(trim(label)//" values do not match input data")
    end subroutine CHECK_REAL_VALUES

    subroutine CHECK_LOGICAL_VALUES(label, values)
        character(len=*), intent(in) :: label
        integer, intent(in) :: values(:)

        if (any(values .ne. 0 .and. values .ne. 1)) &
            call RESTART_ERROR(trim(label)//" contains values other than 0 or 1")
    end subroutine CHECK_LOGICAL_VALUES

end module Netcdf_Restart
