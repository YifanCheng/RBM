module Netcdf_Input
    use netcdf
    use Block_Control
    use Block_Energy
    use Block_Hydro
    use Block_Network
    use Block_Reservoir
    implicit none

    integer :: flow_ncid = -1
    integer :: heat_ncid = -1
    integer :: flow_varid = -1
    integer :: flow_id_varid = -1
    integer :: heat_id_varid = -1
    integer :: tair_varid = -1
    integer :: vp_varid = -1
    integer :: sw_varid = -1
    integer :: lw_varid = -1
    integer :: press_varid = -1
    integer :: wind_varid = -1
    integer, allocatable :: flow_ids(:)
    integer(kind=8), allocatable :: heat_ids(:)
    logical :: nc_forcing_open = .false.

contains

    subroutine NC_INPUT_CHECK(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in) :: context

        if (status /= nf90_noerr) then
            write(*,*) 'NetCDF input error: ', trim(context), ': ', trim(nf90_strerror(status))
            stop
        end if
    end subroutine NC_INPUT_CHECK

    subroutine NC_INPUT_INIT()
        integer :: dimid, nflow, nheat

        if (nc_forcing_open) return

        call NC_INPUT_CHECK(nf90_open(trim(control_flow_file), nf90_nowrite, flow_ncid), 'open flow file')
        call NC_INPUT_CHECK(nf90_open(trim(control_heat_file), nf90_nowrite, heat_ncid), 'open heat file')
        call NC_INPUT_CHECK(nf90_inq_varid(flow_ncid, trim(control_flow_var), flow_varid), 'find flow variable')
        call NC_INPUT_CHECK(nf90_inq_varid(flow_ncid, trim(control_flow_id_var), flow_id_varid), 'find flow id variable')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_id_var), heat_id_varid), 'find heat id variable')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_tair_var), tair_varid), 'find air temperature')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_vp_var), vp_varid), 'find vapor pressure')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_sw_var), sw_varid), 'find shortwave')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_lw_var), lw_varid), 'find longwave')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_press_var), press_varid), 'find air pressure')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_wind_var), wind_varid), 'find wind')

        call NC_INPUT_CHECK(nf90_inq_dimid(flow_ncid, 'seg', dimid), 'find flow seg dimension')
        call NC_INPUT_CHECK(nf90_inquire_dimension(flow_ncid, dimid, len=nflow), 'read flow seg length')
        call NC_INPUT_CHECK(nf90_inq_dimid(heat_ncid, 'hru', dimid), 'find heat hru dimension')
        call NC_INPUT_CHECK(nf90_inquire_dimension(heat_ncid, dimid, len=nheat), 'read heat hru length')

        allocate(flow_ids(nflow))
        allocate(heat_ids(nheat))
        call NC_INPUT_CHECK(nf90_get_var(flow_ncid, flow_id_varid, flow_ids), 'read flow ids')
        call NC_INPUT_CHECK(nf90_get_var(heat_ncid, heat_id_varid, heat_ids), 'read heat ids')
        nc_forcing_open = .true.
    end subroutine NC_INPUT_INIT

    subroutine READ_FORCING_NETCDF()
        integer :: nc, ncell, no_flow, no_heat, nr
        integer :: nreservoir, n1, n2, flow_idx, heat_idx
        real :: q_cfs, q_unused
        real :: z_temp, w_temp
        real :: min_flow = 5.0
        real :: min_depth = 5.0

        call NC_INPUT_INIT()

        n1 = 1
        n2 = 2
        no_flow = 0
        no_heat = 0
        do nr = 1, nreach
            do nc = 1, no_cells(nr)-1
                no_flow = no_flow + 1
                no_heat = no_heat + 1
                ncell = no_heat
                call READ_FLOW_CELL(ndays, ncell, q_cfs, q_unused, Q_diff(no_heat), depth(no_heat), &
                    width(no_heat), u(no_heat), Q_local(no_heat))
                Q_out(no_heat) = q_cfs
                call APPLY_FLOW_LIMITS(no_heat, min_flow, min_depth)
                if (Q_diff(no_heat) .gt. 0) write(*,*) 'Q_diff is not equal to 0', no_heat, Q_diff(no_heat)
                if (u(no_heat) .lt. 0.01) u(no_heat) = 0.01
                call READ_HEAT_CELL(ndays, no_heat)
                if (wind(no_heat) .lt. 1.0) wind(no_heat) = 1.0
                delta_n = ndelta(ncell)
                dt(no_heat) = dx(no_heat)/u(no_heat)
                if (dt(no_heat) .gt. dt_comp) write(45,*) &
                    'Travel time=',dt(no_heat), '> dt_comp at node -',no_heat
            end do

            no_heat = no_heat + 1
            call READ_HEAT_CELL(ndays, no_heat)
            if (wind(no_heat) .lt. 1.0) wind(no_heat) = 1.0
            if (nr .eq. nreach .and. nc .eq. no_cells(nr)) then
                ncell = no_heat
                call READ_FLOW_CELL(ndays, ncell, q_cfs, q_unused, Q_diff(no_heat), depth(no_heat), &
                    width(no_heat), u(no_heat), Q_local(no_heat))
                Q_out(no_heat) = q_cfs
                call APPLY_FLOW_LIMITS(no_heat, min_flow, min_depth)
                Q_trib(nr) = 0
                Q_in(no_heat) = Q_out(no_heat-1)
                dt(no_heat) = dx(ncell)/u(no_heat)
            else
                Q_out(no_heat) = Q_out(no_heat-1)
                Q_trib(nr) = Q_out(no_heat)
                Q_in(no_heat) = Q_out(no_heat-1)
                u(no_heat) = u(no_heat-1)
                depth(no_heat) = depth(no_heat-1)
                width(no_heat) = width(no_heat-1)
                dt(no_heat) = dx(no_heat)/u(no_heat)
            end if
        end do

        res_storage(:,n2) = res_storage(:,n1)
        read(38,*)
        do nreservoir = 1, nres
            read(38,*) res_storage(nreservoir, n1)
        end do

        read(39,*)
        do nreservoir = 1, nres
            read(39,*) eh_withdraw_ratio(nreservoir, n1), eh_withdraw_ratio(nreservoir, n2)
        end do

        call Water_Balance
    end subroutine READ_FORCING_NETCDF

    subroutine READ_FLOW_CELL(day_index, cell_index, q_out, q_in_dummy, q_diff_out, depth_out, width_out, &
        velocity_out, q_local_out)
        integer, intent(in) :: day_index, cell_index
        real, intent(out) :: q_out, q_in_dummy, q_diff_out, depth_out, width_out, velocity_out, q_local_out
        integer :: flow_idx
        character(len=120) :: context
        real :: q_cms, q_for_hydraulics, depth_calc, width_calc, velocity_calc
        real :: q_in(1)
        real, parameter :: cms_to_cfs = 1.0/0.028316847
        real, parameter :: min_flow = 5.0
        real, parameter :: min_velocity = 0.5

        flow_idx = FIND_FLOW_INDEX(hru_id(cell_index))
        write(context,'(A,I0,A,I0,A,I0)') 'read flow value day=',day_index,' cell=',cell_index,' flow_idx=',flow_idx
        call NC_INPUT_CHECK(nf90_get_var(flow_ncid, flow_varid, q_in, start=(/flow_idx, day_index/), &
            count=(/1, 1/)), trim(context))
        q_cms = q_in(1)
        q_for_hydraulics = q_cms * cms_to_cfs
        if (q_for_hydraulics .lt. min_flow) q_for_hydraulics = min_flow
        depth_calc = a_z * (q_for_hydraulics**b_z)
        width_calc = a_w * (q_for_hydraulics**b_w)
        velocity_calc = q_for_hydraulics/(depth_calc*width_calc)
        if (velocity_calc .lt. min_velocity) velocity_calc = min_velocity

        q_out = ROUND1(q_for_hydraulics)
        q_in_dummy = q_out
        q_diff_out = 0.0
        depth_out = ROUND1(depth_calc)
        width_out = ROUND1(width_calc)
        velocity_out = ROUND2(velocity_calc)
        q_local_out = 0.0
    end subroutine READ_FLOW_CELL

    subroutine READ_HEAT_CELL(day_index, cell_index)
        integer, intent(in) :: day_index, cell_index
        integer :: heat_idx
        character(len=120) :: context

        heat_idx = FIND_HEAT_INDEX(hru_id(cell_index))
        write(context,'(A,I0,A,I0,A,I0)') 'read heat value day=',day_index,' cell=',cell_index,' heat_idx=',heat_idx
        call READ_NC_REAL(heat_ncid, tair_varid, day_index, heat_idx, dbt(cell_index), 1, trim(context))
        call READ_NC_REAL(heat_ncid, vp_varid, day_index, heat_idx, ea(cell_index), 1, trim(context))
        call READ_NC_REAL(heat_ncid, sw_varid, day_index, heat_idx, Q_ns(cell_index), 4, trim(context))
        call READ_NC_REAL(heat_ncid, lw_varid, day_index, heat_idx, Q_na(cell_index), 4, trim(context))
        rho = 1.225
        call READ_NC_REAL(heat_ncid, press_varid, day_index, heat_idx, press(cell_index), 1, trim(context))
        call READ_NC_REAL(heat_ncid, wind_varid, day_index, heat_idx, wind(cell_index), 1, trim(context))
    end subroutine READ_HEAT_CELL

    subroutine READ_NC_REAL(ncid, varid, day_index, id_index, value, round_digits, context)
        integer, intent(in) :: ncid, varid, day_index, id_index, round_digits
        character(len=*), intent(in) :: context
        real, intent(out) :: value
        real :: raw_value
        real :: raw_values(1)

        call NC_INPUT_CHECK(nf90_get_var(ncid, varid, raw_values, start=(/id_index, day_index/), &
            count=(/1, 1/)), context)
        raw_value = raw_values(1)
        if (round_digits .eq. 1) then
            value = ROUND1(raw_value)
        else if (round_digits .eq. 2) then
            value = ROUND2(raw_value)
        else if (round_digits .eq. 4) then
            value = ROUND4(raw_value)
        else
            value = raw_value
        end if
    end subroutine READ_NC_REAL

    subroutine APPLY_FLOW_LIMITS(cell_index, min_flow, min_depth)
        integer, intent(in) :: cell_index
        real, intent(in) :: min_flow, min_depth
        real :: z_temp, w_temp

        if (Q_out(cell_index) .lt. min_flow) then
            z_temp = a_z * (min_flow**b_z)
            w_temp = a_w * (min_flow**b_w)
            u(cell_index) = min_flow/(z_temp*w_temp)
            depth(cell_index) = z_temp
            width(cell_index) = w_temp
            Q_out(cell_index) = min_flow
        end if
        if (depth(cell_index) .lt. min_depth) then
            depth(cell_index) = min_depth
            width(cell_index) = Q_out(cell_index)/(u(cell_index)*depth(cell_index))
        end if
    end subroutine APPLY_FLOW_LIMITS

    integer function FIND_FLOW_INDEX(id)
        integer, intent(in) :: id
        integer :: i

        do i = 1, size(flow_ids)
            if (flow_ids(i) .eq. id) then
                FIND_FLOW_INDEX = i
                return
            end if
        end do
        write(*,*) 'Flow reachID not found in netCDF input: ',id
        stop
    end function FIND_FLOW_INDEX

    integer function FIND_HEAT_INDEX(id)
        integer, intent(in) :: id
        integer :: i

        do i = 1, size(heat_ids)
            if (heat_ids(i) .eq. int(id, kind=8)) then
                FIND_HEAT_INDEX = i
                return
            end if
        end do
        write(*,*) 'Heat hruId not found in netCDF input: ',id
        stop
    end function FIND_HEAT_INDEX

    real function ROUND1(value)
        real, intent(in) :: value
        ROUND1 = real(nint(value*10.0))/10.0
    end function ROUND1

    real function ROUND2(value)
        real, intent(in) :: value
        ROUND2 = real(nint(value*100.0))/100.0
    end function ROUND2

    real function ROUND4(value)
        real, intent(in) :: value
        ROUND4 = real(nint(value*10000.0))/10000.0
    end function ROUND4

end module Netcdf_Input
