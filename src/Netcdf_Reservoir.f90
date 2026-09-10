module Netcdf_Reservoir
    use netcdf
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use Block_Control
    use Block_Network, only: heat_cells
    use Block_Reservoir
    implicit none

    integer :: reservoir_ncid = -1
    integer :: storage_varid = -1
    integer :: withdraw_epi_varid = -1
    integer :: withdraw_hyp_varid = -1
    integer :: reservoir_start_index = 1
    integer, allocatable :: reservoir_time_days(:)
    integer :: reservoir_origin_year = 0
    integer :: reservoir_origin_month = 0
    integer :: reservoir_origin_day = 0
    logical :: reservoir_nc_open = .false.

contains

    subroutine RESERVOIR_NC_CHECK(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in) :: context

        if (status .ne. nf90_noerr) then
            write(*,*) "NetCDF reservoir error: ", trim(context), ": ", &
                trim(nf90_strerror(status))
            stop 1
        end if
    end subroutine RESERVOIR_NC_CHECK

    subroutine READ_RESERVOIR_NC_PARAMETERS()
        integer :: reservoir_dimid, time_dimid
        integer :: file_nres, ntime
        integer :: sy, sm, sd, ey, em, ed, requested_days
        integer :: i
        integer, allocatable :: file_ids(:)
        integer, external :: Julian

        if (reservoir_nc_open) return
        if (len_trim(control_reservoir_data_file) .eq. 0) then
            write(*,*) "NetCDF reservoir error: reservoir_data_file is empty."
            stop 1
        end if

        call RESERVOIR_NC_CHECK(nf90_open(trim(control_reservoir_data_file), &
            nf90_nowrite, reservoir_ncid), "open reservoir data file")
        call RESERVOIR_NC_CHECK(nf90_inq_dimid(reservoir_ncid, "reservoir", &
            reservoir_dimid), "find reservoir dimension")
        call RESERVOIR_NC_CHECK(nf90_inquire_dimension(reservoir_ncid, &
            reservoir_dimid, len=file_nres), "read reservoir dimension")
        if (file_nres .ne. nres) then
            write(*,*) "NetCDF reservoir error: file contains ", file_nres, &
                " reservoirs but <nres> is ", nres, "."
            stop 1
        end if

        allocate(file_ids(nres))
        call READ_INT_VECTOR("reservoir_id", file_ids, reservoir_dimid)
        do i = 1, nres
            if (file_ids(i) .ne. i) then
                write(*,*) "NetCDF reservoir error: reservoir_id must be ordered 1..nres; ", &
                    "position ", i, " contains ", file_ids(i), "."
                stop 1
            end if
        end do
        dam_number = file_ids
        deallocate(file_ids)

        call READ_REAL_VECTOR("grid_lat", dam_grid_lat, reservoir_dimid)
        call READ_REAL_VECTOR("grid_lon", dam_grid_lon, reservoir_dimid)
        call READ_REAL_VECTOR("depth_m", res_depth_meter, reservoir_dimid)
        call READ_REAL_VECTOR("width_m", res_width_meter, reservoir_dimid)
        call READ_REAL_VECTOR("area_km2", res_area_km2, reservoir_dimid)
        call READ_REAL_VECTOR("capacity_mcm", res_capacity_mcm, reservoir_dimid)
        call READ_INT_VECTOR("start_node", res_start_node, reservoir_dimid)
        call READ_INT_VECTOR("end_node", res_end_node, reservoir_dimid)
        call VALIDATE_STATIC_PARAMETERS()

        call INIT_RESERVOIR_TIME(time_dimid, ntime)
        call FIND_DAILY_VARIABLE("storage", reservoir_dimid, time_dimid, &
            storage_varid)
        call FIND_DAILY_VARIABLE("withdraw_epilimnion", reservoir_dimid, &
            time_dimid, withdraw_epi_varid)
        call FIND_DAILY_VARIABLE("withdraw_hypolimnion", reservoir_dimid, &
            time_dimid, withdraw_hyp_varid)

        read(control_start_date, "(i4,2i2)") sy, sm, sd
        read(control_end_date, "(i4,2i2)") ey, em, ed
        requested_days = Julian(ey, em, ed) - Julian(sy, sm, sd) + 1
        reservoir_start_index = FIND_RESERVOIR_TIME(sy, sm, sd)
        if (reservoir_start_index + requested_days - 1 .gt. ntime) then
            write(*,*) "NetCDF reservoir error: reservoir data has ", ntime, &
                " daily records but does not cover the requested end date."
            stop 1
        end if

        write(*,*) "NetCDF reservoir input mode"
        write(*,*) "  Reservoir file   : ", trim(control_reservoir_data_file)
        write(*,*) "  Reservoirs       : ", nres
        write(*,*) "  Daily records    : ", ntime
        write(*,*) "  Start index      : ", reservoir_start_index
        reservoir_nc_open = .true.
    end subroutine READ_RESERVOIR_NC_PARAMETERS

    subroutine READ_INT_VECTOR(name, values, expected_dimid)
        character(len=*), intent(in) :: name
        integer, intent(out) :: values(:)
        integer, intent(in) :: expected_dimid
        integer :: varid, ndims, dimids(nf90_max_var_dims)

        call RESERVOIR_NC_CHECK(nf90_inq_varid(reservoir_ncid, trim(name), &
            varid), "find "//trim(name))
        call RESERVOIR_NC_CHECK(nf90_inquire_variable(reservoir_ncid, varid, &
            ndims=ndims, dimids=dimids), "inquire "//trim(name))
        if (ndims .ne. 1 .or. dimids(1) .ne. expected_dimid) then
            write(*,*) "NetCDF reservoir error: ", trim(name), &
                " must use dimension (reservoir)."
            stop 1
        end if
        call RESERVOIR_NC_CHECK(nf90_get_var(reservoir_ncid, varid, values), &
            "read "//trim(name))
    end subroutine READ_INT_VECTOR

    subroutine READ_REAL_VECTOR(name, values, expected_dimid)
        character(len=*), intent(in) :: name
        real, intent(out) :: values(:)
        integer, intent(in) :: expected_dimid
        integer :: varid, ndims, dimids(nf90_max_var_dims)

        call RESERVOIR_NC_CHECK(nf90_inq_varid(reservoir_ncid, trim(name), &
            varid), "find "//trim(name))
        call RESERVOIR_NC_CHECK(nf90_inquire_variable(reservoir_ncid, varid, &
            ndims=ndims, dimids=dimids), "inquire "//trim(name))
        if (ndims .ne. 1 .or. dimids(1) .ne. expected_dimid) then
            write(*,*) "NetCDF reservoir error: ", trim(name), &
                " must use dimension (reservoir)."
            stop 1
        end if
        call RESERVOIR_NC_CHECK(nf90_get_var(reservoir_ncid, varid, values), &
            "read "//trim(name))
    end subroutine READ_REAL_VECTOR

    subroutine VALIDATE_STATIC_PARAMETERS()
        integer :: i

        do i = 1, nres
            if (.not. ieee_is_finite(dam_grid_lat(i)) .or. &
                .not. ieee_is_finite(dam_grid_lon(i)) .or. &
                .not. ieee_is_finite(res_depth_meter(i)) .or. &
                .not. ieee_is_finite(res_width_meter(i)) .or. &
                .not. ieee_is_finite(res_area_km2(i)) .or. &
                .not. ieee_is_finite(res_capacity_mcm(i))) then
                write(*,*) "NetCDF reservoir error: non-finite static value for reservoir ", i, "."
                stop 1
            end if
            if (res_depth_meter(i) .le. 0.0 .or. res_width_meter(i) .le. 0.0 .or. &
                res_area_km2(i) .le. 0.0 .or. res_capacity_mcm(i) .le. 0.0) then
                write(*,*) "NetCDF reservoir error: depth, width, area, and capacity ", &
                    "must be positive for reservoir ", i, "."
                stop 1
            end if
            if (res_start_node(i) .lt. 1 .or. res_start_node(i) .gt. heat_cells .or. &
                res_end_node(i) .lt. res_start_node(i) .or. &
                res_end_node(i) .gt. heat_cells) then
                write(*,*) "NetCDF reservoir error: invalid start/end nodes for reservoir ", i, "."
                stop 1
            end if
        end do
    end subroutine VALIDATE_STATIC_PARAMETERS

    subroutine INIT_RESERVOIR_TIME(time_dimid, ntime)
        integer, intent(out) :: time_dimid, ntime
        integer :: time_varid, ndims, dimids(nf90_max_var_dims)
        integer :: i, ios, since_pos, status
        real(8), allocatable :: time_values(:)
        character(len=256) :: units, units_lower, date_text
        character(len=32) :: calendar

        call RESERVOIR_NC_CHECK(nf90_inq_dimid(reservoir_ncid, "time", &
            time_dimid), "find time dimension")
        call RESERVOIR_NC_CHECK(nf90_inquire_dimension(reservoir_ncid, &
            time_dimid, len=ntime), "read time dimension")
        call RESERVOIR_NC_CHECK(nf90_inq_varid(reservoir_ncid, "time", &
            time_varid), "find time coordinate")
        call RESERVOIR_NC_CHECK(nf90_inquire_variable(reservoir_ncid, time_varid, &
            ndims=ndims, dimids=dimids), "inquire time coordinate")
        if (ndims .ne. 1 .or. dimids(1) .ne. time_dimid) then
            write(*,*) "NetCDF reservoir error: time must use dimension (time)."
            stop 1
        end if

        allocate(time_values(ntime), reservoir_time_days(ntime))
        call RESERVOIR_NC_CHECK(nf90_get_var(reservoir_ncid, time_varid, &
            time_values), "read time coordinate")
        units = ""
        call RESERVOIR_NC_CHECK(nf90_get_att(reservoir_ncid, time_varid, &
            "units", units), "read time units")
        units_lower = units
        call LOWERCASE_RESERVOIR(units_lower)
        since_pos = index(units_lower, "since")
        if (index(adjustl(units_lower), "days since") .ne. 1 .or. &
            since_pos .eq. 0) then
            write(*,*) "NetCDF reservoir error: time units must use ", &
                "'days since YYYY-MM-DD'."
            stop 1
        end if
        date_text = adjustl(units(since_pos+5:))
        read(date_text, "(i4,1x,i2,1x,i2)", iostat=ios) &
            reservoir_origin_year, reservoir_origin_month, reservoir_origin_day
        if (ios .ne. 0) then
            write(*,*) "NetCDF reservoir error: cannot parse time origin from '", &
                trim(units), "'."
            stop 1
        end if

        calendar = "standard"
        status = nf90_get_att(reservoir_ncid, time_varid, "calendar", calendar)
        if (status .ne. nf90_noerr .and. status .ne. nf90_enotatt) &
            call RESERVOIR_NC_CHECK(status, "read time calendar")
        call LOWERCASE_RESERVOIR(calendar)
        if (trim(calendar) .ne. "standard" .and. trim(calendar) .ne. "gregorian" .and. &
            trim(calendar) .ne. "proleptic_gregorian") then
            write(*,*) "NetCDF reservoir error: calendar must be Gregorian; found '", &
                trim(calendar), "'."
            stop 1
        end if

        do i = 1, ntime
            if (.not. ieee_is_finite(time_values(i)) .or. &
                abs(time_values(i) - real(nint(time_values(i)), 8)) .gt. 1.0d-6) then
                write(*,*) "NetCDF reservoir error: time must contain integer daily offsets."
                stop 1
            end if
            reservoir_time_days(i) = nint(time_values(i))
            if (i .gt. 1) then
                if (reservoir_time_days(i) .ne. reservoir_time_days(i-1) + 1) then
                    write(*,*) "NetCDF reservoir error: time must contain consecutive daily records."
                    stop 1
                end if
            end if
        end do
        deallocate(time_values)
    end subroutine INIT_RESERVOIR_TIME

    subroutine FIND_DAILY_VARIABLE(name, reservoir_dimid, time_dimid, varid)
        character(len=*), intent(in) :: name
        integer, intent(in) :: reservoir_dimid, time_dimid
        integer, intent(out) :: varid
        integer :: ndims, dimids(nf90_max_var_dims)

        call RESERVOIR_NC_CHECK(nf90_inq_varid(reservoir_ncid, trim(name), &
            varid), "find "//trim(name))
        call RESERVOIR_NC_CHECK(nf90_inquire_variable(reservoir_ncid, varid, &
            ndims=ndims, dimids=dimids), "inquire "//trim(name))
        if (ndims .ne. 2 .or. dimids(1) .ne. reservoir_dimid .or. &
            dimids(2) .ne. time_dimid) then
            write(*,*) "NetCDF reservoir error: ", trim(name), &
                " must use dimensions (time,reservoir)."
            stop 1
        end if
    end subroutine FIND_DAILY_VARIABLE

    integer function FIND_RESERVOIR_TIME(year, month, day)
        integer, intent(in) :: year, month, day
        integer :: target, low, high, middle
        integer, external :: Julian

        target = Julian(year, month, day) - Julian(reservoir_origin_year, &
            reservoir_origin_month, reservoir_origin_day)
        low = 1
        high = size(reservoir_time_days)
        do while (low .le. high)
            middle = (low + high)/2
            if (reservoir_time_days(middle) .eq. target) then
                FIND_RESERVOIR_TIME = middle
                return
            else if (reservoir_time_days(middle) .lt. target) then
                low = middle + 1
            else
                high = middle - 1
            end if
        end do
        write(*,"(A,I4.4,2I2.2,A)") "NetCDF reservoir error: no daily record for ", &
            year, month, day, "."
        stop 1
    end function FIND_RESERVOIR_TIME

    subroutine READ_RESERVOIR_NC_DAY(run_day)
        integer, intent(in) :: run_day
        integer :: file_day, i

        if (.not. reservoir_nc_open) then
            write(*,*) "NetCDF reservoir error: reservoir file was not initialized."
            stop 1
        end if
        file_day = reservoir_start_index + run_day - 1
        if (file_day .lt. 1 .or. file_day .gt. size(reservoir_time_days)) then
            write(*,*) "NetCDF reservoir error: requested reservoir day is out of range."
            stop 1
        end if

        res_storage(:,2) = res_storage(:,1)
        call RESERVOIR_NC_CHECK(nf90_get_var(reservoir_ncid, storage_varid, &
            res_storage(:,1), start=(/1,file_day/), count=(/nres,1/)), &
            "read daily storage")
        call RESERVOIR_NC_CHECK(nf90_get_var(reservoir_ncid, withdraw_epi_varid, &
            eh_withdraw_ratio(:,1), start=(/1,file_day/), count=(/nres,1/)), &
            "read daily epilimnion withdrawal")
        call RESERVOIR_NC_CHECK(nf90_get_var(reservoir_ncid, withdraw_hyp_varid, &
            eh_withdraw_ratio(:,2), start=(/1,file_day/), count=(/nres,1/)), &
            "read daily hypolimnion withdrawal")

        do i = 1, nres
            if (.not. ieee_is_finite(res_storage(i,1)) .or. &
                res_storage(i,1) .lt. 0.0) then
                write(*,*) "NetCDF reservoir error: invalid storage for reservoir ", &
                    i, " on run day ", run_day, "."
                stop 1
            end if
            if (.not. ieee_is_finite(eh_withdraw_ratio(i,1)) .or. &
                .not. ieee_is_finite(eh_withdraw_ratio(i,2)) .or. &
                eh_withdraw_ratio(i,1) .lt. 0.0 .or. &
                eh_withdraw_ratio(i,2) .lt. 0.0 .or. &
                abs(sum(eh_withdraw_ratio(i,:)) - 1.0) .gt. 1.0e-4) then
                write(*,*) "NetCDF reservoir error: withdrawal fractions must be ", &
                    "finite, nonnegative, and sum to 1 for reservoir ", i, "."
                stop 1
            end if
        end do
    end subroutine READ_RESERVOIR_NC_DAY

    subroutine CLOSE_RESERVOIR_NC()
        if (reservoir_ncid .ge. 0) &
            call RESERVOIR_NC_CHECK(nf90_close(reservoir_ncid), &
                "close reservoir data file")
        if (allocated(reservoir_time_days)) deallocate(reservoir_time_days)
        reservoir_ncid = -1
        storage_varid = -1
        withdraw_epi_varid = -1
        withdraw_hyp_varid = -1
        reservoir_start_index = 1
        reservoir_nc_open = .false.
    end subroutine CLOSE_RESERVOIR_NC

    subroutine LOWERCASE_RESERVOIR(text)
        character(len=*), intent(inout) :: text
        integer :: i, code

        do i = 1, len_trim(text)
            code = iachar(text(i:i))
            if (code .ge. iachar("A") .and. code .le. iachar("Z")) &
                text(i:i) = achar(code + 32)
        end do
    end subroutine LOWERCASE_RESERVOIR

end module Netcdf_Reservoir
