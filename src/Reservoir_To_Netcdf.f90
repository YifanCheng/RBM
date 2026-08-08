program Reservoir_To_Netcdf
    use netcdf
    implicit none

    character(len=512) :: parameter_file, storage_file, withdrawal_file
    character(len=512) :: output_file
    character(len=256) :: line, storage_date, withdrawal_date
    character(len=64) :: time_units
    integer :: nres, ntime, day, reservoir, ios
    integer :: year, month, date_day, first_year, first_month, first_day
    integer :: current_julian, previous_julian
    integer, external :: Julian
    integer, allocatable :: reservoir_id(:), start_node(:), end_node(:)
    integer, allocatable :: time_values(:)
    real(8), allocatable :: grid_lat(:), grid_lon(:), depth_m(:), width_m(:)
    real(8), allocatable :: area_km2(:), capacity_mcm(:)
    real(8), allocatable :: storage(:,:), withdraw_epi(:,:), withdraw_hyp(:,:)

    if (command_argument_count() .ne. 4) then
        write(*,*) "Usage: reservoir_to_netcdf <parameters.txt> <storage.txt> ", &
            "<withdrawal.txt> <reservoir_data.nc>"
        stop 1
    end if
    call get_command_argument(1, parameter_file)
    call get_command_argument(2, storage_file)
    call get_command_argument(3, withdrawal_file)
    call get_command_argument(4, output_file)

    call COUNT_RESERVOIRS(trim(parameter_file), nres)
    call COUNT_DAYS(trim(storage_file), nres, ntime)
    if (nres .lt. 1 .or. ntime .lt. 1) then
        write(*,*) "Reservoir conversion error: input files contain no data."
        stop 1
    end if

    allocate(reservoir_id(nres), start_node(nres), end_node(nres))
    allocate(grid_lat(nres), grid_lon(nres), depth_m(nres), width_m(nres))
    allocate(area_km2(nres), capacity_mcm(nres))
    allocate(time_values(ntime))
    allocate(storage(nres,ntime), withdraw_epi(nres,ntime), &
        withdraw_hyp(nres,ntime))

    open(unit=10, file=trim(parameter_file), status="old", action="read")
    read(10, "(A)", iostat=ios) line
    if (ios .ne. 0) call CONVERSION_ERROR("cannot read parameter header")
    do reservoir = 1, nres
        read(10, *, iostat=ios) reservoir_id(reservoir), grid_lat(reservoir), &
            grid_lon(reservoir), depth_m(reservoir), width_m(reservoir), &
            area_km2(reservoir), capacity_mcm(reservoir), &
            start_node(reservoir), end_node(reservoir)
        if (ios .ne. 0) call CONVERSION_ERROR("cannot read reservoir parameters")
        if (reservoir_id(reservoir) .ne. reservoir) then
            call CONVERSION_ERROR("reservoir IDs must be ordered 1..nres")
        end if
    end do
    close(10)

    open(unit=11, file=trim(storage_file), status="old", action="read")
    open(unit=12, file=trim(withdrawal_file), status="old", action="read")
    do day = 1, ntime
        read(11, "(A)", iostat=ios) storage_date
        if (ios .ne. 0) call CONVERSION_ERROR("storage file ended early")
        read(12, "(A)", iostat=ios) withdrawal_date
        if (ios .ne. 0) call CONVERSION_ERROR("withdrawal file ended early")
        call PARSE_DATE(storage_date, year, month, date_day)
        if (day .eq. 1) then
            first_year = year
            first_month = month
            first_day = date_day
            previous_julian = Julian(year, month, date_day) - 1
        end if
        current_julian = Julian(year, month, date_day)
        if (current_julian .ne. previous_julian + 1) &
            call CONVERSION_ERROR("storage dates are not consecutive")
        time_values(day) = current_julian - &
            Julian(first_year, first_month, first_day)
        previous_julian = current_julian

        call CHECK_MATCHING_DATE(withdrawal_date, year, month, date_day)
        do reservoir = 1, nres
            read(11, *, iostat=ios) storage(reservoir,day)
            if (ios .ne. 0) call CONVERSION_ERROR("cannot read daily storage")
            read(12, *, iostat=ios) withdraw_epi(reservoir,day), &
                withdraw_hyp(reservoir,day)
            if (ios .ne. 0) call CONVERSION_ERROR("cannot read withdrawal fractions")
            if (storage(reservoir,day) .lt. 0.0d0) &
                call CONVERSION_ERROR("storage values must be nonnegative")
            if (withdraw_epi(reservoir,day) .lt. 0.0d0 .or. &
                withdraw_hyp(reservoir,day) .lt. 0.0d0 .or. &
                abs(withdraw_epi(reservoir,day) + &
                withdraw_hyp(reservoir,day) - 1.0d0) .gt. 1.0d-6) then
                call CONVERSION_ERROR("withdrawal fractions must be nonnegative and sum to 1")
            end if
        end do
    end do
    read(11, "(A)", iostat=ios) line
    if (ios .eq. 0) call CONVERSION_ERROR("storage file has extra records")
    read(12, "(A)", iostat=ios) line
    if (ios .eq. 0) call CONVERSION_ERROR("withdrawal file has extra records")
    close(11)
    close(12)

    write(time_units, "(A,I4.4,A,I2.2,A,I2.2)") "days since ", first_year, &
        "-", first_month, "-", first_day
    call WRITE_RESERVOIR_NETCDF(trim(output_file), trim(time_units), &
        reservoir_id, time_values, grid_lat, grid_lon, depth_m, width_m, &
        area_km2, capacity_mcm, start_node, end_node, storage, &
        withdraw_epi, withdraw_hyp)
    write(*,*) "Wrote netCDF reservoir file: ", trim(output_file)
    write(*,*) "  Reservoirs: ", nres
    write(*,*) "  Daily records: ", ntime

contains

    subroutine COUNT_RESERVOIRS(path, count)
        character(len=*), intent(in) :: path
        integer, intent(out) :: count
        integer :: status
        character(len=512) :: record

        count = 0
        open(unit=20, file=trim(path), status="old", action="read")
        read(20, "(A)", iostat=status) record
        if (status .ne. 0) call CONVERSION_ERROR("cannot read parameter file")
        do
            read(20, "(A)", iostat=status) record
            if (status .ne. 0) exit
            if (len_trim(record) .gt. 0) count = count + 1
        end do
        close(20)
    end subroutine COUNT_RESERVOIRS

    subroutine COUNT_DAYS(path, reservoir_count, count)
        character(len=*), intent(in) :: path
        integer, intent(in) :: reservoir_count
        integer, intent(out) :: count
        integer :: status, i
        real(8) :: ignored
        character(len=512) :: record

        count = 0
        open(unit=21, file=trim(path), status="old", action="read")
        do
            read(21, "(A)", iostat=status) record
            if (status .ne. 0) exit
            if (len_trim(record) .eq. 0) &
                call CONVERSION_ERROR("blank date in storage file")
            count = count + 1
            do i = 1, reservoir_count
                read(21, *, iostat=status) ignored
                if (status .ne. 0) &
                    call CONVERSION_ERROR("incomplete day in storage file")
            end do
        end do
        close(21)
    end subroutine COUNT_DAYS

    subroutine PARSE_DATE(text, parsed_year, parsed_month, parsed_day)
        character(len=*), intent(in) :: text
        integer, intent(out) :: parsed_year, parsed_month, parsed_day
        integer :: status

        if (index(text, "-") .gt. 0) then
            read(text, "(i4,1x,i2,1x,i2)", iostat=status) parsed_year, &
                parsed_month, parsed_day
        else
            read(text, "(i4,i2,i2)", iostat=status) parsed_year, &
                parsed_month, parsed_day
        end if
        if (status .ne. 0) call CONVERSION_ERROR("invalid date in daily file")
    end subroutine PARSE_DATE

    subroutine CHECK_MATCHING_DATE(text, expected_year, expected_month, &
        expected_day)
        character(len=*), intent(in) :: text
        integer, intent(in) :: expected_year, expected_month, expected_day
        integer :: parsed_year, parsed_month, parsed_day

        call PARSE_DATE(text, parsed_year, parsed_month, parsed_day)
        if (parsed_year .ne. expected_year .or. &
            parsed_month .ne. expected_month .or. &
            parsed_day .ne. expected_day) then
            call CONVERSION_ERROR("storage and withdrawal dates do not match")
        end if
    end subroutine CHECK_MATCHING_DATE

    subroutine WRITE_RESERVOIR_NETCDF(path, units, ids, times, lat, lon, &
        depth, width, area, capacity, first_node, last_node, daily_storage, &
        daily_epi, daily_hyp)
        character(len=*), intent(in) :: path, units
        integer, intent(in) :: ids(:), times(:), first_node(:), last_node(:)
        real(8), intent(in) :: lat(:), lon(:), depth(:), width(:), area(:)
        real(8), intent(in) :: capacity(:), daily_storage(:,:)
        real(8), intent(in) :: daily_epi(:,:), daily_hyp(:,:)
        integer :: ncid, reservoir_dimid, time_dimid
        integer :: id_varid, time_varid, lat_varid, lon_varid
        integer :: depth_varid, width_varid, area_varid, capacity_varid
        integer :: start_varid, end_varid, storage_id, epi_id, hyp_id

        call NC_CONVERT_CHECK(nf90_create(path, nf90_clobber, ncid), "create output")
        call NC_CONVERT_CHECK(nf90_def_dim(ncid, "reservoir", size(ids), &
            reservoir_dimid), "define reservoir dimension")
        call NC_CONVERT_CHECK(nf90_def_dim(ncid, "time", size(times), &
            time_dimid), "define time dimension")
        call DEFINE_INT(ncid, "reservoir_id", (/reservoir_dimid/), id_varid)
        call DEFINE_INT(ncid, "time", (/time_dimid/), time_varid)
        call DEFINE_DOUBLE(ncid, "grid_lat", (/reservoir_dimid/), lat_varid)
        call DEFINE_DOUBLE(ncid, "grid_lon", (/reservoir_dimid/), lon_varid)
        call DEFINE_DOUBLE(ncid, "depth_m", (/reservoir_dimid/), depth_varid)
        call DEFINE_DOUBLE(ncid, "width_m", (/reservoir_dimid/), width_varid)
        call DEFINE_DOUBLE(ncid, "area_km2", (/reservoir_dimid/), area_varid)
        call DEFINE_DOUBLE(ncid, "capacity_mcm", (/reservoir_dimid/), capacity_varid)
        call DEFINE_INT(ncid, "start_node", (/reservoir_dimid/), start_varid)
        call DEFINE_INT(ncid, "end_node", (/reservoir_dimid/), end_varid)
        call DEFINE_DOUBLE(ncid, "storage", (/reservoir_dimid,time_dimid/), &
            storage_id)
        call DEFINE_DOUBLE(ncid, "withdraw_epilimnion", &
            (/reservoir_dimid,time_dimid/), epi_id)
        call DEFINE_DOUBLE(ncid, "withdraw_hypolimnion", &
            (/reservoir_dimid,time_dimid/), hyp_id)

        call NC_CONVERT_CHECK(nf90_put_att(ncid, nf90_global, "title", &
            "RBM reservoir parameters and daily forcing"), "write title")
        call NC_CONVERT_CHECK(nf90_put_att(ncid, time_varid, "units", units), &
            "write time units")
        call NC_CONVERT_CHECK(nf90_put_att(ncid, time_varid, "calendar", &
            "standard"), "write calendar")
        call PUT_UNITS(ncid, lat_varid, "degrees_north")
        call PUT_UNITS(ncid, lon_varid, "degrees_east")
        call PUT_UNITS(ncid, depth_varid, "m")
        call PUT_UNITS(ncid, width_varid, "m")
        call PUT_UNITS(ncid, area_varid, "km2")
        call PUT_UNITS(ncid, capacity_varid, "million m3")
        call PUT_UNITS(ncid, storage_id, "m3")
        call PUT_UNITS(ncid, epi_id, "1")
        call PUT_UNITS(ncid, hyp_id, "1")
        call NC_CONVERT_CHECK(nf90_enddef(ncid), "end define mode")

        call NC_CONVERT_CHECK(nf90_put_var(ncid, id_varid, ids), "write IDs")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, time_varid, times), "write time")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, lat_varid, lat), "write latitude")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, lon_varid, lon), "write longitude")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, depth_varid, depth), "write depth")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, width_varid, width), "write width")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, area_varid, area), "write area")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, capacity_varid, capacity), &
            "write capacity")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, start_varid, first_node), &
            "write start nodes")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, end_varid, last_node), &
            "write end nodes")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, storage_id, daily_storage), &
            "write storage")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, epi_id, daily_epi), &
            "write epilimnion fractions")
        call NC_CONVERT_CHECK(nf90_put_var(ncid, hyp_id, daily_hyp), &
            "write hypolimnion fractions")
        call NC_CONVERT_CHECK(nf90_close(ncid), "close output")
    end subroutine WRITE_RESERVOIR_NETCDF

    subroutine DEFINE_INT(ncid, name, dimensions, varid)
        integer, intent(in) :: ncid, dimensions(:)
        character(len=*), intent(in) :: name
        integer, intent(out) :: varid
        call NC_CONVERT_CHECK(nf90_def_var(ncid, name, nf90_int, dimensions, &
            varid), "define "//trim(name))
    end subroutine DEFINE_INT

    subroutine DEFINE_DOUBLE(ncid, name, dimensions, varid)
        integer, intent(in) :: ncid, dimensions(:)
        character(len=*), intent(in) :: name
        integer, intent(out) :: varid
        call NC_CONVERT_CHECK(nf90_def_var(ncid, name, nf90_double, dimensions, &
            varid), "define "//trim(name))
    end subroutine DEFINE_DOUBLE

    subroutine PUT_UNITS(ncid, varid, units)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: units
        call NC_CONVERT_CHECK(nf90_put_att(ncid, varid, "units", units), &
            "write variable units")
    end subroutine PUT_UNITS

    subroutine NC_CONVERT_CHECK(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in) :: context
        if (status .ne. nf90_noerr) then
            write(*,*) "Reservoir conversion error: ", trim(context), ": ", &
                trim(nf90_strerror(status))
            stop 1
        end if
    end subroutine NC_CONVERT_CHECK

    subroutine CONVERSION_ERROR(message)
        character(len=*), intent(in) :: message
        write(*,*) "Reservoir conversion error: ", trim(message)
        stop 1
    end subroutine CONVERSION_ERROR

end program Reservoir_To_Netcdf
