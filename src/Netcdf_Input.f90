module Netcdf_Input
    use netcdf
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use Block_Control
    use Block_Energy
    use Block_Hydro
    use Block_Network
    use Block_Reservoir
    use Netcdf_Reservoir
    implicit none

    integer :: flow_ncid = -1
    integer :: heat_ncid = -1
    integer :: local_flow_ncid = -1
    integer :: area_ncid = -1
    integer :: flow_varid = -1
    integer :: flow_id_varid = -1
    integer :: heat_id_varid = -1
    integer :: tair_varid = -1
    integer :: vp_varid = -1
    integer :: sw_varid = -1
    integer :: lw_varid = -1
    integer :: press_varid = -1
    integer :: wind_varid = -1
    integer :: density_varid = -1
    integer :: local_runoff_varid = -1
    integer :: local_baseflow_varid = -1
    integer :: area_varid = -1
    integer :: heat_remap_ncid = -1
    integer, allocatable :: remap_overlaps(:)
    integer, allocatable :: remap_i_index(:)
    integer, allocatable :: remap_j_index(:)
    integer, allocatable :: remap_start(:)
    real(8), allocatable :: remap_weight(:)
    logical :: heat_remap_mode = .false.
    integer(kind=8), allocatable :: flow_ids(:)
    integer(kind=8), allocatable :: heat_ids(:)
    logical :: nc_forcing_open = .false.
    logical :: raw_heat_schema = .false.
    logical :: grid_flow_schema = .false.
    logical :: grid_heat_schema = .false.
    logical :: heat_source_is_grid = .false.
    logical :: local_flow_enabled = .false.
    integer, allocatable :: flow_grid_i(:), flow_grid_j(:)
    integer, allocatable :: heat_grid_i(:), heat_grid_j(:)
    integer, allocatable :: local_grid_i(:), local_grid_j(:)
    integer, allocatable :: area_grid_i(:), area_grid_j(:)
    integer, allocatable :: flow_cell_index(:), heat_cell_index(:)
    integer, allocatable :: flow_time_days(:), heat_time_days(:), local_time_days(:)
    integer :: flow_value_count = 0, heat_value_count = 0
    integer :: flow_nlon = 0, flow_nlat = 0, heat_nlon = 0, heat_nlat = 0
    integer :: local_nlon = 0, local_nlat = 0, area_nlon = 0, area_nlat = 0
    integer :: cached_flow_day = -1, cached_heat_day = -1, cached_local_day = -1
    real(kind=8), allocatable :: flow_day_values(:,:), flow_grid_day(:,:,:)
    real, allocatable :: runoff_grid_day(:,:,:), baseflow_grid_day(:,:,:), area_grid(:,:)
    real, allocatable :: tair_day(:,:), vp_day(:,:), sw_day(:,:), lw_day(:,:), press_day(:,:), wind_day(:,:)
    real, allocatable :: density_day(:,:)
    real, allocatable :: tair_grid_day(:,:,:), vp_grid_day(:,:,:), sw_grid_day(:,:,:), lw_grid_day(:,:,:)
    real, allocatable :: press_grid_day(:,:,:), wind_grid_day(:,:,:), density_grid_day(:,:,:)
    real :: flow_fill = 0.0, tair_fill = 0.0, vp_fill = 0.0, sw_fill = 0.0
    real :: lw_fill = 0.0, press_fill = 0.0, wind_fill = 0.0, density_fill = 0.0
    real :: runoff_fill = 0.0, baseflow_fill = 0.0, area_fill = 0.0
    logical :: flow_has_fill = .false., tair_has_fill = .false., vp_has_fill = .false.
    logical :: sw_has_fill = .false., lw_has_fill = .false., press_has_fill = .false.
    logical :: wind_has_fill = .false., density_has_fill = .false.
    logical :: runoff_has_fill = .false., baseflow_has_fill = .false., area_has_fill = .false.
    integer :: flow_time_origin_year, flow_time_origin_month, flow_time_origin_day
    integer :: heat_time_origin_year, heat_time_origin_month, heat_time_origin_day
    integer :: local_time_origin_year, local_time_origin_month, local_time_origin_day
    integer :: flow_start_index = 1
    integer :: heat_start_index = 1
    integer :: local_start_index = 1
    character(len=32) :: flow_time_calendar = "standard", heat_time_calendar = "standard"
    character(len=32) :: local_time_calendar = "standard"

contains

    subroutine NC_INPUT_CHECK(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in) :: context

        if (status /= nf90_noerr) then
            write(*,*) 'NetCDF input error: ', trim(context), ': ', trim(nf90_strerror(status))
            stop 1
        end if
    end subroutine NC_INPUT_CHECK

    subroutine CHECK_VECTOR_VARIABLE(ncid, varid, spatial_dimension, label)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: spatial_dimension, label
        integer :: ndims, dimids(nf90_max_var_dims)
        character(len=nf90_max_name) :: first_name, second_name

        call NC_INPUT_CHECK(nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids), &
            "inspect dimensions for "//trim(label))
        if (ndims .ne. 2) then
            write(*,*) "NetCDF input error: ", trim(label), " must have dimensions (time,", &
                trim(spatial_dimension), ")."
            stop 1
        end if
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, dimids(1), name=first_name), &
            "inspect first dimension for "//trim(label))
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, dimids(2), name=second_name), &
            "inspect second dimension for "//trim(label))
        if (trim(first_name) .ne. trim(spatial_dimension) .or. trim(second_name) .ne. "time") then
            write(*,*) "NetCDF input error: ", trim(label), " must use dimensions (time,", &
                trim(spatial_dimension), "); found Fortran order (", trim(first_name), ",", &
                trim(second_name), ")."
            stop 1
        end if
    end subroutine CHECK_VECTOR_VARIABLE

    subroutine CHECK_GRID_VARIABLE(ncid, varid, label)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: label
        integer :: ndims, dimids(nf90_max_var_dims)
        character(len=nf90_max_name) :: first_name, second_name, third_name

        call NC_INPUT_CHECK(nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids), &
            "inspect dimensions for "//trim(label))
        if (ndims .ne. 3) then
            write(*,*) "NetCDF input error: ", trim(label), " must have dimensions (time,lat,lon)."
            stop 1
        end if
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, dimids(1), name=first_name), &
            "inspect first dimension for "//trim(label))
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, dimids(2), name=second_name), &
            "inspect second dimension for "//trim(label))
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, dimids(3), name=third_name), &
            "inspect third dimension for "//trim(label))
        if (trim(first_name) .ne. "lon" .or. trim(second_name) .ne. "lat" .or. &
            trim(third_name) .ne. "time") then
            write(*,*) "NetCDF input error: ", trim(label), &
                " must use dimensions (time,lat,lon); found Fortran order (", trim(first_name), ",", &
                trim(second_name), ",", trim(third_name), ")."
            stop 1
        end if
    end subroutine CHECK_GRID_VARIABLE

    subroutine CHECK_AREA_VARIABLE(ncid, varid, label)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: label
        integer :: ndims, dimids(nf90_max_var_dims)
        character(len=nf90_max_name) :: first_name, second_name

        call NC_INPUT_CHECK(nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids), &
            "inspect dimensions for "//trim(label))
        if (ndims .ne. 2) then
            write(*,*) "NetCDF input error: ", trim(label), " must have dimensions (lat,lon)."
            stop 1
        end if
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, dimids(1), name=first_name), &
            "inspect first dimension for "//trim(label))
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, dimids(2), name=second_name), &
            "inspect second dimension for "//trim(label))
        if (trim(first_name) .ne. "lon" .or. trim(second_name) .ne. "lat") then
            write(*,*) "NetCDF input error: ", trim(label), &
                " must use dimensions (lat,lon); found Fortran order (", trim(first_name), ",", &
                trim(second_name), ")."
            stop 1
        end if
    end subroutine CHECK_AREA_VARIABLE

    subroutine CHECK_HEAT_VARIABLES(is_grid)
        logical, intent(in) :: is_grid

        if (is_grid) then
            call CHECK_GRID_VARIABLE(heat_ncid, tair_varid, "air temperature")
            call CHECK_GRID_VARIABLE(heat_ncid, vp_varid, "humidity/vapor pressure")
            call CHECK_GRID_VARIABLE(heat_ncid, sw_varid, "shortwave radiation")
            call CHECK_GRID_VARIABLE(heat_ncid, lw_varid, "longwave radiation")
            call CHECK_GRID_VARIABLE(heat_ncid, press_varid, "air pressure")
            call CHECK_GRID_VARIABLE(heat_ncid, wind_varid, "wind")
            if (density_varid .ge. 0) call CHECK_GRID_VARIABLE(heat_ncid, density_varid, "air density")
        else
            call CHECK_VECTOR_VARIABLE(heat_ncid, tair_varid, "hru", "air temperature")
            call CHECK_VECTOR_VARIABLE(heat_ncid, vp_varid, "hru", "humidity/vapor pressure")
            call CHECK_VECTOR_VARIABLE(heat_ncid, sw_varid, "hru", "shortwave radiation")
            call CHECK_VECTOR_VARIABLE(heat_ncid, lw_varid, "hru", "longwave radiation")
            call CHECK_VECTOR_VARIABLE(heat_ncid, press_varid, "hru", "air pressure")
            call CHECK_VECTOR_VARIABLE(heat_ncid, wind_varid, "hru", "wind")
            if (density_varid .ge. 0) call CHECK_VECTOR_VARIABLE(heat_ncid, density_varid, "hru", "air density")
        end if
    end subroutine CHECK_HEAT_VARIABLES

    subroutine GET_GRID_SHAPE(ncid, nlon, nlat, label)
        integer, intent(in) :: ncid
        integer, intent(out) :: nlon, nlat
        character(len=*), intent(in) :: label
        integer :: dimid

        call NC_INPUT_CHECK(nf90_inq_dimid(ncid, "lon", dimid), "find "//trim(label)//" lon dimension")
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, dimid, len=nlon), "read "//trim(label)//" lon length")
        call NC_INPUT_CHECK(nf90_inq_dimid(ncid, "lat", dimid), "find "//trim(label)//" lat dimension")
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, dimid, len=nlat), "read "//trim(label)//" lat length")
    end subroutine GET_GRID_SHAPE

    subroutine READ_FILL_VALUE(ncid, varid, fill_value, has_fill)
        integer, intent(in) :: ncid, varid
        real, intent(out) :: fill_value
        logical, intent(out) :: has_fill
        integer :: status

        fill_value = 0.0
        status = nf90_get_att(ncid, varid, "_FillValue", fill_value)
        if (status .eq. nf90_noerr) then
            has_fill = .true.
            return
        end if
        if (status .ne. nf90_enotatt) call NC_INPUT_CHECK(status, "read variable _FillValue")
        status = nf90_get_att(ncid, varid, "missing_value", fill_value)
        has_fill = (status .eq. nf90_noerr)
        if (status .ne. nf90_noerr .and. status .ne. nf90_enotatt) &
            call NC_INPUT_CHECK(status, "read variable missing_value")
        if (.not. has_fill) fill_value = 0.0
    end subroutine READ_FILL_VALUE

    subroutine INIT_HEAT_METADATA()
        call READ_FILL_VALUE(heat_ncid, tair_varid, tair_fill, tair_has_fill)
        call READ_FILL_VALUE(heat_ncid, vp_varid, vp_fill, vp_has_fill)
        call READ_FILL_VALUE(heat_ncid, sw_varid, sw_fill, sw_has_fill)
        call READ_FILL_VALUE(heat_ncid, lw_varid, lw_fill, lw_has_fill)
        call READ_FILL_VALUE(heat_ncid, press_varid, press_fill, press_has_fill)
        call READ_FILL_VALUE(heat_ncid, wind_varid, wind_fill, wind_has_fill)
        if (density_varid .ge. 0) &
            call READ_FILL_VALUE(heat_ncid, density_varid, density_fill, density_has_fill)

        if (raw_heat_schema) then
            call CHECK_UNITS(heat_ncid, tair_varid, "TSA", "temperature_k")
            call CHECK_UNITS(heat_ncid, vp_varid, "RH2M", "relative_humidity")
            call CHECK_UNITS(heat_ncid, sw_varid, "FSDS", "radiation")
            call CHECK_UNITS(heat_ncid, lw_varid, "FLDS", "radiation")
            call CHECK_UNITS(heat_ncid, press_varid, "PBOT", "pressure_pa")
            call CHECK_UNITS(heat_ncid, wind_varid, "U10", "wind")
        else if (heat_source_is_grid) then
            call CHECK_UNITS(heat_ncid, tair_varid, trim(control_heat_tair_var), "temperature_c")
            call CHECK_UNITS(heat_ncid, vp_varid, trim(control_heat_vp_var), "pressure_kpa")
            call CHECK_UNITS(heat_ncid, sw_varid, trim(control_heat_sw_var), "radiation")
            call CHECK_UNITS(heat_ncid, lw_varid, trim(control_heat_lw_var), "radiation")
            call CHECK_UNITS(heat_ncid, press_varid, trim(control_heat_press_var), "pressure_kpa")
            call CHECK_UNITS(heat_ncid, wind_varid, trim(control_heat_wind_var), "wind")
            if (density_varid .ge. 0) &
                call CHECK_UNITS(heat_ncid, density_varid, trim(control_heat_density_var), "density")
        end if
    end subroutine INIT_HEAT_METADATA

    subroutine CHECK_UNITS(ncid, varid, label, unit_class)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: label, unit_class
        integer :: status
        character(len=128) :: units, normalized
        logical :: valid

        units = ""
        status = nf90_get_att(ncid, varid, "units", units)
        if (status .eq. nf90_enotatt) then
            write(*,*) "NetCDF input warning: ", trim(label), " has no units attribute; expected ", trim(unit_class)
            return
        end if
        call NC_INPUT_CHECK(status, "read units for "//trim(label))
        normalized = units
        call NORMALIZE_UNITS(normalized)
        select case (trim(unit_class))
        case ("flow")
            if (trim(control_flow_units) .eq. "cfs") then
                valid = trim(normalized) .eq. "cfs" .or. trim(normalized) .eq. "ft3/s" .or. &
                    trim(normalized) .eq. "ft3s-1"
            else
                valid = trim(normalized) .eq. "m3/s" .or. trim(normalized) .eq. "m3s-1"
            end if
        case ("temperature_k")
            valid = trim(normalized) .eq. "k" .or. trim(normalized) .eq. "kelvin"
        case ("temperature_c")
            valid = trim(normalized) .eq. "c" .or. trim(normalized) .eq. "degc" .or. &
                trim(normalized) .eq. "degreecelsius"
        case ("relative_humidity")
            valid = trim(normalized) .eq. "%" .or. trim(normalized) .eq. "percent"
        case ("radiation")
            valid = trim(normalized) .eq. "w/m2" .or. trim(normalized) .eq. "wm-2"
        case ("pressure_pa")
            valid = trim(normalized) .eq. "pa" .or. trim(normalized) .eq. "pascal"
        case ("pressure_kpa")
            valid = trim(normalized) .eq. "kpa"
        case ("wind")
            valid = trim(normalized) .eq. "m/s" .or. trim(normalized) .eq. "ms-1"
        case ("density")
            valid = trim(normalized) .eq. "kg/m3" .or. trim(normalized) .eq. "kgm-3"
        case ("runoff_depth")
            valid = trim(normalized) .eq. "mm" .or. trim(normalized) .eq. "mm/day" .or. &
                trim(normalized) .eq. "mmd-1"
        case ("area")
            valid = trim(normalized) .eq. "m2"
        case default
            valid = .false.
        end select
        if (.not. valid) then
            write(*,*) "NetCDF input error: ", trim(label), " units '", trim(units), &
                "' are incompatible with expected ", trim(unit_class), "."
            stop 1
        end if
    end subroutine CHECK_UNITS

    subroutine NORMALIZE_UNITS(text)
        character(len=*), intent(inout) :: text
        character(len=len(text)) :: compact
        integer :: i, j, code

        compact = ""
        j = 0
        do i = 1, len_trim(text)
            if (text(i:i) .eq. " " .or. text(i:i) .eq. "^" .or. text(i:i) .eq. "_") cycle
            j = j + 1
            code = iachar(text(i:i))
            if (code .ge. iachar("A") .and. code .le. iachar("Z")) then
                compact(j:j) = achar(code + 32)
            else
                compact(j:j) = text(i:i)
            end if
        end do
        text = compact
    end subroutine NORMALIZE_UNITS

    subroutine CHECK_UNIQUE_IDS(ids, label)
        integer(kind=8), intent(in) :: ids(:)
        character(len=*), intent(in) :: label
        integer :: i, j

        do i = 1, size(ids)
            do j = i + 1, size(ids)
                if (ids(i) .eq. ids(j)) then
                    write(*,*) "NetCDF input error: duplicate ", trim(label), " ID ", ids(i), "."
                    stop 1
                end if
            end do
        end do
    end subroutine CHECK_UNIQUE_IDS

    subroutine INIT_LOCAL_FLOW(sy, sm, sd, ndays_needed)
        integer, intent(in) :: sy, sm, sd, ndays_needed
        integer :: var_ndims, var_dimids(nf90_max_var_dims), ntime, cell

        local_flow_enabled = (len_trim(control_local_flow_file) .gt. 0)
        if (.not. local_flow_enabled) return

        call NC_INPUT_CHECK(nf90_open(trim(control_local_flow_file), nf90_nowrite, local_flow_ncid), &
            "open local flow file")
        call NC_INPUT_CHECK(nf90_inq_varid(local_flow_ncid, trim(control_local_runoff_var), &
            local_runoff_varid), "find local runoff variable")
        call NC_INPUT_CHECK(nf90_inq_varid(local_flow_ncid, trim(control_local_baseflow_var), &
            local_baseflow_varid), "find local baseflow variable")
        call CHECK_GRID_VARIABLE(local_flow_ncid, local_runoff_varid, trim(control_local_runoff_var))
        call CHECK_GRID_VARIABLE(local_flow_ncid, local_baseflow_varid, trim(control_local_baseflow_var))
        call INIT_GRID_CELL_MAP(local_flow_ncid, local_grid_i, local_grid_j, local_nlon, local_nlat, &
            "local flow")
        call READ_FILL_VALUE(local_flow_ncid, local_runoff_varid, runoff_fill, runoff_has_fill)
        call READ_FILL_VALUE(local_flow_ncid, local_baseflow_varid, baseflow_fill, baseflow_has_fill)
        call CHECK_UNITS(local_flow_ncid, local_runoff_varid, trim(control_local_runoff_var), "runoff_depth")
        call CHECK_UNITS(local_flow_ncid, local_baseflow_varid, trim(control_local_baseflow_var), "runoff_depth")

        call NC_INPUT_CHECK(nf90_open(trim(control_area_file), nf90_nowrite, area_ncid), "open grid area file")
        call NC_INPUT_CHECK(nf90_inq_varid(area_ncid, trim(control_area_var), area_varid), &
            "find grid area variable")
        call CHECK_AREA_VARIABLE(area_ncid, area_varid, trim(control_area_var))
        call INIT_GRID_CELL_MAP(area_ncid, area_grid_i, area_grid_j, area_nlon, area_nlat, "area")
        call READ_FILL_VALUE(area_ncid, area_varid, area_fill, area_has_fill)
        call CHECK_UNITS(area_ncid, area_varid, trim(control_area_var), "area")

        allocate(runoff_grid_day(local_nlon, local_nlat, 1))
        allocate(baseflow_grid_day(local_nlon, local_nlat, 1))
        allocate(area_grid(area_nlon, area_nlat))
        call NC_INPUT_CHECK(nf90_get_var(area_ncid, area_varid, area_grid), "read grid cell area")
        do cell = 1, heat_cells
            if (VALUE_IS_MISSING(area_grid(area_grid_i(cell),area_grid_j(cell)), area_fill, area_has_fill) .or. &
                area_grid(area_grid_i(cell),area_grid_j(cell)) .le. 0.0) then
                write(*,*) "NetCDF input error: invalid grid area for topology cell ", cell, "."
                stop 1
            end if
        end do

        call INIT_TIME_AXIS(local_flow_ncid, "local flow", local_time_days, local_time_origin_year, &
            local_time_origin_month, local_time_origin_day, local_time_calendar)
        local_start_index = FIND_TIME_INDEX(local_time_days, local_time_origin_year, &
            local_time_origin_month, local_time_origin_day, local_time_calendar, sy, sm, sd, "local flow")
        call NC_INPUT_CHECK(nf90_inquire_variable(local_flow_ncid, local_runoff_varid, &
            ndims=var_ndims, dimids=var_dimids), "inquire local runoff dimensions")
        call NC_INPUT_CHECK(nf90_inquire_dimension(local_flow_ncid, var_dimids(var_ndims), len=ntime), &
            "read local runoff time dimension length")
        if (ntime .ne. size(local_time_days)) then
            write(*,*) "NetCDF input error: local runoff time dimension does not match time coordinate."
            stop 1
        end if
        if (local_start_index + ndays_needed - 1 .gt. size(local_time_days)) &
            call TIME_RANGE_ERROR("local flow", local_start_index, ndays_needed, size(local_time_days))
    end subroutine INIT_LOCAL_FLOW

    subroutine NC_INPUT_INIT()
        integer :: dimid, nflow, nheat
        integer :: var_ndims, var_dimids(nf90_max_var_dims), ntime
        integer :: sy, sm, sd, ey, em, ed, ndays_needed, i, ncheck
        integer :: status
        integer :: f_idx, h_idx
        integer, external :: Julian

        if (nc_forcing_open) return

        call NC_INPUT_CHECK(nf90_open(trim(control_flow_file), nf90_nowrite, flow_ncid), "open flow file")
        call NC_INPUT_CHECK(nf90_open(trim(control_heat_file), nf90_nowrite, heat_ncid), "open heat file")
        call NC_INPUT_CHECK(nf90_inq_varid(flow_ncid, trim(control_flow_var), flow_varid), "find flow variable")
        call NC_INPUT_CHECK(nf90_inquire_variable(flow_ncid, flow_varid, ndims=var_ndims, &
            dimids=var_dimids), "inquire flow variable dimensions")
        grid_flow_schema = (var_ndims .eq. 3)
        if (grid_flow_schema) then
            nflow = flow_cells
            call CHECK_GRID_VARIABLE(flow_ncid, flow_varid, trim(control_flow_var))
            call INIT_GRID_CELL_MAP(flow_ncid, flow_grid_i, flow_grid_j, flow_nlon, flow_nlat, "flow")
            flow_value_count = flow_nlon*flow_nlat
        else
            if (var_ndims .ne. 2) then
                write(*,*) "NetCDF input error: flow variable must use (time,seg) or (time,lat,lon)."
                stop 1
            end if
            call CHECK_VECTOR_VARIABLE(flow_ncid, flow_varid, "seg", trim(control_flow_var))
            call NC_INPUT_CHECK(nf90_inq_varid(flow_ncid, trim(control_flow_id_var), flow_id_varid), &
                "find flow id variable")
            call NC_INPUT_CHECK(nf90_inq_dimid(flow_ncid, "seg", dimid), "find flow seg dimension")
            call NC_INPUT_CHECK(nf90_inquire_dimension(flow_ncid, dimid, len=nflow), "read flow seg length")
            allocate(flow_ids(nflow))
            call NC_INPUT_CHECK(nf90_get_var(flow_ncid, flow_id_varid, flow_ids), "read flow ids")
            call CHECK_UNIQUE_IDS(flow_ids, "flow")
            flow_value_count = nflow
        end if
        call READ_FILL_VALUE(flow_ncid, flow_varid, flow_fill, flow_has_fill)
        call CHECK_UNITS(flow_ncid, flow_varid, trim(control_flow_var), "flow")

        heat_remap_mode = (LEN_TRIM(control_heat_remap_file) .gt. 0)
        status = nf90_inq_varid(heat_ncid, trim(control_heat_tair_var), tair_varid)
        raw_heat_schema = (status .ne. nf90_noerr .or. trim(control_heat_tair_var) .eq. "TSA")
        if (raw_heat_schema) then
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, "TSA", tair_varid), "find raw air temperature TSA")
        else
            call NC_INPUT_CHECK(status, "find air temperature")
        end if
        call NC_INPUT_CHECK(nf90_inquire_variable(heat_ncid, tair_varid, ndims=var_ndims), &
            "inquire heat variable dimensions")
        heat_source_is_grid = (var_ndims .eq. 3)
        grid_heat_schema = (heat_source_is_grid .and. .not. heat_remap_mode)

        if (raw_heat_schema) then
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, "RH2M", vp_varid), "find raw relative humidity RH2M")
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, "FSDS", sw_varid), "find raw shortwave FSDS")
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, "FLDS", lw_varid), "find raw longwave FLDS")
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, "PBOT", press_varid), "find raw air pressure PBOT")
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, "U10", wind_varid), "find raw wind U10")
        else
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_vp_var), vp_varid), "find vapor pressure")
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_sw_var), sw_varid), "find shortwave")
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_lw_var), lw_varid), "find longwave")
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_press_var), press_varid), "find air pressure")
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_wind_var), wind_varid), "find wind")
            if (LEN_TRIM(control_heat_density_var) .gt. 0) then
                call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_density_var), density_varid), &
                    "find air density")
            end if
        end if
        if (heat_source_is_grid) then
            call CHECK_HEAT_VARIABLES(.true.)
        else
            if (heat_remap_mode) then
                write(*,*) "NetCDF input error: heat_remap_file requires gridded heat variables."
                stop 1
            end if
            call CHECK_HEAT_VARIABLES(.false.)
        end if
        call INIT_HEAT_METADATA()

        if (heat_remap_mode) then
            call GET_GRID_SHAPE(heat_ncid, heat_nlon, heat_nlat, "heat")
            call INIT_HEAT_REMAP(nflow, nheat)
        else if (grid_heat_schema) then
            nheat = heat_cells
            call INIT_GRID_CELL_MAP(heat_ncid, heat_grid_i, heat_grid_j, heat_nlon, heat_nlat, "heat")
        else
            call NC_INPUT_CHECK(nf90_inq_varid(heat_ncid, trim(control_heat_id_var), heat_id_varid), &
                "find heat id variable")
            call NC_INPUT_CHECK(nf90_inq_dimid(heat_ncid, "hru", dimid), "find heat hru dimension")
            call NC_INPUT_CHECK(nf90_inquire_dimension(heat_ncid, dimid, len=nheat), "read heat hru length")
            allocate(heat_ids(nheat))
            call NC_INPUT_CHECK(nf90_get_var(heat_ncid, heat_id_varid, heat_ids), "read heat ids")
            call CHECK_UNIQUE_IDS(heat_ids, "heat")
        end if
        if (heat_source_is_grid) then
            heat_value_count = heat_nlon*heat_nlat
        else
            heat_value_count = nheat
        end if
        allocate(flow_cell_index(heat_cells))
        flow_cell_index = 0
        if (.not. grid_heat_schema) then
            allocate(heat_cell_index(heat_cells))
            ! The established preprocessing notebook writes heat records
            ! positionally in mizuRoute reach order.
            do i = 1, heat_cells
                heat_cell_index(i) = FIND_FLOW_INDEX(hru_id(i))
            end do
        end if
        call ALLOCATE_DAY_CACHE()

        read(control_start_date,"(i4,2i2)") sy, sm, sd
        read(control_end_date,  "(i4,2i2)") ey, em, ed
        ndays_needed = Julian(ey,em,ed) - Julian(sy,sm,sd) + 1
        call INIT_TIME_AXIS(flow_ncid, "flow", flow_time_days, flow_time_origin_year, &
            flow_time_origin_month, flow_time_origin_day, flow_time_calendar)
        call INIT_TIME_AXIS(heat_ncid, "heat", heat_time_days, heat_time_origin_year, &
            heat_time_origin_month, heat_time_origin_day, heat_time_calendar)
        flow_start_index = FIND_TIME_INDEX(flow_time_days, flow_time_origin_year, &
            flow_time_origin_month, flow_time_origin_day, flow_time_calendar, sy, sm, sd, "flow")
        heat_start_index = FIND_TIME_INDEX(heat_time_days, heat_time_origin_year, &
            heat_time_origin_month, heat_time_origin_day, heat_time_calendar, sy, sm, sd, "heat")
        call INIT_LOCAL_FLOW(sy, sm, sd, ndays_needed)

        call NC_INPUT_CHECK(nf90_inquire_variable(flow_ncid, flow_varid, ndims=var_ndims, &
            dimids=var_dimids), "inquire flow variable dimensions")
        if (var_ndims .lt. 2) then
            write(*,*) "NetCDF input error: flow variable ", trim(control_flow_var), &
                " must have at least 2 dimensions"
            stop 1
        end if
        call NC_INPUT_CHECK(nf90_inquire_dimension(flow_ncid, var_dimids(var_ndims), len=ntime), &
            "read flow time dimension length")
        if (ntime .ne. size(flow_time_days)) then
            write(*,*) "NetCDF input error: flow variable time dimension does not match time coordinate."
            stop 1
        end if
        call NC_INPUT_CHECK(nf90_inquire_variable(heat_ncid, tair_varid, ndims=var_ndims, &
            dimids=var_dimids), "inquire heat variable dimensions")
        if (var_ndims .lt. 2) then
            write(*,*) "NetCDF input error: heat variable must have at least 2 dimensions."
            stop 1
        end if
        call NC_INPUT_CHECK(nf90_inquire_dimension(heat_ncid, var_dimids(var_ndims), len=ntime), &
            "read heat time dimension length")
        if (ntime .ne. size(heat_time_days)) then
            write(*,*) "NetCDF input error: heat variable time dimension does not match time coordinate."
            stop 1
        end if
        if (flow_start_index + ndays_needed - 1 .gt. size(flow_time_days)) &
            call TIME_RANGE_ERROR("flow", flow_start_index, ndays_needed, size(flow_time_days))
        if (heat_start_index + ndays_needed - 1 .gt. size(heat_time_days)) &
            call TIME_RANGE_ERROR("heat", heat_start_index, ndays_needed, size(heat_time_days))

        write(*,*) "------------------------------------------------------------"
        write(*,*) "NetCDF forcing input mode"
        write(*,*) "  Flow file        : ", trim(control_flow_file)
        write(*,*) "  Heat file        : ", trim(control_heat_file)
        write(*,*) "  Flow variable    : ", trim(control_flow_var)
        if (grid_flow_schema) then
            write(*,*) "  Flow schema      : gridded (time/lat/lon)"
        else
            write(*,*) "  Flow id variable : ", trim(control_flow_id_var)
        end if
        if (heat_remap_mode) then
            write(*,*) "  Heat remap file  : ", trim(control_heat_remap_file)
        else if (grid_heat_schema) then
            write(*,*) "  Heat schema      : gridded (time/lat/lon)"
        else
            write(*,*) "  Heat id variable : ", trim(control_heat_id_var)
            write(*,*) "  Heat mapping     : positional preprocessing-compatible order"
        end if
        if (raw_heat_schema) then
            write(*,*) "  Heat variables   : raw CLM TSA/RH2M/FSDS/FLDS/PBOT/U10"
        else
            write(*,*) "  Heat variables   : ", trim(control_heat_tair_var), " ", &
                trim(control_heat_vp_var), " ", trim(control_heat_sw_var), " ", &
                trim(control_heat_lw_var), " ", trim(control_heat_press_var), " ", &
                trim(control_heat_wind_var)
        end if
        write(*,*) "  Number of flow cells: ", nflow
        write(*,*) "  Number of heat cells: ", nheat
        write(*,*) "  Flow time records  : ", size(flow_time_days), " (start index ", flow_start_index, ")"
        write(*,*) "  Heat time records  : ", size(heat_time_days), " (start index ", heat_start_index, ")"
        write(*,*) "  Requested days     : ", ndays_needed
        ncheck = min(5, heat_cells)
        if (grid_flow_schema .and. grid_heat_schema) then
            write(*,*) "  First grid mappings (cell, flow_i, flow_j, heat_i, heat_j):"
            do i = 1, ncheck
                write(*,"(A,5I10)") "    ", i, flow_grid_i(i), flow_grid_j(i), heat_grid_i(i), heat_grid_j(i)
            end do
        else if (.not. grid_flow_schema .and. .not. grid_heat_schema) then
            write(*,*) "  First cell-to-ID mappings (cell, hru_id, flow_idx, heat_idx):"
            do i = 1, ncheck
                f_idx = FIND_FLOW_INDEX(hru_id(i))
                h_idx = heat_cell_index(i)
                write(*,"(A,4I12)") "    ", i, hru_id(i), f_idx, h_idx
            end do
        end if
        if (local_flow_enabled) then
            write(*,*) "  Local flow file   : ", trim(control_local_flow_file)
            write(*,*) "  Area file         : ", trim(control_area_file)
            write(*,*) "  Local flow vars   : ", trim(control_local_runoff_var), " ", &
                trim(control_local_baseflow_var), " / ", trim(control_area_var)
            write(*,*) "  Local flow records: ", size(local_time_days), " (start index ", local_start_index, ")"
        else
            write(*,*) "  Q_local           : 0 (no local_flow_file and area_file configured)"
        end if
        write(*,*) "  Q_diff            : 0 (netCDF forcing)"
        write(*,*) "------------------------------------------------------------"

        nc_forcing_open = .true.
    end subroutine NC_INPUT_INIT

    subroutine INIT_TIME_AXIS(ncid, label, axis_days, origin_year, origin_month, origin_day, calendar)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: label
        integer, allocatable, intent(out) :: axis_days(:)
        integer, intent(out) :: origin_year, origin_month, origin_day
        character(len=*), intent(out) :: calendar
        integer :: time_varid, var_ndims, var_dimids(nf90_max_var_dims)
        integer :: ntime, i, ios, since_pos, status
        real(8), allocatable :: time_values(:)
        character(len=256) :: units, units_lower, date_text

        status = nf90_inq_varid(ncid, "time", time_varid)
        if (status .eq. nf90_enotvar) then
            write(*,*) "NetCDF input error: ", trim(label), " file has no time coordinate."
            stop 1
        end if
        call NC_INPUT_CHECK(status, "find "//trim(label)//" time coordinate")
        call NC_INPUT_CHECK(nf90_inquire_variable(ncid, time_varid, ndims=var_ndims, &
            dimids=var_dimids), "inquire "//trim(label)//" time coordinate")
        if (var_ndims .ne. 1) then
            write(*,*) "NetCDF input error: ", trim(label), " time coordinate must be one-dimensional."
            stop 1
        end if
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, var_dimids(1), len=ntime), &
            "read "//trim(label)//" time length")
        allocate(time_values(ntime), axis_days(ntime))
        call NC_INPUT_CHECK(nf90_get_var(ncid, time_varid, time_values), &
            "read "//trim(label)//" time coordinate")

        units = ""
        call NC_INPUT_CHECK(nf90_get_att(ncid, time_varid, "units", units), &
            "read "//trim(label)//" time units")
        units_lower = units
        call LOWERCASE(units_lower)
        since_pos = index(units_lower, "since")
        if (index(adjustl(units_lower), "days since") .ne. 1 .or. since_pos .eq. 0) then
            write(*,*) "NetCDF input error: ", trim(label), &
                " time units must use 'days since YYYY-MM-DD'; found '", trim(units), "'."
            stop 1
        end if
        date_text = adjustl(units(since_pos+5:))
        read(date_text,"(i4,1x,i2,1x,i2)",iostat=ios) origin_year, origin_month, origin_day
        if (ios .ne. 0) then
            write(*,*) "NetCDF input error: cannot parse ", trim(label), " time origin from '", trim(units), "'."
            stop 1
        end if

        calendar = "standard"
        status = nf90_get_att(ncid, time_varid, "calendar", calendar)
        if (status .ne. nf90_noerr .and. status .ne. nf90_enotatt) &
            call NC_INPUT_CHECK(status, "read "//trim(label)//" calendar")
        call LOWERCASE(calendar)
        if (trim(calendar) .ne. "standard" .and. trim(calendar) .ne. "gregorian" .and. &
            trim(calendar) .ne. "proleptic_gregorian" .and. trim(calendar) .ne. "noleap" .and. &
            trim(calendar) .ne. "365_day") then
            write(*,*) "NetCDF input error: unsupported ", trim(label), " calendar '", trim(calendar), "'."
            stop 1
        end if

        do i = 1, ntime
            if (.not. ieee_is_finite(time_values(i))) then
                write(*,*) "NetCDF input error: ", trim(label), " time coordinate contains a missing value."
                stop 1
            end if
            axis_days(i) = floor(time_values(i) + 1.0e-6_8)
            if (i .gt. 1) then
                if (axis_days(i) .ne. axis_days(i-1) + 1) then
                    write(*,*) "NetCDF input error: ", trim(label), &
                        " time coordinate must contain one consecutive record per day."
                    stop 1
                end if
            end if
        end do
        deallocate(time_values)
    end subroutine INIT_TIME_AXIS

    integer function FIND_TIME_INDEX(axis_days, origin_year, origin_month, origin_day, calendar, &
        year, month, day, label)
        integer, intent(in) :: axis_days(:)
        integer, intent(in) :: origin_year, origin_month, origin_day
        integer, intent(in) :: year, month, day
        character(len=*), intent(in) :: calendar, label
        integer :: target_day, low, high, middle

        target_day = CALENDAR_DAY_OFFSET(origin_year, origin_month, origin_day, year, month, day, calendar)
        low = 1
        high = size(axis_days)
        do while (low .le. high)
            middle = (low + high)/2
            if (axis_days(middle) .eq. target_day) then
                FIND_TIME_INDEX = middle
                return
            else if (axis_days(middle) .lt. target_day) then
                low = middle + 1
            else
                high = middle - 1
            end if
        end do
        write(*,"(A,A,A,I4.4,2I2.2,A)") "NetCDF input error: ", trim(label), &
            " forcing has no daily record for ", year, month, day, "."
        stop 1
    end function FIND_TIME_INDEX

    integer function CALENDAR_DAY_OFFSET(origin_year, origin_month, origin_day, year, month, day, calendar)
        integer, intent(in) :: origin_year, origin_month, origin_day
        integer, intent(in) :: year, month, day
        character(len=*), intent(in) :: calendar
        integer, parameter :: month_start(12) = (/0,31,59,90,120,151,181,212,243,273,304,334/)
        integer, external :: Julian

        if (trim(calendar) .eq. "noleap" .or. trim(calendar) .eq. "365_day") then
            if (month .eq. 2 .and. day .eq. 29) then
                write(*,*) "NetCDF input error: February 29 is not present in a noleap forcing calendar."
                stop 1
            end if
            CALENDAR_DAY_OFFSET = 365*(year-origin_year) + month_start(month) + day - &
                month_start(origin_month) - origin_day
        else
            CALENDAR_DAY_OFFSET = Julian(year,month,day) - Julian(origin_year,origin_month,origin_day)
        end if
    end function CALENDAR_DAY_OFFSET

    subroutine TIME_RANGE_ERROR(label, start_index, requested_days, available_days)
        character(len=*), intent(in) :: label
        integer, intent(in) :: start_index, requested_days, available_days

        write(*,*) "NetCDF input error: ", trim(label), " forcing has ", available_days, &
            " records, but start index ", start_index, " plus ", requested_days, " requested days exceeds it."
        stop 1
    end subroutine TIME_RANGE_ERROR

    subroutine LOWERCASE(text)
        character(len=*), intent(inout) :: text
        integer :: i, code

        do i = 1, len_trim(text)
            code = iachar(text(i:i))
            if (code .ge. iachar("A") .and. code .le. iachar("Z")) text(i:i) = achar(code + 32)
        end do
    end subroutine LOWERCASE

    subroutine INIT_GRID_CELL_MAP(ncid, i_index, j_index, nlon, nlat, label)
        integer, intent(in) :: ncid
        integer, allocatable, intent(out) :: i_index(:), j_index(:)
        integer, intent(out) :: nlon, nlat
        character(len=*), intent(in) :: label
        integer :: lat_dimid, lon_dimid, lat_varid, lon_varid
        integer :: cell
        real(8), allocatable :: lat_values(:), lon_values(:)

        call NC_INPUT_CHECK(nf90_inq_dimid(ncid, "lat", lat_dimid), "find "//trim(label)//" lat dimension")
        call NC_INPUT_CHECK(nf90_inq_dimid(ncid, "lon", lon_dimid), "find "//trim(label)//" lon dimension")
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, lat_dimid, len=nlat), &
            "read "//trim(label)//" lat length")
        call NC_INPUT_CHECK(nf90_inquire_dimension(ncid, lon_dimid, len=nlon), &
            "read "//trim(label)//" lon length")
        call NC_INPUT_CHECK(nf90_inq_varid(ncid, "lat", lat_varid), "find "//trim(label)//" lat variable")
        call NC_INPUT_CHECK(nf90_inq_varid(ncid, "lon", lon_varid), "find "//trim(label)//" lon variable")

        allocate(lat_values(nlat), lon_values(nlon))
        allocate(i_index(heat_cells), j_index(heat_cells))
        call NC_INPUT_CHECK(nf90_get_var(ncid, lat_varid, lat_values), "read "//trim(label)//" lat values")
        call NC_INPUT_CHECK(nf90_get_var(ncid, lon_varid, lon_values), "read "//trim(label)//" lon values")

        if (any(.not. ieee_is_finite(lat_values)) .or. any(.not. ieee_is_finite(lon_values))) then
            write(*,*) "NetCDF input error: ", trim(label), " grid coordinates contain missing values."
            stop 1
        end if
        if (nlat .gt. 1) then
            if (any(abs(lat_values(2:) - lat_values(:nlat-1)) .le. epsilon(1.0_8))) then
                write(*,*) "NetCDF input error: ", trim(label), " latitude coordinate contains duplicates."
                stop 1
            end if
        end if
        if (nlon .gt. 1) then
            if (any(abs(lon_values(2:) - lon_values(:nlon-1)) .le. epsilon(1.0_8))) then
                write(*,*) "NetCDF input error: ", trim(label), " longitude coordinate contains duplicates."
                stop 1
            end if
        end if
        do cell = 1, heat_cells
            i_index(cell) = FIND_GRID_COORD(real(grid_lon(cell),kind=8), lon_values, cell, label, "longitude")
            j_index(cell) = FIND_GRID_COORD(real(grid_lat(cell),kind=8), lat_values, cell, label, "latitude")
        end do

        deallocate(lat_values, lon_values)
    end subroutine INIT_GRID_CELL_MAP

    integer function FIND_GRID_COORD(target, values, cell, label, coordinate)
        real(8), intent(in) :: target
        real(8), intent(in) :: values(:)
        integer, intent(in) :: cell
        character(len=*), intent(in) :: label, coordinate
        integer :: i, nearest
        real(8) :: difference, best

        nearest = 0
        best = huge(best)
        do i = 1, size(values)
            difference = abs(values(i) - target)
            if (difference .lt. best) then
                best = difference
                nearest = i
            end if
        end do
        if (nearest .lt. 1 .or. best .gt. 1.0e-5_8) then
            write(*,*) "NetCDF input error: topology cell ", cell, " ", trim(coordinate), target, &
                " not found in ", trim(label), " grid"
            stop 1
        end if
        FIND_GRID_COORD = nearest
    end function FIND_GRID_COORD



    subroutine INIT_HEAT_REMAP(nflow, nheat)
        integer, intent(in) :: nflow
        integer, intent(out) :: nheat
        integer :: dimid, data_len, varid, p, k, k0, k1
        integer(kind=8), allocatable :: remap_poly_ids(:)
        real(8), allocatable :: raw_weight(:)
        real(8) :: weight_sum

        call NC_INPUT_CHECK(nf90_open(trim(control_heat_remap_file), nf90_nowrite, heat_remap_ncid), &
            'open heat remap file')
        call NC_INPUT_CHECK(nf90_inq_dimid(heat_remap_ncid, 'polyid', dimid), 'find remap polyid dimension')
        call NC_INPUT_CHECK(nf90_inquire_dimension(heat_remap_ncid, dimid, len=nheat), &
            'read remap polyid length')
        call NC_INPUT_CHECK(nf90_inq_dimid(heat_remap_ncid, 'data', dimid), 'find remap data dimension')
        call NC_INPUT_CHECK(nf90_inquire_dimension(heat_remap_ncid, dimid, len=data_len), &
            'read remap data length')

        if (nheat .lt. nflow) then
            write(*,*) 'NetCDF input error: heat remap polyid count (', nheat, &
                ') is smaller than flow ID count (', nflow, ')'
            stop 1
        end if

        allocate(heat_ids(nheat))
        allocate(remap_poly_ids(nheat))
        allocate(remap_overlaps(nheat))
        allocate(remap_start(nheat))
        allocate(remap_i_index(data_len))
        allocate(remap_j_index(data_len))
        allocate(remap_weight(data_len))
        allocate(raw_weight(data_len))

        call NC_INPUT_CHECK(nf90_inq_varid(heat_remap_ncid, 'polyid', varid), 'find remap polyid')
        call NC_INPUT_CHECK(nf90_get_var(heat_remap_ncid, varid, remap_poly_ids), 'read remap polyid')
        call CHECK_UNIQUE_IDS(remap_poly_ids, "heat remap")
        heat_ids = remap_poly_ids
        call NC_INPUT_CHECK(nf90_inq_varid(heat_remap_ncid, 'overlaps', varid), 'find remap overlaps')
        call NC_INPUT_CHECK(nf90_get_var(heat_remap_ncid, varid, remap_overlaps), 'read remap overlaps')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_remap_ncid, 'i_index', varid), 'find remap i_index')
        call NC_INPUT_CHECK(nf90_get_var(heat_remap_ncid, varid, remap_i_index), 'read remap i_index')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_remap_ncid, 'j_index', varid), 'find remap j_index')
        call NC_INPUT_CHECK(nf90_get_var(heat_remap_ncid, varid, remap_j_index), 'read remap j_index')
        call NC_INPUT_CHECK(nf90_inq_varid(heat_remap_ncid, 'weight', varid), 'find remap weight')
        call NC_INPUT_CHECK(nf90_get_var(heat_remap_ncid, varid, raw_weight), 'read remap weight')

        k0 = 1
        do p = 1, nheat
            if (remap_overlaps(p) .le. 0) then
                write(*,*) "NetCDF input error: heat remap polyid ", heat_ids(p), &
                    " has no source-grid overlaps."
                stop 1
            end if
            remap_start(p) = k0
            k1 = k0 + remap_overlaps(p) - 1
            if (k1 .gt. data_len) then
                write(*,*) 'NetCDF input error: heat remap overlaps exceed data dimension at polyid index ', p
                stop 1
            end if
            weight_sum = 0.0_8
            do k = k0, k1
                if (.not. ieee_is_finite(raw_weight(k)) .or. raw_weight(k) .lt. 0.0_8) then
                    write(*,*) "NetCDF input error: invalid heat remap weight at data index ", k, "."
                    stop 1
                end if
                if (remap_i_index(k) .lt. 1 .or. remap_i_index(k) .gt. heat_nlon .or. &
                    remap_j_index(k) .lt. 1 .or. remap_j_index(k) .gt. heat_nlat) then
                    write(*,*) "NetCDF input error: heat remap grid index out of bounds at data index ", k, "."
                    stop 1
                end if
                weight_sum = weight_sum + raw_weight(k)
            end do
            if (weight_sum .gt. 0.0_8) then
                do k = k0, k1
                    remap_weight(k) = raw_weight(k) / weight_sum
                end do
            else
                write(*,*) "NetCDF input error: heat remap weights sum to zero for polyid ", heat_ids(p), "."
                stop 1
            end if
            k0 = k1 + 1
        end do
        if (k0 .ne. data_len + 1) then
            write(*,*) 'NetCDF input error: heat remap overlaps do not sum to data dimension.'
            stop 1
        end if

        deallocate(remap_poly_ids)
        deallocate(raw_weight)
    end subroutine INIT_HEAT_REMAP

    subroutine ALLOCATE_DAY_CACHE()
        if (grid_flow_schema) then
            allocate(flow_grid_day(flow_nlon, flow_nlat, 1))
        else
            allocate(flow_day_values(flow_value_count, 1))
        end if

        if (heat_source_is_grid) then
            allocate(tair_grid_day(heat_nlon, heat_nlat, 1), vp_grid_day(heat_nlon, heat_nlat, 1))
            allocate(sw_grid_day(heat_nlon, heat_nlat, 1), lw_grid_day(heat_nlon, heat_nlat, 1))
            allocate(press_grid_day(heat_nlon, heat_nlat, 1), wind_grid_day(heat_nlon, heat_nlat, 1))
            if (density_varid .ge. 0) allocate(density_grid_day(heat_nlon, heat_nlat, 1))
        else
            allocate(tair_day(heat_value_count, 1), vp_day(heat_value_count, 1))
            allocate(sw_day(heat_value_count, 1), lw_day(heat_value_count, 1))
            allocate(press_day(heat_value_count, 1), wind_day(heat_value_count, 1))
            if (density_varid .ge. 0) allocate(density_day(heat_value_count, 1))
        end if
    end subroutine ALLOCATE_DAY_CACHE

    subroutine LOAD_FORCING_DAY(flow_day_index, heat_day_index, local_day_index)
        integer, intent(in) :: flow_day_index, heat_day_index, local_day_index

        if (cached_flow_day .ne. flow_day_index) then
            if (grid_flow_schema) then
                call LOAD_FLOW_GRID_DAY(flow_ncid, flow_varid, flow_day_index, flow_grid_day)
            else
                call LOAD_FLOW_VECTOR_DAY(flow_ncid, flow_varid, flow_day_index, flow_day_values)
            end if
            cached_flow_day = flow_day_index
        end if

        if (cached_heat_day .ne. heat_day_index) then
            if (heat_source_is_grid) then
                call LOAD_GRID_DAY(heat_ncid, tair_varid, heat_day_index, tair_grid_day, "air temperature")
                call LOAD_GRID_DAY(heat_ncid, vp_varid, heat_day_index, vp_grid_day, "humidity/vapor pressure")
                call LOAD_GRID_DAY(heat_ncid, sw_varid, heat_day_index, sw_grid_day, "shortwave radiation")
                call LOAD_GRID_DAY(heat_ncid, lw_varid, heat_day_index, lw_grid_day, "longwave radiation")
                call LOAD_GRID_DAY(heat_ncid, press_varid, heat_day_index, press_grid_day, "air pressure")
                call LOAD_GRID_DAY(heat_ncid, wind_varid, heat_day_index, wind_grid_day, "wind")
                if (density_varid .ge. 0) &
                    call LOAD_GRID_DAY(heat_ncid, density_varid, heat_day_index, density_grid_day, "air density")
            else
                call LOAD_VECTOR_DAY(heat_ncid, tair_varid, heat_day_index, tair_day, "air temperature")
                call LOAD_VECTOR_DAY(heat_ncid, vp_varid, heat_day_index, vp_day, "humidity/vapor pressure")
                call LOAD_VECTOR_DAY(heat_ncid, sw_varid, heat_day_index, sw_day, "shortwave radiation")
                call LOAD_VECTOR_DAY(heat_ncid, lw_varid, heat_day_index, lw_day, "longwave radiation")
                call LOAD_VECTOR_DAY(heat_ncid, press_varid, heat_day_index, press_day, "air pressure")
                call LOAD_VECTOR_DAY(heat_ncid, wind_varid, heat_day_index, wind_day, "wind")
                if (density_varid .ge. 0) &
                    call LOAD_VECTOR_DAY(heat_ncid, density_varid, heat_day_index, density_day, "air density")
            end if
            cached_heat_day = heat_day_index
        end if
        if (local_flow_enabled .and. cached_local_day .ne. local_day_index) then
            call LOAD_GRID_DAY(local_flow_ncid, local_runoff_varid, local_day_index, &
                runoff_grid_day, "local runoff")
            call LOAD_GRID_DAY(local_flow_ncid, local_baseflow_varid, local_day_index, &
                baseflow_grid_day, "local baseflow")
            cached_local_day = local_day_index
        end if
    end subroutine LOAD_FORCING_DAY

    subroutine LOAD_FLOW_VECTOR_DAY(ncid, varid, day_index, values)
        integer, intent(in) :: ncid, varid, day_index
        real(kind=8), intent(out) :: values(:,:)
        character(len=160) :: context

        write(context,"(A,I0)") "read flow day ", day_index
        call NC_INPUT_CHECK(nf90_get_var(ncid, varid, values, start=(/1,day_index/), &
            count=(/size(values,1),1/)), trim(context))
    end subroutine LOAD_FLOW_VECTOR_DAY

    subroutine LOAD_FLOW_GRID_DAY(ncid, varid, day_index, values)
        integer, intent(in) :: ncid, varid, day_index
        real(kind=8), intent(out) :: values(:,:,:)
        character(len=160) :: context

        write(context,"(A,I0)") "read gridded flow day ", day_index
        call NC_INPUT_CHECK(nf90_get_var(ncid, varid, values, start=(/1,1,day_index/), &
            count=(/size(values,1),size(values,2),1/)), trim(context))
    end subroutine LOAD_FLOW_GRID_DAY

    subroutine LOAD_VECTOR_DAY(ncid, varid, day_index, values, label)
        integer, intent(in) :: ncid, varid, day_index
        real, intent(out) :: values(:,:)
        character(len=*), intent(in) :: label
        character(len=160) :: context

        write(context,"(A,A,A,I0)") "read ", trim(label), " day ", day_index
        call NC_INPUT_CHECK(nf90_get_var(ncid, varid, values, start=(/1,day_index/), &
            count=(/size(values,1),1/)), trim(context))
    end subroutine LOAD_VECTOR_DAY

    subroutine LOAD_GRID_DAY(ncid, varid, day_index, values, label)
        integer, intent(in) :: ncid, varid, day_index
        real, intent(out) :: values(:,:,:)
        character(len=*), intent(in) :: label
        character(len=160) :: context

        write(context,"(A,A,A,I0)") "read gridded ", trim(label), " day ", day_index
        call NC_INPUT_CHECK(nf90_get_var(ncid, varid, values, start=(/1,1,day_index/), &
            count=(/size(values,1),size(values,2),1/)), trim(context))
    end subroutine LOAD_GRID_DAY

    subroutine GET_HEAT_VECTOR_VALUE(varid, index_value, value, context)
        integer, intent(in) :: varid, index_value
        real, intent(out) :: value
        character(len=*), intent(in) :: context

        if (varid .eq. tair_varid) then
            value = tair_day(index_value,1)
        else if (varid .eq. vp_varid) then
            value = vp_day(index_value,1)
        else if (varid .eq. sw_varid) then
            value = sw_day(index_value,1)
        else if (varid .eq. lw_varid) then
            value = lw_day(index_value,1)
        else if (varid .eq. press_varid) then
            value = press_day(index_value,1)
        else if (varid .eq. wind_varid) then
            value = wind_day(index_value,1)
        else if (varid .eq. density_varid .and. allocated(density_day)) then
            value = density_day(index_value,1)
        else
            write(*,*) "NetCDF input error: internal unknown heat variable in ", trim(context)
            stop 1
        end if
        call CHECK_HEAT_VALUE(varid, value, context)
    end subroutine GET_HEAT_VECTOR_VALUE

    subroutine GET_HEAT_GRID_VALUE(varid, i, j, value, context)
        integer, intent(in) :: varid, i, j
        real, intent(out) :: value
        character(len=*), intent(in) :: context

        if (varid .eq. tair_varid) then
            value = tair_grid_day(i,j,1)
        else if (varid .eq. vp_varid) then
            value = vp_grid_day(i,j,1)
        else if (varid .eq. sw_varid) then
            value = sw_grid_day(i,j,1)
        else if (varid .eq. lw_varid) then
            value = lw_grid_day(i,j,1)
        else if (varid .eq. press_varid) then
            value = press_grid_day(i,j,1)
        else if (varid .eq. wind_varid) then
            value = wind_grid_day(i,j,1)
        else if (varid .eq. density_varid .and. allocated(density_grid_day)) then
            value = density_grid_day(i,j,1)
        else
            write(*,*) "NetCDF input error: internal unknown gridded heat variable in ", trim(context)
            stop 1
        end if
        call CHECK_HEAT_VALUE(varid, value, context)
    end subroutine GET_HEAT_GRID_VALUE

    subroutine CHECK_HEAT_VALUE(varid, value, context)
        integer, intent(in) :: varid
        real, intent(in) :: value
        character(len=*), intent(in) :: context
        real :: fill_value
        logical :: has_fill

        if (varid .eq. tair_varid) then
            fill_value = tair_fill; has_fill = tair_has_fill
        else if (varid .eq. vp_varid) then
            fill_value = vp_fill; has_fill = vp_has_fill
        else if (varid .eq. sw_varid) then
            fill_value = sw_fill; has_fill = sw_has_fill
        else if (varid .eq. lw_varid) then
            fill_value = lw_fill; has_fill = lw_has_fill
        else if (varid .eq. press_varid) then
            fill_value = press_fill; has_fill = press_has_fill
        else if (varid .eq. wind_varid) then
            fill_value = wind_fill; has_fill = wind_has_fill
        else
            fill_value = density_fill; has_fill = density_has_fill
        end if
        if (VALUE_IS_MISSING(value, fill_value, has_fill)) then
            write(*,*) "NetCDF input error: missing forcing value in ", trim(context)
            stop 1
        end if
    end subroutine CHECK_HEAT_VALUE

    logical function VALUE_IS_MISSING(value, fill_value, has_fill)
        real, intent(in) :: value, fill_value
        logical, intent(in) :: has_fill

        VALUE_IS_MISSING = .not. ieee_is_finite(value) .or. abs(value) .ge. 1.0e30
        if (has_fill) then
            if (ieee_is_finite(fill_value)) then
                VALUE_IS_MISSING = VALUE_IS_MISSING .or. &
                    abs(value - fill_value) .le. epsilon(1.0) * max(1.0, abs(fill_value))
            end if
        end if
    end function VALUE_IS_MISSING

    logical function FLOW_VALUE_IS_MISSING(value)
        real(kind=8), intent(in) :: value

        FLOW_VALUE_IS_MISSING = .not. ieee_is_finite(value) .or. abs(value) .ge. 1.0d30
        if (flow_has_fill) then
            if (ieee_is_finite(flow_fill)) then
                FLOW_VALUE_IS_MISSING = FLOW_VALUE_IS_MISSING .or. &
                    abs(value - real(flow_fill,kind=8)) .le. epsilon(1.0d0) * &
                    max(1.0d0, abs(real(flow_fill,kind=8)))
            end if
        end if
    end function FLOW_VALUE_IS_MISSING

    subroutine READ_FORCING_NETCDF()
        integer :: nc, ncell, no_heat, nr
        integer :: nreservoir, n1, n2
        integer :: flow_day_index, heat_day_index, local_day_index
        real :: q_cfs, q_unused

        call NC_INPUT_INIT()
        flow_day_index = flow_start_index + ndays - 1
        heat_day_index = heat_start_index + ndays - 1
        local_day_index = 0
        if (local_flow_enabled) local_day_index = local_start_index + ndays - 1
        call LOAD_FORCING_DAY(flow_day_index, heat_day_index, local_day_index)

        n1 = 1
        n2 = 2
        no_heat = 0
        do nr = 1, nreach
            do nc = 1, no_cells(nr)-1
                no_heat = no_heat + 1
                ncell = no_heat
                call READ_FLOW_CELL(flow_day_index, ncell, q_cfs, q_unused, Q_diff(no_heat), depth(no_heat), &
                    width(no_heat), u(no_heat), Q_local(no_heat))
                Q_out(no_heat) = q_cfs
                call APPLY_FLOW_LIMITS(no_heat, real(control_min_flow), real(control_min_depth))
                if (Q_diff(no_heat) .gt. 0) write(*,*) 'Q_diff is not equal to 0', no_heat, Q_diff(no_heat)
                if (u(no_heat) .lt. 0.01) u(no_heat) = 0.01
                call READ_HEAT_CELL(heat_day_index, no_heat)
                if (wind(no_heat) .lt. 1.0) wind(no_heat) = 1.0
                delta_n = ndelta(ncell)
                dt(no_heat) = dx(no_heat)/u(no_heat)
                if (dt(no_heat) .gt. dt_comp) write(45,*) &
                    'Travel time=',dt(no_heat), '> dt_comp at node -',no_heat
            end do

            no_heat = no_heat + 1
            call READ_HEAT_CELL(heat_day_index, no_heat)
            if (wind(no_heat) .lt. 1.0) wind(no_heat) = 1.0
            if (nr .eq. nreach .and. nc .eq. no_cells(nr)) then
                ncell = no_heat
                call READ_FLOW_CELL(flow_day_index, ncell, q_cfs, q_unused, Q_diff(no_heat), depth(no_heat), &
                    width(no_heat), u(no_heat), Q_local(no_heat))
                Q_out(no_heat) = q_cfs
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

        if (.not. control_file_mode .or. reservoir) then
            if (control_file_mode .and. LEN_TRIM(control_reservoir_data_file) .gt. 0) then
                call READ_RESERVOIR_NC_DAY(ndays)
            else
                res_storage(:,n2) = res_storage(:,n1)
                read(38,*)
                do nreservoir = 1, nres
                    read(38,*) res_storage(nreservoir, n1)
                end do

                read(39,*)
                do nreservoir = 1, nres
                    read(39,*) eh_withdraw_ratio(nreservoir, n1), eh_withdraw_ratio(nreservoir, n2)
                end do
            end if
        end if

        call Water_Balance
    end subroutine READ_FORCING_NETCDF

    subroutine READ_FLOW_CELL(day_index, cell_index, q_out, q_in_dummy, q_diff_out, depth_out, width_out, &
        velocity_out, q_local_out)
        integer, intent(in) :: day_index, cell_index
        real, intent(out) :: q_out, q_in_dummy, q_diff_out, depth_out, width_out, velocity_out, q_local_out
        integer :: flow_idx
        character(len=120) :: context
        real(kind=8) :: q_cms, q_for_hydraulics, depth_calc, width_calc, velocity_calc
        real :: q_vector_cfs, depth_vector, width_vector, velocity_vector
        real(kind=8), parameter :: cms_to_cfs = (1000.0d0/25.4d0/12.0d0)**3
        real, parameter :: vector_cms_to_cfs = 1.0/0.028316847

        if (grid_flow_schema) then
            write(context,"(A,I0,A,I0)") "read gridded flow day=",day_index," cell=",cell_index
            q_cms = flow_grid_day(flow_grid_i(cell_index), flow_grid_j(cell_index), 1)
        else
            if (flow_cell_index(cell_index) .eq. 0) &
                flow_cell_index(cell_index) = FIND_FLOW_INDEX(hru_id(cell_index))
            flow_idx = flow_cell_index(cell_index)
            write(context,"(A,I0,A,I0,A,I0)") "read flow value day=",day_index, &
                " cell=",cell_index," flow_idx=",flow_idx
            q_cms = flow_day_values(flow_idx,1)
        end if
        if (FLOW_VALUE_IS_MISSING(q_cms)) then
            write(*,*) "NetCDF input error: missing streamflow value in ", trim(context)
            stop 1
        end if
        ! Apply the conversion precision associated with each supported forcing schema.
        if (grid_flow_schema) then
            if (trim(control_flow_units) .eq. "cfs") then
                q_for_hydraulics = q_cms
            else
                q_for_hydraulics = q_cms * cms_to_cfs
            end if
            if (q_for_hydraulics .gt. 0.0d0) then
                depth_calc = control_a_d * (q_for_hydraulics**control_b_d)
                width_calc = control_a_w * (q_for_hydraulics**control_b_w)
                velocity_calc = q_for_hydraulics/(depth_calc*width_calc)
                if (velocity_calc .lt. control_min_velocity) velocity_calc = control_min_velocity
            else
                depth_calc = 0.0d0
                width_calc = 0.0d0
                velocity_calc = control_min_velocity
            end if
            q_out = ROUND1_DOUBLE(q_for_hydraulics)
            depth_out = ROUND1_DOUBLE(depth_calc)
            width_out = ROUND1_DOUBLE(width_calc)
            velocity_out = ROUND2_DOUBLE(velocity_calc)
        else
            if (trim(control_flow_units) .eq. "cfs") then
                q_vector_cfs = real(q_cms)
            else
                q_vector_cfs = real(q_cms) * vector_cms_to_cfs
            end if
            if (q_vector_cfs .lt. real(control_min_flow)) q_vector_cfs = real(control_min_flow)
            depth_vector = real(control_a_d) * (q_vector_cfs**real(control_b_d))
            width_vector = real(control_a_w) * (q_vector_cfs**real(control_b_w))
            velocity_vector = q_vector_cfs/(depth_vector*width_vector)
            if (velocity_vector .lt. real(control_min_velocity)) velocity_vector = real(control_min_velocity)
            q_out = ROUND1(q_vector_cfs)
            depth_out = ROUND1(depth_vector)
            width_out = ROUND1(width_vector)
            velocity_out = ROUND2(velocity_vector)
        end if

        q_in_dummy = q_out
        q_diff_out = 0.0
        if (local_flow_enabled) then
            call READ_LOCAL_FLOW_CELL(cell_index, q_local_out)
        else
            q_local_out = 0.0
        end if
    end subroutine READ_FLOW_CELL

    subroutine READ_LOCAL_FLOW_CELL(cell_index, q_local_out)
        integer, intent(in) :: cell_index
        real, intent(out) :: q_local_out
        integer :: local_i, local_j, area_i, area_j
        real :: runoff_value, baseflow_value, area_value
        real(8) :: q_local_cfs
        real(8), parameter :: mm_to_m = 1.0e-3_8
        real(8), parameter :: cubic_m_to_cubic_ft = (1.0_8/0.3048_8)**3
        real(8), parameter :: seconds_per_day = 86400.0_8

        local_i = local_grid_i(cell_index)
        local_j = local_grid_j(cell_index)
        area_i = area_grid_i(cell_index)
        area_j = area_grid_j(cell_index)
        runoff_value = runoff_grid_day(local_i,local_j,1)
        baseflow_value = baseflow_grid_day(local_i,local_j,1)
        area_value = area_grid(area_i,area_j)

        if (VALUE_IS_MISSING(runoff_value, runoff_fill, runoff_has_fill)) then
            write(*,*) "NetCDF input error: missing local runoff for topology cell ", cell_index, "."
            stop 1
        end if
        if (VALUE_IS_MISSING(baseflow_value, baseflow_fill, baseflow_has_fill)) then
            write(*,*) "NetCDF input error: missing local baseflow for topology cell ", cell_index, "."
            stop 1
        end if

        q_local_cfs = (real(runoff_value,8) + real(baseflow_value,8)) * real(area_value,8) * &
            mm_to_m * cubic_m_to_cubic_ft / seconds_per_day
        q_local_out = ROUND1(real(q_local_cfs))
    end subroutine READ_LOCAL_FLOW_CELL



    subroutine READ_REMAP_HEAT_CELL(heat_idx, cell_index, context)
        integer, intent(in) :: heat_idx, cell_index
        character(len=*), intent(in) :: context
        real :: tsa_k, rh_pct, fsds, flds, pbot, u10
        real, parameter :: radiation_factor = 2.388e-4

        call READ_REMAP_RAW(tair_varid, heat_idx, tsa_k, context)
        call READ_REMAP_RAW(vp_varid, heat_idx, rh_pct, context)
        call READ_REMAP_RAW(sw_varid, heat_idx, fsds, context)
        call READ_REMAP_RAW(lw_varid, heat_idx, flds, context)
        call READ_REMAP_RAW(press_varid, heat_idx, pbot, context)
        call READ_REMAP_RAW(wind_varid, heat_idx, u10, context)

        dbt(cell_index) = ROUND1(tsa_k - 273.15)
        ea(cell_index) = ROUND1((rh_pct/100.0) * SATURATION_VAPOR_PRESSURE_MB(tsa_k))
        Q_ns(cell_index) = ROUND4(fsds * radiation_factor)
        Q_na(cell_index) = ROUND4(flds * radiation_factor)
        rho = 1.225
        press(cell_index) = ROUND1(pbot/100.0)
        wind(cell_index) = ROUND1(u10)
    end subroutine READ_REMAP_HEAT_CELL

    subroutine READ_REMAP_RAW(varid, poly_index, value, context)
        integer, intent(in) :: varid, poly_index
        character(len=*), intent(in) :: context
        real, intent(out) :: value
        integer :: k, k0, k1
        real :: raw_values(1)
        real(8) :: total

        if (poly_index .lt. 1 .or. poly_index .gt. size(remap_overlaps)) then
            write(*,*) 'NetCDF input error: remap index out of range in ', trim(context), ': ', poly_index
            stop 1
        end if

        total = 0.0_8
        k0 = remap_start(poly_index)
        k1 = k0 + remap_overlaps(poly_index) - 1
        do k = k0, k1
            call GET_HEAT_GRID_VALUE(varid, remap_i_index(k), remap_j_index(k), raw_values(1), context)
            total = total + remap_weight(k) * real(raw_values(1), kind=8)
        end do
        value = real(total)
    end subroutine READ_REMAP_RAW

    subroutine READ_HEAT_CELL(day_index, cell_index)
        integer, intent(in) :: day_index, cell_index
        integer :: heat_idx
        character(len=120) :: context

        if (grid_heat_schema) then
            write(context,"(A,I0,A,I0)") "read gridded heat day=",day_index," cell=",cell_index
            call READ_GRID_HEAT_CELL(cell_index, trim(context))
            return
        end if

        heat_idx = heat_cell_index(cell_index)
        write(context,"(A,I0,A,I0,A,I0)") "read heat value day=",day_index, &
            " cell=",cell_index," heat_idx=",heat_idx
        if (heat_remap_mode) then
            call READ_REMAP_HEAT_CELL(heat_idx, cell_index, trim(context))
        else if (raw_heat_schema) then
            call READ_RAW_HEAT_CELL(heat_idx, cell_index, trim(context))
        else
            call READ_NC_REAL(tair_varid, heat_idx, dbt(cell_index), 1, trim(context))
            call READ_NC_REAL(vp_varid, heat_idx, ea(cell_index), 1, trim(context))
            call READ_NC_REAL(sw_varid, heat_idx, Q_ns(cell_index), 4, trim(context))
            call READ_NC_REAL(lw_varid, heat_idx, Q_na(cell_index), 4, trim(context))
            rho = 1.225
            call READ_NC_REAL(press_varid, heat_idx, press(cell_index), 1, trim(context))
            call READ_NC_REAL(wind_varid, heat_idx, wind(cell_index), 1, trim(context))
        end if
    end subroutine READ_HEAT_CELL

    subroutine READ_GRID_HEAT_CELL(cell_index, context)
        integer, intent(in) :: cell_index
        character(len=*), intent(in) :: context
        real :: tair_value, vapor_value, shortwave_value, longwave_value
        real :: pressure_value, wind_value, density_value
        real(kind=8), parameter :: radiation_factor = 2.388d-4

        call READ_GRID_RAW(tair_varid, cell_index, tair_value, context)
        call READ_GRID_RAW(vp_varid, cell_index, vapor_value, context)
        call READ_GRID_RAW(sw_varid, cell_index, shortwave_value, context)
        call READ_GRID_RAW(lw_varid, cell_index, longwave_value, context)
        call READ_GRID_RAW(press_varid, cell_index, pressure_value, context)
        call READ_GRID_RAW(wind_varid, cell_index, wind_value, context)

        if (raw_heat_schema) then
            dbt(cell_index) = ROUND1(tair_value - 273.15)
            ea(cell_index) = ROUND1((vapor_value/100.0) * SATURATION_VAPOR_PRESSURE_MB(tair_value))
            press(cell_index) = ROUND1(pressure_value/100.0)
            rho = 1.225
        else
            dbt(cell_index) = ROUND1(tair_value)
            ea(cell_index) = ROUND1_DOUBLE(real(vapor_value,kind=8)*10.0d0)
            press(cell_index) = ROUND1_DOUBLE(real(pressure_value,kind=8)*10.0d0)
            if (density_varid .ge. 0) then
                call READ_GRID_RAW(density_varid, cell_index, density_value, context)
                rho = density_value
            else
                rho = 1.225
            end if
        end if
        Q_ns(cell_index) = ROUND4_DOUBLE(real(shortwave_value,kind=8) * radiation_factor)
        Q_na(cell_index) = ROUND4_DOUBLE(real(longwave_value,kind=8) * radiation_factor)
        wind(cell_index) = ROUND1(wind_value)
    end subroutine READ_GRID_HEAT_CELL

    subroutine READ_GRID_RAW(varid, cell_index, value, context)
        integer, intent(in) :: varid, cell_index
        character(len=*), intent(in) :: context
        real, intent(out) :: value

        call GET_HEAT_GRID_VALUE(varid, heat_grid_i(cell_index), heat_grid_j(cell_index), value, context)
    end subroutine READ_GRID_RAW


    subroutine READ_RAW_HEAT_CELL(heat_idx, cell_index, context)
        integer, intent(in) :: heat_idx, cell_index
        character(len=*), intent(in) :: context
        real :: tsa_k, rh_pct, fsds, flds, pbot, u10
        real, parameter :: radiation_factor = 2.388e-4

        call READ_NC_RAW(tair_varid, heat_idx, tsa_k, context)
        call READ_NC_RAW(vp_varid, heat_idx, rh_pct, context)
        call READ_NC_RAW(sw_varid, heat_idx, fsds, context)
        call READ_NC_RAW(lw_varid, heat_idx, flds, context)
        call READ_NC_RAW(press_varid, heat_idx, pbot, context)
        call READ_NC_RAW(wind_varid, heat_idx, u10, context)

        dbt(cell_index) = ROUND1(tsa_k - 273.15)
        ea(cell_index) = ROUND1((rh_pct/100.0) * SATURATION_VAPOR_PRESSURE_MB(tsa_k))
        Q_ns(cell_index) = ROUND4(fsds * radiation_factor)
        Q_na(cell_index) = ROUND4(flds * radiation_factor)
        rho = 1.225
        press(cell_index) = ROUND1(pbot/100.0)
        wind(cell_index) = ROUND1(u10)
    end subroutine READ_RAW_HEAT_CELL

    subroutine READ_NC_RAW(varid, id_index, value, context)
        integer, intent(in) :: varid, id_index
        character(len=*), intent(in) :: context
        real, intent(out) :: value

        call GET_HEAT_VECTOR_VALUE(varid, id_index, value, context)
    end subroutine READ_NC_RAW

    real function SATURATION_VAPOR_PRESSURE_MB(temp_k)
        real, intent(in) :: temp_k
        real :: temp_c
        temp_c = temp_k - 273.15
        SATURATION_VAPOR_PRESSURE_MB = 6.112 * exp(17.67 * temp_c / (temp_c + 243.5))
    end function SATURATION_VAPOR_PRESSURE_MB

    subroutine READ_NC_REAL(varid, id_index, value, round_digits, context)
        integer, intent(in) :: varid, id_index, round_digits
        character(len=*), intent(in) :: context
        real, intent(out) :: value
        real :: raw_value

        call GET_HEAT_VECTOR_VALUE(varid, id_index, raw_value, context)
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
            z_temp = real(control_a_d) * (min_flow**real(control_b_d))
            w_temp = real(control_a_w) * (min_flow**real(control_b_w))
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
            if (flow_ids(i) .eq. int(id, kind=8)) then
                FIND_FLOW_INDEX = i
                return
            end if
        end do
        write(*,*) 'Flow reachID not found in netCDF input: ',id
        stop 1
    end function FIND_FLOW_INDEX

    subroutine NC_INPUT_CLOSE()
        if (.not. nc_forcing_open) return

        if (heat_remap_ncid .ge. 0) call NC_INPUT_CHECK(nf90_close(heat_remap_ncid), "close heat remap file")
        if (area_ncid .ge. 0) call NC_INPUT_CHECK(nf90_close(area_ncid), "close grid area file")
        if (local_flow_ncid .ge. 0) call NC_INPUT_CHECK(nf90_close(local_flow_ncid), "close local flow file")
        if (heat_ncid .ge. 0) call NC_INPUT_CHECK(nf90_close(heat_ncid), "close heat file")
        if (flow_ncid .ge. 0) call NC_INPUT_CHECK(nf90_close(flow_ncid), "close flow file")

        if (allocated(flow_ids)) deallocate(flow_ids)
        if (allocated(heat_ids)) deallocate(heat_ids)
        if (allocated(flow_cell_index)) deallocate(flow_cell_index)
        if (allocated(heat_cell_index)) deallocate(heat_cell_index)
        if (allocated(flow_grid_i)) deallocate(flow_grid_i)
        if (allocated(flow_grid_j)) deallocate(flow_grid_j)
        if (allocated(heat_grid_i)) deallocate(heat_grid_i)
        if (allocated(heat_grid_j)) deallocate(heat_grid_j)
        if (allocated(flow_time_days)) deallocate(flow_time_days)
        if (allocated(heat_time_days)) deallocate(heat_time_days)
        if (allocated(local_time_days)) deallocate(local_time_days)
        if (allocated(local_grid_i)) deallocate(local_grid_i)
        if (allocated(local_grid_j)) deallocate(local_grid_j)
        if (allocated(area_grid_i)) deallocate(area_grid_i)
        if (allocated(area_grid_j)) deallocate(area_grid_j)
        if (allocated(runoff_grid_day)) deallocate(runoff_grid_day)
        if (allocated(baseflow_grid_day)) deallocate(baseflow_grid_day)
        if (allocated(area_grid)) deallocate(area_grid)
        if (allocated(remap_overlaps)) deallocate(remap_overlaps)
        if (allocated(remap_i_index)) deallocate(remap_i_index)
        if (allocated(remap_j_index)) deallocate(remap_j_index)
        if (allocated(remap_start)) deallocate(remap_start)
        if (allocated(remap_weight)) deallocate(remap_weight)
        if (allocated(flow_day_values)) deallocate(flow_day_values)
        if (allocated(flow_grid_day)) deallocate(flow_grid_day)
        if (allocated(tair_day)) deallocate(tair_day)
        if (allocated(vp_day)) deallocate(vp_day)
        if (allocated(sw_day)) deallocate(sw_day)
        if (allocated(lw_day)) deallocate(lw_day)
        if (allocated(press_day)) deallocate(press_day)
        if (allocated(wind_day)) deallocate(wind_day)
        if (allocated(density_day)) deallocate(density_day)
        if (allocated(tair_grid_day)) deallocate(tair_grid_day)
        if (allocated(vp_grid_day)) deallocate(vp_grid_day)
        if (allocated(sw_grid_day)) deallocate(sw_grid_day)
        if (allocated(lw_grid_day)) deallocate(lw_grid_day)
        if (allocated(press_grid_day)) deallocate(press_grid_day)
        if (allocated(wind_grid_day)) deallocate(wind_grid_day)
        if (allocated(density_grid_day)) deallocate(density_grid_day)

        flow_ncid = -1
        heat_ncid = -1
        local_flow_ncid = -1
        area_ncid = -1
        heat_remap_ncid = -1
        flow_varid = -1
        flow_id_varid = -1
        heat_id_varid = -1
        tair_varid = -1
        vp_varid = -1
        sw_varid = -1
        lw_varid = -1
        press_varid = -1
        wind_varid = -1
        density_varid = -1
        local_runoff_varid = -1
        local_baseflow_varid = -1
        area_varid = -1
        cached_flow_day = -1
        cached_heat_day = -1
        cached_local_day = -1
        local_flow_enabled = .false.
        nc_forcing_open = .false.
    end subroutine NC_INPUT_CLOSE

    real function ROUND1(value)
        real, intent(in) :: value
        character(len=32) :: buffer
        write(buffer,'(f20.1)') value
        read(buffer,*) ROUND1
    end function ROUND1

    real function ROUND2(value)
        real, intent(in) :: value
        character(len=32) :: buffer
        write(buffer,'(f20.2)') value
        read(buffer,*) ROUND2
    end function ROUND2

    real function ROUND1_DOUBLE(value)
        real(kind=8), intent(in) :: value
        character(len=32) :: buffer
        write(buffer,'(f20.1)') value
        read(buffer,*) ROUND1_DOUBLE
    end function ROUND1_DOUBLE

    real function ROUND2_DOUBLE(value)
        real(kind=8), intent(in) :: value
        character(len=32) :: buffer
        write(buffer,'(f20.2)') value
        read(buffer,*) ROUND2_DOUBLE
    end function ROUND2_DOUBLE

    real function ROUND4_DOUBLE(value)
        real(kind=8), intent(in) :: value
        character(len=32) :: buffer
        write(buffer,'(f20.4)') value
        read(buffer,*) ROUND4_DOUBLE
    end function ROUND4_DOUBLE

    real function ROUND4(value)
        real, intent(in) :: value
        character(len=32) :: buffer
        write(buffer,'(f20.4)') value
        read(buffer,*) ROUND4
    end function ROUND4

end module Netcdf_Input
