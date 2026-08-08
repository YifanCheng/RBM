SUBROUTINE READ_CONTROL(control_file)
    use Block_Control
    implicit none
    !
    character(len=*), intent(in) :: control_file
    character(len=512) :: line
    character(len=200) :: name,value
    integer :: ios,ibeg,iend,icomment
    !
    open(unit=91,file=TRIM(control_file),status="old",action="read")
    do
        read(91,"(A)",iostat=ios) line
        if (ios .ne. 0) exit
        line = ADJUSTL(line)
        if (LEN_TRIM(line) .eq. 0) cycle
        if (line(1:1) .eq. "!") cycle
        ibeg = INDEX(line,"<")
        iend = INDEX(line,">")
        if (ibeg .eq. 0 .or. iend .eq. 0 .or. iend .le. ibeg) cycle
        icomment = INDEX(line,"!")
        if (icomment .eq. 0) icomment = LEN_TRIM(line)+1
        name = ADJUSTL(line(ibeg+1:iend-1))
        value = ADJUSTL(line(iend+1:icomment-1))
        call SET_CONTROL_VALUE(TRIM(name),TRIM(value))
    end do
    close(91)
    call CHECK_CONTROL(control_file)
    control_file_mode = .TRUE.
END SUBROUTINE READ_CONTROL

SUBROUTINE SET_CONTROL_VALUE(name,value)
    use Block_Control
    implicit none
    !
    character(len=*), intent(in) :: name,value
    !
    logical :: CONTROL_LOGICAL
    !
    select case (TRIM(name))
    case ("network_file")
        control_network_file = TRIM(value)
    case ("topology_file")
        control_topology_file = TRIM(value)
    case ("flow_file")
        control_flow_file = TRIM(value)
    case ("heat_file")
        control_heat_file = TRIM(value)
    case ("heat_remap_file")
        control_heat_remap_file = TRIM(value)
    case ("local_flow_file")
        control_local_flow_file = TRIM(value)
    case ("area_file")
        control_area_file = TRIM(value)
    case ("reservoir_data_file")
        control_reservoir_data_file = TRIM(value)
    case ("reservoir_file")
        control_reservoir_file = TRIM(value)
    case ("reservoir_storage_file")
        control_reservoir_storage_file = TRIM(value)
    case ("reservoir_sel_withdraw_file")
        control_reservoir_sel_withdraw_file = TRIM(value)
    case ("output_prefix")
        control_output_prefix = TRIM(value)
    case ("start_date")
        control_start_date = TRIM(value)
    case ("end_date")
        control_end_date = TRIM(value)
    case ("source")
        control_source = CONTROL_LOGICAL(value)
    case ("reservoir")
        control_reservoir = CONTROL_LOGICAL(value)
    case ("nres")
        read(value,*) control_nres
    case ("select_withdraw")
        control_select_withdraw = CONTROL_LOGICAL(value)
    case ("verbose")
        control_verbose = CONTROL_LOGICAL(value)
    case ("input_format")
        control_input_format = TRIM(value)
    case ("output_format")
        control_output_format = TRIM(value)
    case ("flow_var")
        control_flow_var = TRIM(value)
    case ("flow_id_var")
        control_flow_id_var = TRIM(value)
    case ("heat_id_var")
        control_heat_id_var = TRIM(value)
    case ("heat_tair_var")
        control_heat_tair_var = TRIM(value)
    case ("heat_vp_var")
        control_heat_vp_var = TRIM(value)
    case ("heat_sw_var")
        control_heat_sw_var = TRIM(value)
    case ("heat_lw_var")
        control_heat_lw_var = TRIM(value)
    case ("heat_press_var")
        control_heat_press_var = TRIM(value)
    case ("heat_wind_var")
        control_heat_wind_var = TRIM(value)
    case ("heat_density_var")
        control_heat_density_var = TRIM(value)
    case ("local_runoff_var")
        control_local_runoff_var = TRIM(value)
    case ("local_baseflow_var")
        control_local_baseflow_var = TRIM(value)
    case ("area_var")
        control_area_var = TRIM(value)
    case ("flow_units")
        control_flow_units = TRIM(value)
    case ("a_d")
        call READ_CONTROL_REAL(name,value,control_a_d)
    case ("b_d")
        call READ_CONTROL_REAL(name,value,control_b_d)
    case ("a_w")
        call READ_CONTROL_REAL(name,value,control_a_w)
    case ("b_w")
        call READ_CONTROL_REAL(name,value,control_b_w)
    case ("min_flow")
        call READ_CONTROL_REAL(name,value,control_min_flow)
    case ("min_velocity")
        call READ_CONTROL_REAL(name,value,control_min_velocity)
    case ("min_depth")
        call READ_CONTROL_REAL(name,value,control_min_depth)
    case default
        write(*,*) "Ignoring unknown control variable: ",TRIM(name)
    end select
END SUBROUTINE SET_CONTROL_VALUE

SUBROUTINE READ_CONTROL_REAL(name,value,result)
    implicit none
    character(len=*), intent(in) :: name,value
    real(8), intent(out) :: result
    integer :: ios

    read(value,*,iostat=ios) result
    if (ios .ne. 0) then
        write(*,*) "Invalid numeric control value for <",TRIM(name),">: '",TRIM(value),"'"
        stop 1
    end if
END SUBROUTINE READ_CONTROL_REAL


LOGICAL FUNCTION CONTROL_LOGICAL(value)
    implicit none
    !
    character(len=*), intent(in) :: value
    character(len=200) :: text
    !
    text = ADJUSTL(value)
    CONTROL_LOGICAL = .FALSE.
    if (LEN_TRIM(text) .eq. 0) return
    if (text(1:1) .eq. "T" .or. text(1:1) .eq. "t" .or. &
        text(1:1) .eq. "Y" .or. text(1:1) .eq. "y" .or. &
        text(1:1) .eq. "1") CONTROL_LOGICAL = .TRUE.
END FUNCTION CONTROL_LOGICAL

SUBROUTINE CHECK_CONTROL(control_file)
    use Block_Control
    implicit none
    !
    character(len=*), intent(in) :: control_file
    logical :: VALID_DATE
    !
    ! <input_format> must be a supported value. Checked first so that the
    ! forcing-file diagnostics below can be tailored to the selected mode.
    if (TRIM(control_input_format) .ne. "ascii" .and. &
        TRIM(control_input_format) .ne. "netcdf") then
        write(*,*) "Invalid <input_format> in ",TRIM(control_file),": '", &
            TRIM(control_input_format),"' (must be 'ascii' or 'netcdf')"
        stop 1
    end if
    if (LEN_TRIM(control_output_format) .eq. 0) then
        control_output_format = control_input_format
    end if
    if (TRIM(control_flow_units) .ne. "m3/s" .and. &
        TRIM(control_flow_units) .ne. "cfs") then
        write(*,*) "Invalid <flow_units> in ",TRIM(control_file),": '", &
            TRIM(control_flow_units),"' (must be 'm3/s' or 'cfs')"
        stop 1
    end if
    if (control_a_d .le. 0.0d0 .or. control_a_w .le. 0.0d0 .or. &
        control_b_d .lt. 0.0d0 .or. control_b_w .lt. 0.0d0) then
        write(*,*) "Invalid hydraulic coefficients in ",TRIM(control_file), &
            ": a_d/a_w must be positive and b_d/b_w must be nonnegative."
        stop 1
    end if
    if (control_min_flow .lt. 0.0d0 .or. control_min_velocity .le. 0.0d0 .or. &
        control_min_depth .le. 0.0d0) then
        write(*,*) "Invalid hydraulic limits in ",TRIM(control_file), &
            ": min_flow must be nonnegative; min_velocity/min_depth must be positive."
        stop 1
    end if
    if (TRIM(control_output_format) .ne. "ascii" .and. &
        TRIM(control_output_format) .ne. "netcdf" .and. &
        TRIM(control_output_format) .ne. "both") then
        write(*,*) "Invalid <output_format> in ",TRIM(control_file),": '", &
            TRIM(control_output_format),"' (must be 'ascii', 'netcdf', or 'both')"
        stop 1
    end if
    control_write_ascii = (TRIM(control_output_format) .eq. "ascii" .or. &
        TRIM(control_output_format) .eq. "both")
    control_write_netcdf = (TRIM(control_output_format) .eq. "netcdf" .or. &
        TRIM(control_output_format) .eq. "both")
    !
    ! Required keys common to every input format.
    if (LEN_TRIM(control_network_file) .eq. 0 .and. LEN_TRIM(control_topology_file) .eq. 0) then
        call MISSING_CONTROL(control_file,"network_file or topology_file")
    end if
    if (LEN_TRIM(control_network_file) .gt. 0 .and. LEN_TRIM(control_topology_file) .gt. 0) then
        write(*,*) "Invalid control in ",TRIM(control_file), &
            ": provide either network_file or topology_file, not both."
        stop 1
    end if
    if (LEN_TRIM(control_output_prefix) .eq. 0) call MISSING_CONTROL(control_file,"output_prefix")
    if (LEN_TRIM(control_start_date) .eq. 0) call MISSING_CONTROL(control_file,"start_date")
    if (LEN_TRIM(control_end_date) .eq. 0) call MISSING_CONTROL(control_file,"end_date")
    !
    ! Forcing inputs are required in both modes; only their meaning differs.
    if (TRIM(control_input_format) .eq. "netcdf") then
        if (LEN_TRIM(control_flow_file) .eq. 0) &
            call MISSING_CONTROL(control_file,"flow_file (netCDF flow forcing path)")
        if (LEN_TRIM(control_heat_file) .eq. 0) &
            call MISSING_CONTROL(control_file,"heat_file (netCDF heat forcing path)")
        if (LEN_TRIM(control_local_flow_file) .gt. 0 .and. LEN_TRIM(control_area_file) .eq. 0) &
            call MISSING_CONTROL(control_file,"area_file (required with local_flow_file)")
        if (LEN_TRIM(control_area_file) .gt. 0 .and. LEN_TRIM(control_local_flow_file) .eq. 0) &
            call MISSING_CONTROL(control_file,"local_flow_file (required with area_file)")
    else
        if (LEN_TRIM(control_flow_file) .eq. 0) &
            call MISSING_CONTROL(control_file,"flow_file (ASCII flow forcing path)")
        if (LEN_TRIM(control_heat_file) .eq. 0) &
            call MISSING_CONTROL(control_file,"heat_file (ASCII heat forcing path)")
        if (LEN_TRIM(control_local_flow_file) .gt. 0 .or. LEN_TRIM(control_area_file) .gt. 0) then
            write(*,*) "Invalid control in ",TRIM(control_file), &
                ": local_flow_file and area_file apply only to netCDF input."
            stop 1
        end if
    end if
    !
    ! Reservoir runs accept either one netCDF file or the three ASCII files.
    if (control_reservoir) then
        if (control_nres .le. 0) then
            write(*,*) "Invalid control in ",TRIM(control_file), &
                ": nres must be greater than zero when reservoir is True."
            stop 1
        end if
        if (LEN_TRIM(control_reservoir_data_file) .gt. 0) then
            if (LEN_TRIM(control_reservoir_file) .gt. 0 .or. &
                LEN_TRIM(control_reservoir_storage_file) .gt. 0 .or. &
                LEN_TRIM(control_reservoir_sel_withdraw_file) .gt. 0) then
                write(*,*) "Invalid control in ",TRIM(control_file), &
                    ": provide reservoir_data_file or the three ASCII reservoir files, not both."
                stop 1
            end if
        else
            if (LEN_TRIM(control_reservoir_file) .eq. 0) call MISSING_CONTROL(control_file,"reservoir_file")
            if (LEN_TRIM(control_reservoir_storage_file) .eq. 0) &
                call MISSING_CONTROL(control_file,"reservoir_storage_file")
            if (LEN_TRIM(control_reservoir_sel_withdraw_file) .eq. 0) &
                call MISSING_CONTROL(control_file,"reservoir_sel_withdraw_file")
        end if
    else
        if (control_nres .ne. 0) then
            write(*,*) "Invalid control in ",TRIM(control_file), &
                ": nres must be zero when reservoir is False."
            stop 1
        end if
        if (control_select_withdraw) then
            write(*,*) "Invalid control in ",TRIM(control_file), &
                ": select_withdraw requires reservoir=True."
            stop 1
        end if
    end if
    !
    ! Dates must be 8-digit YYYYMMDD (read downstream as (i4,2i2)).
    if (.not. VALID_DATE(control_start_date)) &
        call BAD_DATE(control_file,"start_date",control_start_date)
    if (.not. VALID_DATE(control_end_date)) &
        call BAD_DATE(control_file,"end_date",control_end_date)
    !
    ! end_date must not precede start_date. Both are 8-char YYYYMMDD, so a
    ! lexical comparison is also a chronological one.
    if (control_end_date .lt. control_start_date) then
        write(*,*) "Invalid date range in ",TRIM(control_file),": end_date ", &
            TRIM(control_end_date)," is earlier than start_date ",TRIM(control_start_date)
        stop 1
    end if
END SUBROUTINE CHECK_CONTROL

LOGICAL FUNCTION VALID_DATE(value)
    implicit none
    !
    character(len=*), intent(in) :: value
    integer :: i,ic,year,month,day,ios
    integer :: days_in_month(12)
    logical :: leap_year
    !
    VALID_DATE = .FALSE.
    if (LEN_TRIM(value) .ne. 8) return
    do i = 1,8
        ic = ICHAR(value(i:i))
        if (ic .lt. ICHAR("0") .or. ic .gt. ICHAR("9")) return
    end do
    read(value,"(i4,2i2)",iostat=ios) year,month,day
    if (ios .ne. 0 .or. year .lt. 1) return
    if (month .lt. 1 .or. month .gt. 12) return
    days_in_month = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    leap_year = (mod(year,400) .eq. 0 .or. &
        (mod(year,4) .eq. 0 .and. mod(year,100) .ne. 0))
    if (leap_year) days_in_month(2) = 29
    if (day .lt. 1 .or. day .gt. days_in_month(month)) return
    VALID_DATE = .TRUE.
END FUNCTION VALID_DATE

SUBROUTINE MISSING_CONTROL(control_file,name)
    implicit none
    !
    character(len=*), intent(in) :: control_file,name
    !
    write(*,*) "Missing required control variable in ",TRIM(control_file),": ",TRIM(name)
    stop 1
END SUBROUTINE MISSING_CONTROL

SUBROUTINE BAD_DATE(control_file,name,value)
    implicit none
    !
    character(len=*), intent(in) :: control_file,name,value
    !
    write(*,*) "Invalid ",TRIM(name)," in ",TRIM(control_file),": '",TRIM(value), &
        "' (expected a valid calendar date in YYYYMMDD format)"
    stop 1
END SUBROUTINE BAD_DATE
