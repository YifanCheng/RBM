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
    case ("flow_file")
        control_flow_file = TRIM(value)
    case ("heat_file")
        control_heat_file = TRIM(value)
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
    case ("input_format")
        control_input_format = TRIM(value)
    case default
        write(*,*) "Ignoring unknown control variable: ",TRIM(name)
    end select
END SUBROUTINE SET_CONTROL_VALUE

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
    !
    if (LEN_TRIM(control_network_file) .eq. 0) call MISSING_CONTROL(control_file,"network_file")
    if (LEN_TRIM(control_flow_file) .eq. 0) call MISSING_CONTROL(control_file,"flow_file")
    if (LEN_TRIM(control_heat_file) .eq. 0) call MISSING_CONTROL(control_file,"heat_file")
    if (LEN_TRIM(control_reservoir_file) .eq. 0) call MISSING_CONTROL(control_file,"reservoir_file")
    if (LEN_TRIM(control_reservoir_storage_file) .eq. 0) &
        call MISSING_CONTROL(control_file,"reservoir_storage_file")
    if (LEN_TRIM(control_reservoir_sel_withdraw_file) .eq. 0) &
        call MISSING_CONTROL(control_file,"reservoir_sel_withdraw_file")
    if (LEN_TRIM(control_output_prefix) .eq. 0) call MISSING_CONTROL(control_file,"output_prefix")
    if (LEN_TRIM(control_start_date) .eq. 0) call MISSING_CONTROL(control_file,"start_date")
    if (LEN_TRIM(control_end_date) .eq. 0) call MISSING_CONTROL(control_file,"end_date")
    if (TRIM(control_input_format) .ne. "ascii") then
        write(*,*) "Only <input_format> ascii is supported in this milestone."
        stop
    end if
END SUBROUTINE CHECK_CONTROL

SUBROUTINE MISSING_CONTROL(control_file,name)
    implicit none
    !
    character(len=*), intent(in) :: control_file,name
    !
    write(*,*) "Missing required control variable in ",TRIM(control_file),": ",TRIM(name)
    stop
END SUBROUTINE MISSING_CONTROL
