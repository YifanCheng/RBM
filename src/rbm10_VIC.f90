!
PROGRAM RBM10_VIC
    !
    !     Dynamic river basin model for simulating water quality in
    !     branching river systems with freely-flowing river segments.
    !
    !     This version uses Reverse Particle Tracking in the Lagrangian
    !     mode and Lagrangian interpolation in the Eulerian mode.
    !
    !     This version of the model software has the following limited features:
    !
    !     1. Hydrologic and meteorologic forcings are from a direct access file
    !        prepared by the routing program.  Hydraulic parameters are estimated
    !        by the routing program using Leopold relationships describing depth
    !        and speed as a function of flow.
    !
    !     2. Output is written to Unit 20 (specified in the command line for executing
    !        RBM) and includes the simulated temperatures for the first computational cell
    !        in each segment in the same order as that of the *.network file and at the
    !        same time step.
    !
    !     Topology and routing is set up to be consistent with output
    !     from the Variable Infiltration Capacity (VIC) model developed by the
    !     Land Surface Hydrology Group at the University of Washington.
    !     Model details are described in
    !
    !     Yearsley, J. (2012), A grid-based approach for simulating stream temperature,
    !     Water Resour. Res., 48, W03506, doi:10.1029/2011WR011515
    !
    !     For additional information contact:
    !
    !     John Yearsley
    !     Land Surface Hydrology Group
    !     Dept. of Civil and Environmental Engineering
    !     Box 352700
    !     University of Washington
    !     Seattle, Washington
    !     98195-2700
    !
    !use BGIN
    !use SYSTM
    !
    use Block_Control
    implicit none
    !
    !
    character (len=200 ):: inPrefix
    character (len=200 ):: outPrefix
    character (len=200 ):: flow_file
    character (len=200 ):: heat_file
    character (len=200 ):: net_file
    character (len=200 ):: param_file
    character (len=200 ):: temp_file
    character (len=200 ):: res_file
    character (len=200 ):: spatial_file
    character (len=200 ):: reservoir_file
    character (len=200 ):: reservoir_storage_file
    character (len=200 ):: reservoir_sel_withdraw_file
    character (len=200 ):: control_file
    character (len=8)   :: start_data,end_data
    integer iargc
    integer numarg
    !
    ! Command line input
    !
    numarg = iargc ( )
    if (numarg .ne. 1 .and. numarg .ne. 2) then
        write (*,*) 'Invalid number of arguments'
        write (*,*) ' '
        write (*,*) 'Legacy mode:'
        write (*,*) '  $ <program-name> <input_prefix> <output_prefix>'
        write (*,*) 'Control-file mode:'
        write (*,*) '  $ <program-name> <control_file>'
        write (*,*) ' '
        stop
    end if
    if (numarg .eq. 1) then
        call getarg ( 1, control_file )
        call READ_CONTROL(control_file)
        net_file = control_network_file
        outPrefix = control_output_prefix
        inPrefix = ''
    else
        call getarg ( 1, inPrefix )
        call getarg ( 2, outPrefix )
        net_file = TRIM(inPrefix)//'_Network'
    end if
    !
    ! Identify input/output files
    !
    param_file    = TRIM(inPrefix)//'_Parameters'
    spatial_file  = TRIM(outPrefix)//'.Spat'
    temp_file     = TRIM(outPrefix)//'.Temp'
    res_file      = TRIM(outPrefix)//'.Resv'
    !
    write(*,*) 'Network file    : ',net_file
    write(*,*) 'Parameter file  : ',param_file!
    write(*,*) 'Temperature file: ',temp_file
    write(*,*) 'Spatial file: ',spatial_file
    write(*,*) 'Reservoir file:   ',res_file
    !
    OPEN(UNIT=90,FILE=TRIM(net_file),STATUS='OLD')
    !
    !     Read header information from control file
    !
    read(90,*)
    if (control_file_mode) then
        read(90,'(A)')
        read(90,'(A)')
        read(90,'(A)')
        read(90,'(A)')
        read(90,'(A)')
        flow_file = control_flow_file
        heat_file = control_heat_file
        reservoir_file = control_reservoir_file
        reservoir_storage_file = control_reservoir_storage_file
        reservoir_sel_withdraw_file = control_reservoir_sel_withdraw_file
    else
        read(90,'(A)') flow_file
        read(90,'(A)') heat_file
        read(90,'(A)') reservoir_file
        read(90,'(A)') reservoir_storage_file
        read(90,'(A)') reservoir_sel_withdraw_file
    end if
    !
    !     Open file with hydrologic data
    !
    open(unit=35,FILE=TRIM(flow_file), ACCESS='SEQUENTIAL',FORM='FORMATTED', STATUS='old')
    !
    !     Open file with meteorologic data
    !
    open(unit=36,FILE=TRIM(heat_file), ACCESS='SEQUENTIAL',FORM='FORMATTED', STATUS='old')
    !
    !     Open file with reservoir parameters
    !
    open(unit=37,FILE=TRIM(reservoir_file), ACCESS='SEQUENTIAL',FORM='FORMATTED', STATUS='old')
    !
    !     Open file with reservoir storage data
    !
    open(unit=38,FILE=TRIM(reservoir_storage_file),ACCESS='SEQUENTIAL',FORM='FORMATTED', STATUS='old')
    !
    !     Open file with reservoir selective withdrawal data
    !
    open(unit=39,FILE=TRIM(reservoir_sel_withdraw_file),ACCESS='SEQUENTIAL',FORM='FORMATTED', STATUS='old')
    !
    !     Call systems programs to get started
    !
    !     SUBROUTINE BEGIN reads control file, sets up topology and
    !     important properties of reaches
    !
    write(*,*) 'Calling BEGIN'
    !
    !     SUBROUTINE BEGIN reads in river system information from the NETWORK file
    !
    CALL BEGIN(param_file, spatial_file)
    !
    !     SUBROUTINE SYSTMM performs the simulations
    !
    CALL SYSTMM(temp_file,res_file,param_file) ! (WUR_WF_MvV_2011/01/05)
    !
    !     Close files after simulation is complete
    !
    write(*,*) ' Closing files after simulation'

    CLOSE(35)
    CLOSE(36)
    CLOSE(37)
    CLOSE(38)
    CLOSE(39)
    CLOSE(90)
    STOP
END PROGRAM RBM10_VIC
