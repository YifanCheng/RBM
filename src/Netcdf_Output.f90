module Netcdf_Output
    use netcdf
    use Block_Network
    use, intrinsic :: ieee_arithmetic
    implicit none

    integer :: ncid = -1
    integer :: time_dimid = -1
    integer :: seg_dimid = -1
    integer :: hru_dimid = -1
    integer :: time_varid = -1
    integer :: seg_varid = -1
    integer :: hru_varid = -1
    integer :: stream_varid = -1
    integer :: air_varid = -1
    integer :: head_varid = -1
    integer :: n_time_out = 0
    integer :: n_seg_out = 0
    integer :: n_hru_out = 0
    integer :: current_day = 0
    logical :: output_uses_noleap = .false.
    integer, allocatable :: hru_values(:)
    integer, allocatable :: hru_index_by_cell(:,:)
    logical, allocatable :: write_cell_segment(:,:)
    real(8), allocatable :: t_stream(:,:,:)
    real(8), allocatable :: t_air(:,:,:)
    real(8), allocatable :: t_headwater(:,:,:)
    logical :: is_open = .false.

contains

    subroutine NC_CHECK(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in) :: context

        if (status /= nf90_noerr) then
            write(*,*) 'NetCDF error: ', trim(context), ': ', trim(nf90_strerror(status))
            stop 1
        end if
    end subroutine NC_CHECK

    subroutine NC_OUTPUT_INIT(output_file,use_noleap)
        character(len=*), intent(in) :: output_file
        logical, intent(in) :: use_noleap
        real(8) :: fill_value

        output_uses_noleap = use_noleap
        call BUILD_OUTPUT_MAP()
        n_time_out = OUTPUT_DAY_COUNT()
        n_seg_out = nseg_out_num

        fill_value = ieee_value(fill_value, ieee_quiet_nan)
        allocate(t_stream(n_hru_out,n_seg_out,1))
        allocate(t_air(n_hru_out,n_seg_out,1))
        allocate(t_headwater(n_hru_out,n_seg_out,1))
        t_stream = fill_value
        t_air = fill_value
        t_headwater = fill_value
        current_day = 0

        call NC_CHECK(nf90_create(trim(output_file), nf90_clobber, ncid), 'create output file')
        call NC_CHECK(nf90_def_dim(ncid, 'time', n_time_out, time_dimid), 'define time dimension')
        call NC_CHECK(nf90_def_dim(ncid, 'no_seg', n_seg_out, seg_dimid), 'define no_seg dimension')
        call NC_CHECK(nf90_def_dim(ncid, 'hru', n_hru_out, hru_dimid), 'define hru dimension')

        call NC_CHECK(nf90_def_var(ncid, 'time', nf90_int, (/time_dimid/), time_varid), 'define time')
        call NC_CHECK(nf90_def_var(ncid, 'no_seg', nf90_int, (/seg_dimid/), seg_varid), 'define no_seg')
        call NC_CHECK(nf90_def_var(ncid, 'hru', nf90_double, (/hru_dimid/), hru_varid), 'define hru')
        call NC_CHECK(nf90_def_var(ncid, 'T_stream', nf90_double, (/hru_dimid,seg_dimid,time_dimid/), &
            stream_varid), 'define T_stream')
        call NC_CHECK(nf90_def_var(ncid, 'T_air', nf90_double, (/hru_dimid,seg_dimid,time_dimid/), &
            air_varid), 'define T_air')
        call NC_CHECK(nf90_def_var(ncid, 'T_headwater', nf90_double, (/hru_dimid,seg_dimid,time_dimid/), &
            head_varid), 'define T_headwater')

        call NC_CHECK(nf90_put_att(ncid, time_varid, 'units', TIME_UNITS()), 'write time units')
        call NC_CHECK(nf90_put_att(ncid, time_varid, 'calendar', 'proleptic_gregorian'), 'write calendar')
        call NC_CHECK(nf90_put_att(ncid, hru_varid, '_FillValue', fill_value), 'write hru fill value')
        call NC_CHECK(nf90_put_att(ncid, stream_varid, '_FillValue', fill_value), 'write T_stream fill value')
        call NC_CHECK(nf90_put_att(ncid, air_varid, '_FillValue', fill_value), 'write T_air fill value')
        call NC_CHECK(nf90_put_att(ncid, head_varid, '_FillValue', fill_value), 'write T_headwater fill value')

        call NC_CHECK(nf90_enddef(ncid), 'end define mode')
        call WRITE_COORDINATES()
        is_open = .true.
    end subroutine NC_OUTPUT_INIT

    subroutine NC_OUTPUT_WRITE(time, run_day, nr, ncell, ns, T_0, T_head, dbt, Q_inflow, Q_outflow)
        real(8), intent(in) :: time
        integer, intent(in) :: run_day, nr, ncell, ns
        real, intent(in) :: T_0, T_head, dbt, Q_inflow, Q_outflow
        integer :: seg_no, hru_idx, time_idx

        if (.not. is_open) return
        if (run_day .lt. 1 .or. run_day .gt. n_time_out) return
        if (current_day .eq. 0) then
            current_day = run_day
        else if (run_day .ne. current_day) then
            if (run_day .lt. current_day) then
                write(*,*) 'NetCDF output error: output days are not ordered.'
                stop 1
            end if
            call FLUSH_OUTPUT_DAY()
            call CLEAR_OUTPUT_DAY()
            current_day = run_day
        end if

        if (ncell .lt. 1 .or. ncell .gt. heat_cells) return

        seg_no = OUTPUT_SEGMENT_NUMBER(nr,ncell,ns)
        if (seg_no .lt. 1) return
        if (.not. write_cell_segment(ncell,seg_no)) return

        hru_idx = hru_index_by_cell(ncell,seg_no)
        if (hru_idx .lt. 1) return
        time_idx = 1

        t_stream(hru_idx,seg_no,time_idx) = ROUND2(T_0)
        t_headwater(hru_idx,seg_no,time_idx) = ROUND2(T_head)
        t_air(hru_idx,seg_no,time_idx) = ROUND2(dbt)
    end subroutine NC_OUTPUT_WRITE

    subroutine NC_OUTPUT_CLOSE()
        if (.not. is_open) return

        call FLUSH_OUTPUT_DAY()
        call NC_CHECK(nf90_close(ncid), 'close output file')

        ncid = -1
        is_open = .false.
        if (allocated(t_stream)) deallocate(t_stream)
        if (allocated(t_air)) deallocate(t_air)
        if (allocated(t_headwater)) deallocate(t_headwater)
        if (allocated(hru_values)) deallocate(hru_values)
        if (allocated(hru_index_by_cell)) deallocate(hru_index_by_cell)
        if (allocated(write_cell_segment)) deallocate(write_cell_segment)
    end subroutine NC_OUTPUT_CLOSE

    subroutine FLUSH_OUTPUT_DAY()
        integer :: start(3), count(3)

        if (current_day .lt. 1) return
        start = (/1,1,current_day/)
        count = (/n_hru_out,n_seg_out,1/)
        call NC_CHECK(nf90_put_var(ncid, stream_varid, t_stream, start=start, count=count), &
            'write T_stream day')
        call NC_CHECK(nf90_put_var(ncid, air_varid, t_air, start=start, count=count), &
            'write T_air day')
        call NC_CHECK(nf90_put_var(ncid, head_varid, t_headwater, start=start, count=count), &
            'write T_headwater day')
    end subroutine FLUSH_OUTPUT_DAY

    subroutine CLEAR_OUTPUT_DAY()
        real(8) :: fill_value

        fill_value = ieee_value(fill_value, ieee_quiet_nan)
        t_stream = fill_value
        t_air = fill_value
        t_headwater = fill_value
    end subroutine CLEAR_OUTPUT_DAY

    subroutine BUILD_OUTPUT_MAP()
        integer :: nr, nc, cell, seg_no, hru, existing, i
        integer :: row_count, prev_reach
        integer, allocatable :: row_reach(:), row_cell(:), row_hru(:), row_seg(:)
        logical, allocatable :: row_execute(:)

        allocate(row_reach(heat_cells*nseg_out_num))
        allocate(row_cell(heat_cells*nseg_out_num))
        allocate(row_hru(heat_cells*nseg_out_num))
        allocate(row_seg(heat_cells*nseg_out_num))
        allocate(row_execute(heat_cells*nseg_out_num))
        allocate(write_cell_segment(heat_cells,nseg_out_num))
        allocate(hru_index_by_cell(heat_cells,nseg_out_num))
        allocate(hru_values(heat_cells))

        row_count = 0
        row_execute = .FALSE.
        write_cell_segment = .FALSE.
        hru_index_by_cell = 0
        n_hru_out = 0

        do nr = 1,nreach
            do nc = 1,no_cells(nr)
                cell = reach_cell(nr,nc)
                do seg_no = 1,nseg_out_num
                    row_count = row_count + 1
                    hru = hru_id(cell)
                    row_reach(row_count) = nr
                    row_cell(row_count) = cell
                    row_hru(row_count) = hru
                    row_seg(row_count) = seg_no

                    if (seg_no .eq. 1) then
                        prev_reach = -1
                        do i = 1,row_count-1
                            if (row_hru(i) .eq. hru .and. row_reach(i) .gt. prev_reach) prev_reach = row_reach(i)
                        end do
                        if (prev_reach .lt. 0) then
                            row_execute(row_count) = .TRUE.
                        else if (nr .gt. prev_reach) then
                            row_execute(row_count) = .TRUE.
                            do i = 1,row_count-1
                                if (row_hru(i) .eq. hru) row_execute(i) = .FALSE.
                            end do
                        else
                            row_execute(row_count) = .FALSE.
                        end if
                    else
                        row_execute(row_count) = row_execute(row_count-1)
                    end if
                end do
            end do
        end do

        do i = 1,row_count
            if (row_execute(i)) call ADD_HRU_VALUE(row_hru(i))
        end do
        call SORT_HRU_VALUES()

        do i = 1,row_count
            if (row_execute(i)) then
                existing = FIND_HRU_INDEX(row_hru(i))
                write_cell_segment(row_cell(i),row_seg(i)) = .TRUE.
                hru_index_by_cell(row_cell(i),row_seg(i)) = existing
            end if
        end do

        deallocate(row_reach,row_cell,row_hru,row_seg,row_execute)
    end subroutine BUILD_OUTPUT_MAP

    subroutine ADD_HRU_VALUE(value)
        integer, intent(in) :: value
        if (FIND_HRU_INDEX(value) .gt. 0) return
        n_hru_out = n_hru_out + 1
        hru_values(n_hru_out) = value
    end subroutine ADD_HRU_VALUE

    integer function FIND_HRU_INDEX(value)
        integer, intent(in) :: value
        integer :: i
        FIND_HRU_INDEX = 0
        do i = 1,n_hru_out
            if (hru_values(i) .eq. value) then
                FIND_HRU_INDEX = i
                return
            end if
        end do
    end function FIND_HRU_INDEX

    subroutine SORT_HRU_VALUES()
        integer :: i, j, tmp
        do i = 1,n_hru_out-1
            do j = i+1,n_hru_out
                if (hru_values(j) .lt. hru_values(i)) then
                    tmp = hru_values(i)
                    hru_values(i) = hru_values(j)
                    hru_values(j) = tmp
                end if
            end do
        end do
    end subroutine SORT_HRU_VALUES

    integer function OUTPUT_SEGMENT_NUMBER(nr,ncell,ns)
        integer, intent(in) :: nr, ncell, ns
        integer :: seg_no
        OUTPUT_SEGMENT_NUMBER = -1
        do seg_no = 1,nseg_out_num
            if (nseg_out(nr,ncell,seg_no) .eq. ns) then
                OUTPUT_SEGMENT_NUMBER = seg_no
                return
            end if
        end do
    end function OUTPUT_SEGMENT_NUMBER

    integer function OUTPUT_DAY_COUNT()
        integer, external :: Julian

        OUTPUT_DAY_COUNT = Julian(end_year,end_month,end_day) - &
            Julian(start_year,start_month,start_day) + 1
        if (OUTPUT_DAY_COUNT .lt. 1) OUTPUT_DAY_COUNT = 0
    end function OUTPUT_DAY_COUNT

    subroutine WRITE_COORDINATES()
        integer, allocatable :: time_values(:), seg_values(:)
        real(8), allocatable :: hru_coord(:)
        integer :: i, output_year, output_month, output_day, start_julian
        integer, external :: Julian

        allocate(time_values(n_time_out))
        allocate(seg_values(n_seg_out))
        allocate(hru_coord(n_hru_out))

        if (output_uses_noleap) then
            output_year = start_year
            output_month = start_month
            output_day = start_day
            start_julian = Julian(start_year,start_month,start_day)
            do i = 1,n_time_out
                time_values(i) = Julian(output_year,output_month,output_day) - start_julian
                call ADVANCE_NOLEAP_DATE(output_year,output_month,output_day)
            end do
        else
            do i = 1,n_time_out
                time_values(i) = i - 1
            end do
        end if
        do i = 1,n_seg_out
            seg_values(i) = i
        end do
        do i = 1,n_hru_out
            hru_coord(i) = dble(hru_values(i))
        end do

        call NC_CHECK(nf90_put_var(ncid, time_varid, time_values), 'write time coordinate')
        call NC_CHECK(nf90_put_var(ncid, seg_varid, seg_values), 'write no_seg coordinate')
        call NC_CHECK(nf90_put_var(ncid, hru_varid, hru_coord), 'write hru coordinate')

        deallocate(time_values,seg_values,hru_coord)
    end subroutine WRITE_COORDINATES

    subroutine ADVANCE_NOLEAP_DATE(year,month,day)
        integer, intent(inout) :: year, month, day
        integer, dimension(12) :: days_in_month

        days_in_month = (/31,28,31,30,31,30,31,31,30,31,30,31/)
        day = day + 1
        if (day .gt. days_in_month(month)) then
            day = 1
            month = month + 1
            if (month .gt. 12) then
                month = 1
                year = year + 1
            end if
        end if
    end subroutine ADVANCE_NOLEAP_DATE

    real(8) function ROUND2(value)
        real, intent(in) :: value
        character(len=32) :: text
        write(text,'(f20.2)') value
        read(text,*) ROUND2
    end function ROUND2

    character(len=64) function TIME_UNITS()
        write(TIME_UNITS,'(A,I4.4,A,I2.2,A,I2.2,A)') 'days since ',start_year,'-',start_month,'-',start_day, &
            ' 00:00:00'
    end function TIME_UNITS

end module Netcdf_Output
