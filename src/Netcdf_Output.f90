module Netcdf_Output
    use netcdf
    implicit none

    integer :: ncid = -1
    integer :: rec_dimid = -1
    integer :: time_varid = -1
    integer :: day_varid = -1
    integer :: reach_varid = -1
    integer :: cell_varid = -1
    integer :: segment_varid = -1
    integer :: water_temp_varid = -1
    integer :: headwater_temp_varid = -1
    integer :: air_temp_varid = -1
    integer :: qin_varid = -1
    integer :: qout_varid = -1
    integer :: record_count = 0
    logical :: is_open = .false.

contains

    subroutine NC_CHECK(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in) :: context

        if (status /= nf90_noerr) then
            write(*,*) 'NetCDF error: ', trim(context), ': ', trim(nf90_strerror(status))
            stop
        end if
    end subroutine NC_CHECK

    subroutine NC_OUTPUT_INIT(output_file)
        character(len=*), intent(in) :: output_file

        call NC_CHECK(nf90_create(trim(output_file), nf90_clobber, ncid), 'create output file')
        call NC_CHECK(nf90_def_dim(ncid, 'record', nf90_unlimited, rec_dimid), 'define record dimension')

        call NC_CHECK(nf90_def_var(ncid, 'time', nf90_double, (/rec_dimid/), time_varid), 'define time')
        call NC_CHECK(nf90_def_var(ncid, 'day', nf90_int, (/rec_dimid/), day_varid), 'define day')
        call NC_CHECK(nf90_def_var(ncid, 'reach', nf90_int, (/rec_dimid/), reach_varid), 'define reach')
        call NC_CHECK(nf90_def_var(ncid, 'cell', nf90_int, (/rec_dimid/), cell_varid), 'define cell')
        call NC_CHECK(nf90_def_var(ncid, 'segment', nf90_int, (/rec_dimid/), segment_varid), 'define segment')
        call NC_CHECK(nf90_def_var(ncid, 'water_temperature', nf90_float, (/rec_dimid/), water_temp_varid), &
            'define water_temperature')
        call NC_CHECK(nf90_def_var(ncid, 'headwater_temperature', nf90_float, (/rec_dimid/), &
            headwater_temp_varid), 'define headwater_temperature')
        call NC_CHECK(nf90_def_var(ncid, 'air_temperature', nf90_float, (/rec_dimid/), air_temp_varid), &
            'define air_temperature')
        call NC_CHECK(nf90_def_var(ncid, 'qin', nf90_float, (/rec_dimid/), qin_varid), 'define qin')
        call NC_CHECK(nf90_def_var(ncid, 'qout', nf90_float, (/rec_dimid/), qout_varid), 'define qout')

        call NC_CHECK(nf90_put_att(ncid, nf90_global, 'source', &
            'RBM stream temperature output mirrored from ASCII .Temp records'), 'write source attribute')
        call NC_CHECK(nf90_put_att(ncid, water_temp_varid, 'units', 'deg C'), &
            'write water_temperature units')
        call NC_CHECK(nf90_put_att(ncid, headwater_temp_varid, 'units', 'deg C'), &
            'write headwater_temperature units')
        call NC_CHECK(nf90_put_att(ncid, air_temp_varid, 'units', 'deg C'), &
            'write air_temperature units')

        call NC_CHECK(nf90_enddef(ncid), 'end define mode')
        record_count = 0
        is_open = .true.
    end subroutine NC_OUTPUT_INIT

    subroutine NC_OUTPUT_WRITE(time, nd, nr, ncell, ns, T_0, T_head, dbt, Q_inflow, Q_outflow)
        real(8), intent(in) :: time
        integer, intent(in) :: nd, nr, ncell, ns
        real, intent(in) :: T_0, T_head, dbt, Q_inflow, Q_outflow
        integer :: rec
        real(8) :: time_out(1)
        integer :: int_out(1)
        real :: real_out(1)

        if (.not. is_open) return

        record_count = record_count + 1
        rec = record_count

        time_out(1) = time
        call NC_CHECK(nf90_put_var(ncid, time_varid, time_out, start=(/rec/), count=(/1/)), 'write time')

        int_out(1) = nd
        call NC_CHECK(nf90_put_var(ncid, day_varid, int_out, start=(/rec/), count=(/1/)), 'write day')
        int_out(1) = nr
        call NC_CHECK(nf90_put_var(ncid, reach_varid, int_out, start=(/rec/), count=(/1/)), 'write reach')
        int_out(1) = ncell
        call NC_CHECK(nf90_put_var(ncid, cell_varid, int_out, start=(/rec/), count=(/1/)), 'write cell')
        int_out(1) = ns
        call NC_CHECK(nf90_put_var(ncid, segment_varid, int_out, start=(/rec/), count=(/1/)), 'write segment')

        real_out(1) = T_0
        call NC_CHECK(nf90_put_var(ncid, water_temp_varid, real_out, start=(/rec/), count=(/1/)), &
            'write water_temperature')
        real_out(1) = T_head
        call NC_CHECK(nf90_put_var(ncid, headwater_temp_varid, real_out, start=(/rec/), count=(/1/)), &
            'write headwater_temperature')
        real_out(1) = dbt
        call NC_CHECK(nf90_put_var(ncid, air_temp_varid, real_out, start=(/rec/), count=(/1/)), &
            'write air_temperature')
        real_out(1) = Q_inflow
        call NC_CHECK(nf90_put_var(ncid, qin_varid, real_out, start=(/rec/), count=(/1/)), 'write qin')
        real_out(1) = Q_outflow
        call NC_CHECK(nf90_put_var(ncid, qout_varid, real_out, start=(/rec/), count=(/1/)), 'write qout')
    end subroutine NC_OUTPUT_WRITE

    subroutine NC_OUTPUT_CLOSE()
        if (.not. is_open) return

        call NC_CHECK(nf90_close(ncid), 'close output file')
        ncid = -1
        is_open = .false.
    end subroutine NC_OUTPUT_CLOSE

end module Netcdf_Output
