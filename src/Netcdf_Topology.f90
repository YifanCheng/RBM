module Netcdf_Topology
    use netcdf
    use Block_Control
    use Block_Energy
    use Block_Hydro
    use Block_Network
    use Block_Reservoir
    implicit none

contains

    subroutine NC_TOPO_CHECK(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in) :: context

        if (status /= nf90_noerr) then
            write(*,*) 'NetCDF topology error: ', trim(context), ': ', trim(nf90_strerror(status))
            stop
        end if
    end subroutine NC_TOPO_CHECK

    subroutine READ_NC_INT_SCALAR(ncid, name, value)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: value
        integer :: varid

        call NC_TOPO_CHECK(nf90_inq_varid(ncid, trim(name), varid), 'find '//trim(name))
        call NC_TOPO_CHECK(nf90_get_var(ncid, varid, value), 'read '//trim(name))
    end subroutine READ_NC_INT_SCALAR

    subroutine READ_NC_INT_1D(ncid, name, values)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: values(:)
        integer :: varid

        call NC_TOPO_CHECK(nf90_inq_varid(ncid, trim(name), varid), 'find '//trim(name))
        call NC_TOPO_CHECK(nf90_get_var(ncid, varid, values), 'read '//trim(name))
    end subroutine READ_NC_INT_1D

    subroutine READ_NC_REAL_1D(ncid, name, values)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        real, intent(out) :: values(:)
        integer :: varid

        call NC_TOPO_CHECK(nf90_inq_varid(ncid, trim(name), varid), 'find '//trim(name))
        call NC_TOPO_CHECK(nf90_get_var(ncid, varid, values), 'read '//trim(name))
    end subroutine READ_NC_REAL_1D

    subroutine ALLOCATE_TOPOLOGY_ARRAYS()
        allocate(ndelta(heat_cells))
        allocate(mu(nreach))
        allocate(alphamu(nreach))
        allocate(beta(nreach))
        allocate(gmma(nreach))
        allocate(smooth_param(nreach))
        allocate(dx(heat_cells))
        allocate(no_celm(nreach))
        no_celm = 0
        allocate(no_cells(nreach))
        no_cells = 0
        allocate(no_tribs(heat_cells))
        no_tribs = 0
        allocate(trib(heat_cells,10))
        trib = 0
        allocate(conflnce(heat_cells,10))
        conflnce = 0
        allocate(reach_cell(nreach,ns_max))
        allocate(head_cell(nreach))
        allocate(hru_id(heat_cells))
        allocate(segment_cell(nreach,ns_max))
        allocate(x_dist(nreach,0:ns_max))

        allocate(res_num(heat_cells))
        res_num = 0
        allocate(res_pres(heat_cells))
        res_pres = .FALSE.
        allocate(dam_number(nres))
        allocate(dam_grid_lat(nres))
        allocate(dam_grid_lon(nres))
        allocate(res_depth_meter(nres))
        allocate(res_width_meter(nres))
        allocate(res_area_km2(nres))
        allocate(res_start_node(nres))
        allocate(res_end_node(nres))
        allocate(res_capacity_mcm(nres))
        allocate(nseg_out(nreach,heat_cells,nseg_out_num))
    end subroutine ALLOCATE_TOPOLOGY_ARRAYS

    subroutine READ_RESERVOIR_PARAMETERS()
        integer :: nreservoir

        if (reservoir) then
            read(37,*)
            do nreservoir = 1, nres
                read(37,*) dam_number(nreservoir), &
                    dam_grid_lat(nreservoir), dam_grid_lon(nreservoir), &
                    res_depth_meter(nreservoir), res_width_meter(nreservoir), &
                    res_area_km2(nreservoir), res_capacity_mcm(nreservoir), &
                    res_start_node(nreservoir), res_end_node(nreservoir)

                print *, dam_number(nreservoir), res_depth_meter(nreservoir), &
                    res_width_meter(nreservoir), res_area_km2(nreservoir), &
                    res_start_node(nreservoir), res_end_node(nreservoir)
            end do
        end if
    end subroutine READ_RESERVOIR_PARAMETERS

    subroutine READ_TOPOLOGY_NETCDF(spatial_file)
        character(len=*), intent(in) :: spatial_file

        integer :: ncid
        integer :: nc, ncell, nseg, nr, nseg_temp, nseg_cum
        integer :: head_name, trib_cell, main_stem, ns_max_test
        integer :: nrow, ncol, nndlta_int
        real :: rmile0, rmile1, nndlta
        logical :: first_cell
        character(len=8) :: lat
        character(len=10) :: long
        real, parameter :: miles_to_ft = 5280.0

        integer, allocatable :: topo_no_cells(:)
        integer, allocatable :: topo_head_name(:)
        integer, allocatable :: topo_trib_cell(:)
        integer, allocatable :: topo_main_stem(:)
        integer, allocatable :: topo_node(:)
        integer, allocatable :: topo_row(:)
        integer, allocatable :: topo_col(:)
        integer, allocatable :: topo_hru_id(:)
        integer, allocatable :: topo_downstream_id(:)
        integer, allocatable :: topo_res_num(:)
        real, allocatable :: topo_rmile_head(:)
        real, allocatable :: topo_rmile(:)
        real, allocatable :: topo_ndelta(:)
        real, allocatable :: topo_alphamu(:)
        real, allocatable :: topo_beta(:)
        real, allocatable :: topo_gmma(:)
        real, allocatable :: topo_mu(:)
        real, allocatable :: topo_smooth_param(:)

        call NC_TOPO_CHECK(nf90_open(trim(control_topology_file), nf90_nowrite, ncid), 'open topology file')
        call READ_NC_INT_SCALAR(ncid, 'nreach', nreach)
        call READ_NC_INT_SCALAR(ncid, 'flow_cells', flow_cells)
        call READ_NC_INT_SCALAR(ncid, 'heat_cells', heat_cells)

        reservoir = control_reservoir
        nres = control_nres
        select_withdraw = control_select_withdraw

        call ALLOCATE_TOPOLOGY_ARRAYS()
        allocate(topo_no_cells(nreach), topo_head_name(nreach), topo_trib_cell(nreach))
        allocate(topo_main_stem(nreach), topo_rmile_head(nreach))
        allocate(topo_alphamu(nreach), topo_beta(nreach), topo_gmma(nreach))
        allocate(topo_mu(nreach), topo_smooth_param(nreach))
        allocate(topo_node(heat_cells), topo_row(heat_cells), topo_col(heat_cells))
        allocate(topo_hru_id(heat_cells), topo_downstream_id(heat_cells))
        allocate(topo_rmile(heat_cells), topo_ndelta(heat_cells), topo_res_num(heat_cells))

        call READ_NC_INT_1D(ncid, 'no_cells', topo_no_cells)
        call READ_NC_INT_1D(ncid, 'head_name', topo_head_name)
        call READ_NC_INT_1D(ncid, 'trib_cell', topo_trib_cell)
        call READ_NC_INT_1D(ncid, 'main_stem', topo_main_stem)
        call READ_NC_REAL_1D(ncid, 'rmile_head', topo_rmile_head)
        call READ_NC_REAL_1D(ncid, 'alphamu', topo_alphamu)
        call READ_NC_REAL_1D(ncid, 'beta', topo_beta)
        call READ_NC_REAL_1D(ncid, 'gmma', topo_gmma)
        call READ_NC_REAL_1D(ncid, 'mu', topo_mu)
        call READ_NC_REAL_1D(ncid, 'smooth_param', topo_smooth_param)
        call READ_NC_INT_1D(ncid, 'node', topo_node)
        call READ_NC_INT_1D(ncid, 'row', topo_row)
        call READ_NC_INT_1D(ncid, 'col', topo_col)
        call READ_NC_INT_1D(ncid, 'hru_id', topo_hru_id)
        call READ_NC_INT_1D(ncid, 'downstream_id', topo_downstream_id)
        call READ_NC_REAL_1D(ncid, 'rmile', topo_rmile)
        call READ_NC_REAL_1D(ncid, 'ndelta', topo_ndelta)
        call READ_NC_INT_1D(ncid, 'res_num', topo_res_num)
        call NC_TOPO_CHECK(nf90_close(ncid), 'close topology file')

        call READ_RESERVOIR_PARAMETERS()

        ncell = 0
        ns_max_test = -1
        do nr = 1, nreach
            nseg = 0
            nseg_cum = 0
            write(*,*) ' Starting to read reach ', nr

            no_cells(nr) = topo_no_cells(nr)
            head_name = topo_head_name(nr)
            trib_cell = topo_trib_cell(nr)
            main_stem = topo_main_stem(nr)
            rmile0 = topo_rmile_head(nr)
            x_dist(nr,0) = miles_to_ft * rmile0
            write(*,*) 'reach number', nr, 'boundary_distance', x_dist(nr,0)

            if (trib_cell .gt. 0) then
                no_tribs(trib_cell) = no_tribs(trib_cell) + 1
                trib(trib_cell,no_tribs(trib_cell)) = nr
            end if

            alphamu(nr) = topo_alphamu(nr)
            beta(nr) = topo_beta(nr)
            gmma(nr) = topo_gmma(nr)
            mu(nr) = topo_mu(nr)
            smooth_param(nr) = topo_smooth_param(nr)

            first_cell = .true.
            do nc = 1, no_cells(nr)
                ncell = ncell + 1
                reach_cell(nr,nc) = ncell
                nrow = topo_row(ncell)
                ncol = topo_col(ncell)
                hru_id(ncell) = topo_hru_id(ncell)
                write(lat,'(i8)') topo_hru_id(ncell)
                write(long,'(i10)') topo_downstream_id(ncell)
                rmile1 = topo_rmile(ncell)
                ndelta(ncell) = topo_ndelta(ncell)
                res_num(ncell) = topo_res_num(ncell)
                if (reservoir .and. res_num(ncell) .gt. 0) res_pres(ncell) = .TRUE.

                if (ndelta(ncell) .lt. 1) ndelta(ncell) = n_default
                if (first_cell) then
                    first_cell = .false.
                    head_cell(nr) = ncell
                    x_dist(nr,0) = miles_to_ft * rmile0
                end if

                dx(ncell) = miles_to_ft * (rmile0 - rmile1) / ndelta(ncell)
                rmile0 = rmile1

                do nseg_temp = 1, nseg_out_num
                    nseg_out(nr,ncell,nseg_temp) = nseg_cum + ndelta(ncell) * nseg_temp / nseg_out_num
                end do
                nseg_cum = nseg_cum + ndelta(ncell)
                nndlta = 0
200             continue
                nndlta = nndlta + 1
                nseg = nseg + 1
                segment_cell(nr,nseg) = ncell
                x_dist(nr,nseg) = x_dist(nr,nseg-1) - dx(ncell)

                if (control_write_ascii) then
                    do nseg_temp = 1, nseg_out_num
                        if (nseg_out(nr,ncell,nseg_temp) .eq. nseg) then
                            open(22,file=trim(spatial_file),status='unknown')
                            write(22,'(4i6,1x,a8,1x,a10,i5)') nr,ncell,nrow,ncol,lat,long,nseg_temp
                        end if
                    end do
                end if

                if (nndlta .lt. ndelta(ncell)) go to 200
                no_celm(nr) = nseg
                segment_cell(nr,nseg) = ncell
                x_dist(nr,nseg) = miles_to_ft * rmile1
                write(*,*) 'number of segment in reach', nr, nseg
            end do

            if (trib_cell .gt. 0) then
                conflnce(trib_cell,no_tribs(trib_cell)) = ncell
            end if

            if (ns_max_test .lt. nseg) ns_max_test = nseg
        end do

        if (ns_max_test .gt. ns_max) then
            write(*,*) 'RBM is terminating because'
            write(*,*) 'NS_MAX exceeded. Change NS_MAX in Block_Network to: ', ns_max_test
            stop
        end if

        nwpd = 1
        dt_comp = 86400.0

        deallocate(topo_no_cells, topo_head_name, topo_trib_cell, topo_main_stem)
        deallocate(topo_rmile_head, topo_alphamu, topo_beta, topo_gmma, topo_mu, topo_smooth_param)
        deallocate(topo_node, topo_row, topo_col, topo_hru_id, topo_downstream_id)
        deallocate(topo_rmile, topo_ndelta, topo_res_num)
    end subroutine READ_TOPOLOGY_NETCDF

end module Netcdf_Topology
