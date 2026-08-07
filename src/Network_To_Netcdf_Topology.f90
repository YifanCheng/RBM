program Network_To_Netcdf_Topology
    use netcdf
    implicit none

    character(len=256) :: network_file, topology_file
    character(len=512) :: line
    character(len=8) :: lat_text
    character(len=10) :: lon_text
    integer :: argc, ios
    integer :: nreach, flow_cells, heat_cells, nres
    integer :: nr, nc, ncell
    logical :: source, reservoir, select_withdraw, has_grid_coords
    integer, allocatable :: no_cells(:), head_name(:), trib_cell(:), main_stem(:)
    integer, allocatable :: node(:), row(:), col(:), hru_id(:), downstream_id(:), res_num(:)
    real, allocatable :: rmile_head(:), alphamu(:), beta(:), gmma(:), mu(:), smooth_param(:)
    real, allocatable :: rmile(:), ndelta(:)
    real, allocatable :: grid_lat(:), grid_lon(:)

    argc = command_argument_count()
    if (argc .ne. 2) then
        write(*,*) 'Usage: network_to_netcdf_topology <legacy_Network_file> <topology.nc>'
        stop 1
    end if

    call get_command_argument(1, network_file)
    call get_command_argument(2, topology_file)

    open(unit=10, file=trim(network_file), status='old', action='read')
    read(10,*)
    read(10,'(A)')
    read(10,'(A)')
    read(10,'(A)')
    read(10,'(A)')
    read(10,'(A)')
    read(10,'(A)')
    read(10,*,iostat=ios) nreach, flow_cells, heat_cells, source, reservoir, nres, select_withdraw
    if (ios .ne. 0) then
        rewind(10)
        call FIND_COUNTS_LINE(10, nreach, flow_cells, heat_cells, source, reservoir, nres, select_withdraw)
    end if

    allocate(no_cells(nreach), head_name(nreach), trib_cell(nreach), main_stem(nreach))
    allocate(rmile_head(nreach), alphamu(nreach), beta(nreach), gmma(nreach), mu(nreach), smooth_param(nreach))
    allocate(node(heat_cells), row(heat_cells), col(heat_cells), hru_id(heat_cells), downstream_id(heat_cells))
    allocate(rmile(heat_cells), ndelta(heat_cells), res_num(heat_cells))
    allocate(grid_lat(heat_cells), grid_lon(heat_cells))
    res_num = 0

    ncell = 0
    do nr = 1, nreach
        read(10,'(i5,11x,i4,10x,i5,15x,i5,15x,f10.0,i5)') no_cells(nr), &
            head_name(nr), trib_cell(nr), main_stem(nr), rmile_head(nr)
        read(10,*) alphamu(nr), beta(nr), gmma(nr), mu(nr), smooth_param(nr)
        do nc = 1, no_cells(nr)
            ncell = ncell + 1
            if (reservoir) then
                read(10,'(5x,i5,5x,i5,8x,i5,6x,a8,6x,a10,7x,f10.0,f5.0,i6)') &
                    node(ncell), row(ncell), col(ncell), lat_text, lon_text, &
                    rmile(ncell), ndelta(ncell), res_num(ncell)
            else
                read(10,'(5x,i5,5x,i5,8x,i5,6x,a8,6x,a10,7x,f10.0,f5.0)') &
                    node(ncell), row(ncell), col(ncell), lat_text, lon_text, &
                    rmile(ncell), ndelta(ncell)
            end if
            read(lat_text,*,iostat=ios) grid_lat(ncell)
            if (ios .ne. 0) then
                write(*,*) 'Network topology conversion error: invalid Lat at cell ', ncell
                stop 1
            end if
            read(lon_text,*,iostat=ios) grid_lon(ncell)
            if (ios .ne. 0) then
                write(*,*) 'Network topology conversion error: invalid Long at cell ', ncell
                stop 1
            end if
            read(lat_text,*,iostat=ios) hru_id(ncell)
            if (ios .ne. 0) hru_id(ncell) = node(ncell)
            read(lon_text,*,iostat=ios) downstream_id(ncell)
            if (ios .ne. 0) downstream_id(ncell) = node(ncell)
        end do
    end do
    close(10)
    has_grid_coords = all(abs(grid_lat) .le. 90.0) .and. all(abs(grid_lon) .le. 180.0)

    if (ncell .ne. heat_cells) then
        write(*,*) 'Network topology conversion error: read ', ncell, ' cells, expected ', heat_cells
        stop 1
    end if

    call WRITE_TOPOLOGY_NETCDF(trim(topology_file), nreach, flow_cells, heat_cells, has_grid_coords, &
        no_cells, head_name, trib_cell, main_stem, rmile_head, alphamu, beta, gmma, mu, smooth_param, &
        node, row, col, hru_id, downstream_id, grid_lat, grid_lon, rmile, ndelta, res_num)
    write(*,*) 'Wrote netCDF topology file: ', trim(topology_file)

contains

    subroutine CHECK(status, context)
        integer, intent(in) :: status
        character(len=*), intent(in) :: context

        if (status /= nf90_noerr) then
            write(*,*) 'NetCDF topology conversion error: ', trim(context), ': ', trim(nf90_strerror(status))
            stop 1
        end if
    end subroutine CHECK

    subroutine FIND_COUNTS_LINE(unit_no, nreach, flow_cells, heat_cells, source, reservoir, nres, select_withdraw)
        integer, intent(in) :: unit_no
        integer, intent(out) :: nreach, flow_cells, heat_cells, nres
        logical, intent(out) :: source, reservoir, select_withdraw
        character(len=512) :: counts_line
        integer :: ios

        do
            read(unit_no,'(A)',iostat=ios) counts_line
            if (ios .ne. 0) then
                write(*,*) 'Network topology conversion error: could not find counts line.'
                stop 1
            end if
            counts_line = adjustl(counts_line)
            if (len_trim(counts_line) .eq. 0) cycle
            if (counts_line(1:1) .lt. '0' .or. counts_line(1:1) .gt. '9') cycle
            read(counts_line,*,iostat=ios) nreach, flow_cells, heat_cells, source, reservoir, nres, select_withdraw
            if (ios .eq. 0) return
            select_withdraw = .false.
            read(counts_line,*,iostat=ios) nreach, flow_cells, heat_cells, source, reservoir, nres
            if (ios .eq. 0) return
        end do
    end subroutine FIND_COUNTS_LINE

    subroutine DEF_INT_VAR(ncid, dimid, name, varid)
        integer, intent(in) :: ncid, dimid
        character(len=*), intent(in) :: name
        integer, intent(out) :: varid

        call CHECK(nf90_def_var(ncid, trim(name), nf90_int, (/dimid/), varid), 'define '//trim(name))
    end subroutine DEF_INT_VAR

    subroutine DEF_REAL_VAR(ncid, dimid, name, varid)
        integer, intent(in) :: ncid, dimid
        character(len=*), intent(in) :: name
        integer, intent(out) :: varid

        call CHECK(nf90_def_var(ncid, trim(name), nf90_real, (/dimid/), varid), 'define '//trim(name))
    end subroutine DEF_REAL_VAR

    subroutine WRITE_TOPOLOGY_NETCDF(path, nreach, flow_cells, heat_cells, has_grid_coords, &
        no_cells, head_name, trib_cell, main_stem, rmile_head, alphamu, beta, gmma, mu, smooth_param, &
        node, row, col, hru_id, downstream_id, grid_lat, grid_lon, rmile, ndelta, res_num)
        character(len=*), intent(in) :: path
        integer, intent(in) :: nreach, flow_cells, heat_cells
        logical, intent(in) :: has_grid_coords
        integer, intent(in) :: no_cells(:), head_name(:), trib_cell(:), main_stem(:)
        integer, intent(in) :: node(:), row(:), col(:), hru_id(:), downstream_id(:), res_num(:)
        real, intent(in) :: rmile_head(:), alphamu(:), beta(:), gmma(:), mu(:), smooth_param(:)
        real, intent(in) :: grid_lat(:), grid_lon(:), rmile(:), ndelta(:)

        integer :: ncid, reach_dimid, cell_dimid
        integer :: nreach_varid, flow_cells_varid, heat_cells_varid
        integer :: no_cells_varid, head_name_varid, trib_cell_varid, main_stem_varid, rmile_head_varid
        integer :: alphamu_varid, beta_varid, gmma_varid, mu_varid, smooth_param_varid
        integer :: node_varid, row_varid, col_varid, hru_id_varid, downstream_id_varid
        integer :: grid_lat_varid, grid_lon_varid, rmile_varid, ndelta_varid, res_num_varid

        call CHECK(nf90_create(trim(path), nf90_clobber, ncid), 'create topology file')
        call CHECK(nf90_def_dim(ncid, 'reach', nreach, reach_dimid), 'define reach dimension')
        call CHECK(nf90_def_dim(ncid, 'cell', heat_cells, cell_dimid), 'define cell dimension')
        call CHECK(nf90_def_var(ncid, 'nreach', nf90_int, nreach_varid), 'define nreach')
        call CHECK(nf90_def_var(ncid, 'flow_cells', nf90_int, flow_cells_varid), 'define flow_cells')
        call CHECK(nf90_def_var(ncid, 'heat_cells', nf90_int, heat_cells_varid), 'define heat_cells')
        call DEF_INT_VAR(ncid, reach_dimid, 'no_cells', no_cells_varid)
        call DEF_INT_VAR(ncid, reach_dimid, 'head_name', head_name_varid)
        call DEF_INT_VAR(ncid, reach_dimid, 'trib_cell', trib_cell_varid)
        call DEF_INT_VAR(ncid, reach_dimid, 'main_stem', main_stem_varid)
        call DEF_REAL_VAR(ncid, reach_dimid, 'rmile_head', rmile_head_varid)
        call DEF_REAL_VAR(ncid, reach_dimid, 'alphamu', alphamu_varid)
        call DEF_REAL_VAR(ncid, reach_dimid, 'beta', beta_varid)
        call DEF_REAL_VAR(ncid, reach_dimid, 'gmma', gmma_varid)
        call DEF_REAL_VAR(ncid, reach_dimid, 'mu', mu_varid)
        call DEF_REAL_VAR(ncid, reach_dimid, 'smooth_param', smooth_param_varid)
        call DEF_INT_VAR(ncid, cell_dimid, 'node', node_varid)
        call DEF_INT_VAR(ncid, cell_dimid, 'row', row_varid)
        call DEF_INT_VAR(ncid, cell_dimid, 'col', col_varid)
        call DEF_INT_VAR(ncid, cell_dimid, 'hru_id', hru_id_varid)
        call DEF_INT_VAR(ncid, cell_dimid, 'downstream_id', downstream_id_varid)
        if (has_grid_coords) then
            call DEF_REAL_VAR(ncid, cell_dimid, 'grid_lat', grid_lat_varid)
            call DEF_REAL_VAR(ncid, cell_dimid, 'grid_lon', grid_lon_varid)
        end if
        call DEF_REAL_VAR(ncid, cell_dimid, 'rmile', rmile_varid)
        call DEF_REAL_VAR(ncid, cell_dimid, 'ndelta', ndelta_varid)
        call DEF_INT_VAR(ncid, cell_dimid, 'res_num', res_num_varid)
        call CHECK(nf90_put_att(ncid, nf90_global, 'title', 'RBM river topology'), 'write title')
        call CHECK(nf90_put_att(ncid, nf90_global, 'source_schema', 'legacy RBM Network topology'), 'write source schema')
        call CHECK(nf90_enddef(ncid), 'end define mode')

        call CHECK(nf90_put_var(ncid, nreach_varid, nreach), 'write nreach')
        call CHECK(nf90_put_var(ncid, flow_cells_varid, flow_cells), 'write flow_cells')
        call CHECK(nf90_put_var(ncid, heat_cells_varid, heat_cells), 'write heat_cells')
        call CHECK(nf90_put_var(ncid, no_cells_varid, no_cells), 'write no_cells')
        call CHECK(nf90_put_var(ncid, head_name_varid, head_name), 'write head_name')
        call CHECK(nf90_put_var(ncid, trib_cell_varid, trib_cell), 'write trib_cell')
        call CHECK(nf90_put_var(ncid, main_stem_varid, main_stem), 'write main_stem')
        call CHECK(nf90_put_var(ncid, rmile_head_varid, rmile_head), 'write rmile_head')
        call CHECK(nf90_put_var(ncid, alphamu_varid, alphamu), 'write alphamu')
        call CHECK(nf90_put_var(ncid, beta_varid, beta), 'write beta')
        call CHECK(nf90_put_var(ncid, gmma_varid, gmma), 'write gmma')
        call CHECK(nf90_put_var(ncid, mu_varid, mu), 'write mu')
        call CHECK(nf90_put_var(ncid, smooth_param_varid, smooth_param), 'write smooth_param')
        call CHECK(nf90_put_var(ncid, node_varid, node), 'write node')
        call CHECK(nf90_put_var(ncid, row_varid, row), 'write row')
        call CHECK(nf90_put_var(ncid, col_varid, col), 'write col')
        call CHECK(nf90_put_var(ncid, hru_id_varid, hru_id), 'write hru_id')
        call CHECK(nf90_put_var(ncid, downstream_id_varid, downstream_id), 'write downstream_id')
        if (has_grid_coords) then
            call CHECK(nf90_put_var(ncid, grid_lat_varid, grid_lat), 'write grid_lat')
            call CHECK(nf90_put_var(ncid, grid_lon_varid, grid_lon), 'write grid_lon')
        end if
        call CHECK(nf90_put_var(ncid, rmile_varid, rmile), 'write rmile')
        call CHECK(nf90_put_var(ncid, ndelta_varid, ndelta), 'write ndelta')
        call CHECK(nf90_put_var(ncid, res_num_varid, res_num), 'write res_num')
        call CHECK(nf90_close(ncid), 'close topology file')
    end subroutine WRITE_TOPOLOGY_NETCDF

end program Network_To_Netcdf_Topology
