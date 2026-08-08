SUBROUTINE WRITE(time,nd,run_day,nr,ncell,ns,T_0,T_head,dbt,Q_inflow,Q_outflow)
!
#ifdef USE_NETCDF
use Netcdf_Output
#endif
use Block_Control
Implicit NONE
!
integer :: nd,run_day,nr,ncell,ns
real    :: Q_inflow,Q_outflow
real    :: T_0,T_dist, storage, T_storage
real(8) :: time
real    :: T_head
real    :: dbt
!
if (control_write_ascii) then
    write (20,'(f12.4,4i6,3f8.2,4f25.1,f8.1,f8.4)')           &
                time,nd,nr,ncell,ns,T_0,T_head,dbt,Q_inflow,Q_outflow
end if
#ifdef USE_NETCDF
if (control_write_netcdf) then
    call NC_OUTPUT_WRITE(time,run_day,nr,ncell,ns,T_0,T_head,dbt,Q_inflow,Q_outflow)
end if
#endif
end SUBROUTINE WRITE
