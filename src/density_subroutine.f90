subroutine stream_density(temp_x,density_x)
    use data_type
    implicit none
    !integer :: res_no
    real(dp) :: density_x
    real :: temp_x
    real(dp) :: para1 = 1.000028*1e-3
    real(dp) :: para2 = 999.83952
    real(dp) :: para3 = 16.945176
    real(dp) :: para4 = 7.9870401e-3
    real(dp) :: para5 = -46.170461e-6
    real(dp) :: para6 = 105.56302e-9
    real(dp) :: para7 = -280.54235e-12
    real(dp) :: para8 = 16.87985e-3
    real(dp) :: para9 = 1.0
    !
    !-------------------calculate the density based on temperature----------------
    !density_x = 1.000028*1e-3*((999.83952+16.945176*temp_x)-& 
    !    & (7.9870401e-3*(temp_x**2)-46.170461e-6*(temp_x**3))+&
    !    & (105.56302e-9*(temp_x**4)-280.54235e-12*(temp_x**5)))/&
    !    & (1+16.87985e-3*temp_x)
    density_x = para1*((para2+para3*temp_x)-&
        & (para4*(temp_x**2)+para5*(temp_x**3))+&
        & (para6*(temp_x**4)+para7*(temp_x**5)))/&
        & (para9+para8*temp_x)
    !if (res_no.eq.42) write(11,*) "temp",temp_x,"density",density_x
!
end subroutine stream_density

