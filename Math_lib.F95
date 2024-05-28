module Math_lib
    implicit none  
    public  
    contains 

    function integrate_array(npts,X,Y) result(integral)
        integer, intent(in)                           :: npts
        real, dimension(npts), intent(in)             :: X,Y
        real, dimension(npts)                          :: integral
        real, dimension(npts)                       :: delta_X,zleft,zright,zaverage
        integer                                       :: i,j
        real                                          :: inc
        
        inc = 0
        do i = 1,npts-1
            delta_X(i) = x(i+1)-x(i)
            zleft(i) = delta_X(i)*y(i)
            zright(i) = delta_X(i)*y(i+1)
            zaverage(i) = 0.5 *(zleft(i) + zright(i))
            inc = inc + zaverage(i)
            integral(i+1) = inc
        end do
        integral(1) = 0
	end function integrate_array

    function integrand_special(npts,X,Y) result(number)
        integer, intent(in)                           :: npts
        real, dimension(npts), intent(in)             :: X,Y
        real                                          :: number
        real, dimension(npts)                         :: delta_X,zleft,zright,zaverage
        integer                                       :: i,j
        real                                          :: inc
        
        inc = 0
        do i = 1,npts-1
            delta_X(i) = x(i+1)-x(i)
            zleft(i) = delta_X(i)*y(i)
            zright(i) = delta_X(i)*y(i+1)
            zaverage(i) = 0.5 *(zleft(i) + zright(i))
            inc = inc + zaverage(i)
        end do
        number = inc
    end function integrand_special
end module