module HollowSphere_micromorphic_porous
    implicit none  
    public 
    real, parameter  :: sigma_0 =  100
    real, parameter  :: porosity =  0.01
    real, parameter  :: alpha = 3*SQRT(real(3))/2
    real, parameter  :: beta = 2/sigma_0**2
    real, parameter  :: gamma =  3/(2*sigma_0)
    real, parameter  :: rho = 2*porosity*sigma_0**2*gamma


    contains

function f(x,y,C) result(solution)
        real, intent(in) :: x,y,C
        real             :: t1, t2, t3, t4
        real             :: solution, Numerator, Denomenator
        t1 = alpha*(sigma_0-y**2)
        t2 = (2*alpha/x)*y*(sqrt(real(sigma_0-y**2)))
        t3 = SQRT(real(sigma_0-y**2))
        t4 = beta*y
        Numerator = -2*porosity*sinh(gamma*C)+t1-t2
        Denomenator = t3-t4
        if(Denomenator.eq.0) then
            solution = 0
        else
            solution = Numerator/Denomenator
        endif
    end function f

function g(x,y,C) result(solution)
        real, intent(in) :: x,y,C
        real             :: t1, t2
        real             :: solution
        t1 = 2*alpha/x
        t2 = C/(sinh(gamma*y)*rho)+2*porosity/rho
        solution = t1*t2
end function g
!=========================================== 2d data ================================================================
end module