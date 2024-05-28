! Created by Roy Burson
! Purpose: To asses the distribution of stress Enakousta's module
! @copyright. reproduction of this material is prohibited 


module HollowSphere_micromorphic
    implicit none   
    public  
    integer,          parameter :: file1 = 10, file2 = 20, file3 = 30, file4 = 40, file5 = 50, file6 = 60, file7 = 70
    integer,          parameter :: file8 = 80, file9 = 90 ,file10 = 100
    real, parameter  :: A = 0.001 
    real, parameter  :: A_1 = 0.194 ! No unit
    real, parameter  :: A_2 = 6.108 ! No unit
    real, parameter  :: sigma_0 = 100 
    real, parameter  :: porosity = 0.10 ! No unit 
	contains

!==============================================================================================================================================
!=============================Here we define the nessacary functions to compute the function f(r)==============================================
!==============================================================================================================================================

    function eta(b,r,r_i) result(solution)
        real, intent(in):: b
        real, intent(in):: r
        real, intent(in):: r_i
        real           :: solution 
        solution = (A*sigma_0/r**3) * SQRT(real(1+(15*b**2)/(A_2*r**2)))
    end function eta

    function eta_prime(b,r,r_i) result(solution)
        real, intent(in):: b
        real, intent(in):: r
        real, intent(in):: r_i
        real            :: solution 
        solution = - (A*sigma_0)/(A_2) * ((60*b**2+3*A_2*r**2)/(r**4*SQRT(real(1+(15*b**2)/(A_2*r**2)))))
    end function eta_prime

    function eta_double_prime(b,r,r_i) result(solution)
        real, intent(in):: b
        real, intent(in):: r
        real, intent(in):: r_i
        real            :: solution 
        solution = (A*sigma_0)/(A_2**2) * ((12*A_2**2*r**4+ 495*A_2*b**2*r**2+4500*b**4)/(r**9*(1+(15*b**2)/(A_2*r**2)**(3/2))))
    end function eta_double_prime

    function f(b,r,r_i) result(solution)
        real, intent(in):: b
        real, intent(in):: r
        real, intent(in):: r_i
        real            :: solution 
        real            :: t_1
        real            :: t_2
        real            :: t_3
        real            :: t_4
        real            :: t_5
        t_1 = 2*A*sigma_0**2/eta(b,r,r_i)*r**3
        t_2 = (2*A*sigma_0**2*b**2/(A_2*r**4))*(eta_double_prime(b,r,r_i)*eta_prime(b,r,r_i) - eta_prime(b,r,r_i)**2)/(eta(b,r,r_i))
        t_3 = -(A*sigma_0**2 *b**2/(A_2*r**5)) * ((20*eta(b,r,r_i)+8*eta_prime(b,r,r_i)*eta(b,r,r_i))/(eta(b,r,r_i)**2))
        t_4 = -((A*sigma_0**2*b**2/(A_2*r**6))) * (72+2*eta_prime(b,r,r_i)/(eta(b,r,r_i)))
        t_5 = - ((A*sigma_0**2*b**2)/(eta(b,r,r_i)*A_2*r**7))
        solution = t_1 +t_2 +t_3+t_4+t_5
    end function f

!==============================================================================================================================================
!===========================Here we define the nessacary functions to compute the stress and moments===========================================
!==============================================================================================================================================

    function term1(b,r,r_i) result(solution)
        real, intent(in) :: r 
        real, intent(in) :: b 
        real, intent(in) :: r_i
        real 			:: solution 
        if(b==0) then
            solution = 0 
        else
            solution = 2*sigma_0 * (SQRT(real((A_2*r**2 + 15*b**2)/A_2))-SQRT(real((A_2*(r_i)**2 + 15*b**2)/A_2)))
        endif
    end function term1

    function subterm1term2(b,r,r_i) result(solution)
        real, intent(in) :: r 
        real, intent(in) :: b 
        real, intent(in) :: r_i 
        real :: solution 
        real :: t_1
        real :: t_2
        real :: t_3
        real :: t_4
        real :: t_5
        real :: t_6
        if(b==0) then
            solution = 0
        else
            t_1 = - (11*A_2**(9/5)*ATAN((SQRT(real(A_2))*r)/(SQRT(real(15))*b)))/(2*15**(5/2)*b**5)
    		t_2 = (A_2**5 * r)/((450*A_2*b**4*r**2)+(6750*b**6)) - (2*(A_2**4))/(75*b**4*r) 
    		t_3 = (7*A_2**3)/(45*b**2*r**3) - (4*A_2**2)/(r**5)
    		t_4 =  (11*A_2**(9/5)*ATAN((SQRT(real(A_2))*r_i)/(SQRT(real(15))*b)))/(2*15**(5/2)*b**5)
    		t_5 = - (A_2**5 * r_i)/((450*A_2*b**4*r_i**2)+(6750*b**6)) + (2*(A_2**4))/(75*b**4*r_i) 
    		t_6 = - (7*A_2**3)/(45*b**2*r_i**3) + (4*A_2**2)/(r_i**5)
    		solution = t_1 + t_2 + t_3 + t_3+ t_4 + t_5 + t_6
        endif
    end function subterm1term2

    function subterm2term2(b,r,r_i) result(solution)
        real, intent(in) :: r 
        real, intent(in) :: b 
        real, intent(in) :: r_i 
        real :: solution 
        real :: t_1
        real :: t_2
        real :: t_3
        if(b==0) then
         	solution = 0
        else
        	t_1 =  (A_2**3*LOG(A_2*r_i**2 + 15*b**2) - A_2**3*LOG(A_2*r**2 + 15*b**2))/(450* b**4)
    		t_2 = (A_2**3*LOG(r) - A_2**3*LOG(r_i))/(225*b**4) - (A_2**2)/(20*b**2*r_i**2)
    		t_3 = (A_2)/(r_i**4) + (A_2**2)/(30*b**2*r_i**2)- (A_2)/(r**4)
    		solution = t_1 + t_2 + t_3
        endif
    end function subterm2term2

    function term2(b,r,r_i) result(solution)
        real, intent(in) :: r 
        real, intent(in) :: b 
        real, intent(in) :: r_i 
        real :: solution 
        if(b==0) then
            solution = 0
        else
            solution = 2* A * sigma_0**2 * b**2/A_2**3 * (subterm1term2(b,r,r_i) +  subterm2term2(b,r,r_i)/A_2)
        endif
    end function term2


    function subterm1term3(b,r,r_i) result(solution)
        real, intent(in) :: r 
        real, intent(in) :: b 
        real, intent(in) :: r_i 
        real :: solution 
        real :: t_1
        real :: t_2
        if (b==0) then
        	solution = 0
        else
        	t_1 = (3**(3/2) * SQRT(real(5))*(A_2*r**2 + 10*b**2)*r)/((r*SQRT(real(15))*SQRT(real(A_2*r**2+16*b**2)))/(A_2))
        	t_2 = -(3**(3/2) * SQRT(real(5))*(A_2*r_i**2 + 10*b**2)* r_i )/ (r*SQRT(real(15))* SQRT(real(A_2*r_i**2+16*b**2)))/A_2
        	solution =  t_1 + t_2
        endif
    end function subterm1term3

    function subterm2term3(b,r,r_i) result(solution)
        real, intent(in) :: r 
        real, intent(in) :: b 
        real, intent(in) :: r_i 
        real :: solution 
        real :: t_1
        real :: t_2
        real :: t_3
        real :: t_4
        if (b==0) then
        	solution = 0
        else
        	t_1 = (A_2**3 * LOG(A_2*r**2 + 15*b**2) - A_2**3 * LOG(A_2*r_i**2 + 15*b**2))/(6750*b**6)
        	t_2 = (A_2**3*LOG(r_i)- A_2**3*LOG(r))/(3375*b* 6)
        	t_3 = (A_2**2)/(450*b**4*r_i**4) - (A_2)/(60* b**2 *r_i**4) + (2)/(3*r**6)
        	t_4 = - (A_2**2)/(450*b**4*r**4) + (A_2)/(60*b**2*r**4) - (2)/(3* r**6)
        	solution = t_1 + t_2 + t_3 + t_4
        endif
    end function subterm2term3


    function term3(b,r,r_i) result(solution)
        real, intent(in) :: r 
        real, intent(in) :: b 
        real, intent(in) :: r_i 
        real :: solution 
        real :: x_2
        x_2 =  -(2* A**2 * sigma_0**2 * b**2)/A_2
        if(b==0) then
            solution = 0
        else
            solution = (x_2*20/(A*A_2*sigma_0) *subterm1term3(b,r,r_i)) + (x_2 * 8 *  subterm2term3(b,r,r_i))
       endif  

    end function term3

    function subterm1term4(b,r,r_i) result(solution)
        real, intent(in) :: r 
        real, intent(in) :: b 
        real, intent(in) :: r_i 
        real :: solution 
        if (b==0) then
        	solution = 0
        else
        	solution = (A_2/(15*b**2* r *r_i))*(r*SQRT(real((A_2*r_i**2+15*b**2)/A_2)) - r*SQRT(real((A_2*r_i**2+15*b**2)/A_2)))
        endif
    end function subterm1term4

    function subterm2term4(b,r,r_i) result(solution)
        real, intent(in) :: r 
        real, intent(in) :: b 
        real, intent(in) :: r_i 
        real :: solution 
        real :: t_1
        real :: t_2
        real :: t_3
        real :: t_4
        if (b==0) then
        	solution = 0
        else
        	t_1 = (A_2**4*LOG(A_2*r**2 + 15*b**2) -  A_2**4 * LOG(A_2* r_i**2 + 15*b**2))/(6750* b**6)
        	t_2 = (A_2**4*LOG(r_i) - A_2**4*LOG(r))/(3375*b**6)
        	t_3 = (A_2**3)/(450*b**4* r_i**2) - (A_2**2)/(60*b**2 *r_i**4) + (2* A_2)/(3*r_i**6)
        	t_4 = - (A_2**3)/(450*b**4*r**2) + (A_2**2)/(60*b**2*r**4) - (2*A_2)/(3*r**6)
        	solution  = t_1 + t_2 + t_3 + t_4
      	endif
    end function subterm2term4

    function term4(b,r,r_i) result(solution)
        real, intent(in) :: r ! input
        real, intent(in) :: b ! input
        real, intent(in) :: r_i ! input
        real :: solution ! set the data ty
        if(b == 0) then
            solution = 0
        else
            solution = 72*b**2/A_2*(subterm1term4(b,r,r_i) - (2*A*sigma_0 *b*2/A_2**2 * subterm2term4(b,r,r_i)))
        endif
    end function term4

    function term5(b,r,r_i) result(solution)
        real, intent(in) :: r ! input
        real, intent(in) :: b ! input
        real, intent(in) :: r_i ! input
        real :: solution ! set the data ty
        real :: t_1
        real :: t_2
        real :: t_3
        real :: t_4
        real :: t_5
        real :: t_6
        t_1 = -(15*b**2*(LOG((SQRT(A_2)*SQRT(real(A_2*r_i**2+15*b**2))-A_2*r_i)/(A_2*r_i))))/(4*A_2)
        t_2 = -(15*b**2*(LOG((SQRT(A_2)*SQRT(A_2*r_i**2+15*b**2)+A_2*r_i)/(A_2*r_i))))/(4*A_2)
        t_3 = (2*SQRT(A_2)*r_i*SQRT(real(A_2*r_i**2+ 15*b**2)))/(4*A_2)
        t_4 = -(15*b**2*(LOG((SQRT(A_2)*SQRT(A_2*r**2+15*b**2)-A_2*r)/(A_2*r))))/(4*A_2)
        t_5 = (15*b**2*(LOG((SQRT(A_2)*SQRT(A_2*r**2+15*b**2)+A_2*r)/(A_2*r))))/(4*A_2)
        t_6 = (2*SQRT(A_2)*r*SQRT(real(A_2*r**2+ 15*b**2)))/(4*A_2)
        if(b == 0) then
            solution = 0
        else
            solution = 2*sigma_0*b**2* (t_1 + t_2 + t_3 + t_4 + t_4 + t_6)
        endif
    end function term5


!======================================================================================================================
!=====================Here we define compute the first stress invariants sigma_rr component============================
!======================================================================================================================

    function stress_rr_component(b,r,r_i) result(solution)
        real, intent(in) :: r ! input
        real, intent(in) :: b ! input
        real, intent(in) :: r_i ! input
        real :: solution ! set the data 
        solution = term1(b,r,r_i) + term2(b,r,r_i) + term3(b,r,r_i) + term4(b,r,r_i) + term5(b,r,r_i)
    end function stress_rr_component


!======================================================================================================================
!====================Here we define compute the first stress invariants sigma_phiphi component=========================
!======================================================================================================================

    function stress_phiphi_component(b,r,r_i) result(solution)
        real, intent(in) :: r ! input
        real, intent(in) :: b ! input
        real, intent(in) :: r_i! input
        real :: solution ! set the data ty
        solution = stress_rr_component(r,b,r_i) - (sigma_0 * r)/(sqrt(1+(15*b**2)/(A_2*r**2)))
    end function stress_phiphi_component

!==============================================================================================================================================
!=============================Here we define the nessacary functions to compute the strain components==========================================
!==============================================================================================================================================

    function strain_rr(r) result(solution)
        real, intent(in) :: r
        real :: solution
        solution = -2*A/r**3
    end function strain_rr

    function strain_phiphi(r) result(solution)
        real, intent(in) :: r
        real :: solution
        solution = A/r**3
    end function strain_phiphi

    function moment_r_theta_theta(b,r) result(solution)
        real, intent(in) :: r ! input
        real, intent(in) :: b ! input
        real :: solution ! set the data ty
        solution = (sigma_0*b**2*r)/(A_2*sqrt(1+(15*b**2)/(A_2*r**2)))
    end function moment_r_theta_theta

end module


!=====================================================================================================================
!================================== END OF HOLLOWSPHERE_micromorphic =================================================
