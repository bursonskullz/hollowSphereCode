subroutine set_moment_Array(internal,ratio,extern,increment,maxnt,Moment_Array)
    !use HollowSphere_micromorphic
    use HollowSphere_micromorphic_porous
    use Math_lib
    double precision            :: inc_for_i, inc_for_jay 
    integer                     :: j, i
    real, intent(in)            :: internal,ratio,extern,increment
    real, dimension(maxnt)      :: f_array, x
    real, dimension(maxnt)      :: integrand, Moment_Intern, Moment_Array  !
    real                        :: t1,t2,t3 !!! should not need particular if in main, that is if they are public 


!=========================================== 2d data =========
end subroutine set_moment_Array