subroutine Set_porous_data(internal,ratio,extern,increment,maxnt,i, C)
    use HollowSphere_micromorphic_porous
    double precision                            :: inc_for_i, inc_for_jay 
    integer                                     :: j, i
    real, intent(in)                            :: internal,ratio,extern,increment
    real             :: c1, c2, c3, c4, c5
    real             :: solution, y_0, C
    real,      dimension(maxnt) :: SEQ,SM,Moment_Array
    integer,          parameter :: file1 = 10, file2 = 20, file3 = 30, file4 = 40, file5 = 50, file6 = 60, file7 = 70
    integer,          parameter :: file8 = 80, file9 = 90


!=========================================== Perform Euler Scheme at each point of increment i =================================================
!===============================================================================================================================================
  
    x(i) = internal + i*increment 
    y_0 = SM_not/(2*porosity)- internal/rho
    if(i.eq.1) then 
        SEQ(i) = SEQ_not
        SM(i) = SM_not 
    else
        SEQ(i) = SEQ(i-1)+f(x(i-1),SEQ(i-1),C)*increment
        SM(i) = SM(i-1) + g(x(i-1),SEQ(i-1),C)*increment
    endif


!=========================================== Write Data out ==================================================================================
!===============================================================================================================================================


    write (file7, *) x(i) ,',', SM(i)
    write (file8, *) x(i) ,',', SEQ(i)
    !write (file3, *) x(i) ,',', y9(i)
    !write (file10, *) x(i) ,',', y13(i)
    !write (file7, *) x(i) ,',', y17(i)
    !write (file8, *) x(i) ,',', y21(i)
    !write (file9, *) x(i) ,',', y25(i)

end subroutine