subroutine GLPD_HollowSphere(internal,ratio,extern,increment,maxnt,i,b1,b2,b3,b4)
    use HollowSphere_micromorphic 
    double precision             :: inc_for_i, inc_for_jay  
    integer                     :: j
    real, intent(in) :: internal,ratio,extern,increment,b1,b2,b3,b4
!=========================================== 2d data ================================================================
    
    x(i) = internal + i*increment    
    y1(i) = f(real(b1),real(internal + i*increment), real(internal))
    y2(i) = f(real(b2),real(internal + i*increment), real(internal))
    y3(i) = f(real(b3),real(internal + i*increment), real(internal))
    y4(i) = f(real(b4),real(internal + i*increment), real(internal))
    y5(i) = stress_rr_component(real(b1),real(internal + i*increment), real(internal))
    y6(i) = stress_rr_component(real(b2),real(internal + i*increment), real(internal))
    y7(i) = stress_rr_component(real(b3),real(internal + i*increment), real(internal))
    y8(i) = stress_rr_component(real(b4),real(internal + i*increment), real(internal))
    y9(i) = stress_phiphi_component(real(b1),real(internal + i*increment), real(internal))
    y10(i) = stress_phiphi_component(real(b2),real(internal + i*increment), real(internal))
    y11(i) = stress_phiphi_component(real(b3),real(internal + i*increment), real(internal))
    y12(i) = stress_phiphi_component(real(b4),real(internal + i*increment), real(internal))
    write (file1, *) x(i) ,',', y1(i),',',  y2(i),',', y3(i),',', y4(i)
    write (file2, *) x(i) ,',', y5(i),',',  y6(i),',', y7(i),',', y8(i)
    write (file3, *) x(i) ,',', y9(i),',',  y10(i),',', y11(i),',', y12(i)
!=========================================== 3d data ================================================================
    inc_for_i = i*increment

        do j = 1, maxnt
            inc_for_jay = j*increment
            write(file4,*) internal + inc_for_i,',', internal + inc_for_jay, & 
            & ',', stress_rr_component(real(internal+ inc_for_i),real(internal + inc_for_jay), real(internal)), &
            & ',', stress_phiphi_component(real(internal+ inc_for_i),real(internal + inc_for_jay), real(internal)), &
            & ',', moment_r_theta_theta(real(internal+ inc_for_i),real(internal + inc_for_jay))
        end do
!========================================= close files ==============================================================

!=====================================================================================================================
!===================================== END OF HOLLOWSPHERE ===========================================================

end subroutine