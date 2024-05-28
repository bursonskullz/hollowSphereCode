program Stress3dmodule
    character(lEN=80)           :: answer, income 
    integer,          parameter :: U = 1, maxnt = 10000         
    double precision            :: interval_begin, interval 
    integer,          parameter :: file1 = 10, file2 = 20, file3 = 30, file4 = 40, file5 = 50, file6 = 60
    integer,          parameter :: file7 = 70, file8 = 80, file9 = 90
    real,      dimension(100)   :: x,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16 
    real,      dimension(100)   :: y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28 
    real                        :: internal,extern,ratio,increment,b1,b2,b3,b4,UI,Y_Particular,SEQ_not,SM_not, C
    real,      dimension(maxnt) :: f_array 
    real,      dimension(maxnt) :: integrand, Moment_Intern, Moment_Array  ! 
    integer                     :: i,Geo, model, loading
    !This will need to be changed depneding on were the user saves the algorithm ! make it a function
    character(*), parameter :: path1 = 'C:/Users/Roy Burson/Documents/Math research/Resarch on Ductile Rapture/'
    character(*), parameter :: path2 = 'HS-GLPD/glpdMaster/Data/' 
    character(*), parameter :: path = path1//path2


!             6/01/18:    rb - initial creation of program with fortran and gnuplot
!             9/02/18:    rb - added modle type specifications
!             10/17/18:   rb - added 3d plots data generation
!             10/20/18:   rb - added 3d plots data generation 
!             11/20/19:   rb - added matlab code for 2d Graphs
!             4/07/20:    rb - added make file to generate code
!             4/28/20:    rb - cleaned file HollowSphere_micromorphic
!             4/02/20:    rb - added porosity_syttem.f95 file to
!             5/05/20:    rb - modified code to acess external model specifications
!             5/10/20:    rb - modified driver do increments instead of models save time on instatiation
!             5/11/20:    rb - changed some variables to public through the module HollowSphere_micromorphic
!             5/12/20:    rb - made Math_library file to contain general math functions
!             6/01/20:    rb - added GLPD_hollosphere_porosity, HollowSphere_micromorphic_porous, 
!                                                /set_data_porous, and GLPD_hollosphere_porosity
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    write(*,*)"                                                     "
    write(*,*)"                                                     "
    write(*,*)"                                                     "
    write(*,*)"                                                     "
    write(*,*)"                   Roy Burson                        "
    write(*,*)'------------------Copyright@ 2019--------------------'
    write(*,*)"------------This material is classified--------------"
    write(*,*)"                                                     "
    write(*,*)"                Model Specifications                 "
    write(*,*)"    Enter the type of model you would like to use    "
    write(*,*)"                                                     "
    write(*,*)"                                                     "
    write(*,*)"             1) GLPD MODEL                           "
    write(*,*)"             2) RHT MODEL                            " 
    read(*,*)model  
    write(*,*)"                                                     "
    write(*,*)"            Material Type Specifications             "
    write(*,*)" Enter the type of object and loading specifications "
    write(*,*)"                                                     "
    write(*,*)"             1) Hollow Sphere                        "
    write(*,*)"             2) Cube                                 " 
    write(*,*)"             2) Cylinder                             "
    write(*,*)"             3) previous object                      "
    read(*,*)Geo
if(Geo.eq.1) then
    write(*,*)"            Loading Specifications                  "
    write(*,*)"                                                    "
    write(*,*)"         1)     Isotropic Loading                   " 
    write(*,*)"         2)     hydrostatic tension                 " 
    write(*,*)"         3)     uniaxial strain                     " 
    write(*,*)"         4)     triaxial stress                     " 
    write(*,*)"         5)     biaxial stress                      "
    read(*,*)loading
    if(loading.eq.2) then
        write(*,*) "Would you like to assume incompressibility?: YES or NO?"
        write(*,*) "                                                      "
        read(*,*)income
        write(*,*) "Enter an internal radius value:                      "
        read(*,*)internal
        write(*,*) 'Enter a ratio for external radius to internal radius:'
        read(*,*)ratio
        extern = ratio* internal 
        write(*,*) "The interanl radius is:", internal
        write(*,*) "The external radius is:", extern
        increment = (extern-internal)/maxnt

        open(unit = file1, action = 'write', file = path//'function.csv', status = 'replace')
        open(unit = file2, action = 'write', file = path//'Stress_rr_data.csv', status = 'replace')
        open(unit = file3, action = 'write', file = path//'Stress_phiphi_data.csv', status = 'replace')
        open(unit = file4, action = 'write', file = path//'3d_data.csv', status = 'replace')
        open(unit = file5, action = 'write', file = path//'UDDATA.csv', status = 'replace')
        open(unit = file6, action = 'write', file = path//'M_rrr_data.csv', status = 'replace')
        open(unit = file7, action = 'write', file = path//'Sigma_M_data.csv', status = 'replace')
        open(unit = file8, action = 'write', file = path//'Sigma_EQ_data.csv', status = 'replace')

        if(income.eq.'No'.OR. income.eq.'no'.OR.income.eq.'NO') then
            if(model.eq.1.And.loading.eq.2) then
                write(*,*) "Please enter 4 different values for the characterstic length scale"
                write(*,*) "These values must be in the interval [r_i, r_e] "
                read(*,*) b1, b2, b3, b4 ! Note: we should make them automatic
                do i=1, maxnt
                    call GLPD_HollowSphere(internal,ratio,extern,increment,maxnt,i,b1,b2,b3,b4)     ! should call only if compressinle   
                end do
                Write(file5,*) extern, ',', ratio, ',', internal, ',', b1, ',', b2, & 
                & ',', b3, ',', b4   
            else
                write(*,*) "Sorry only hydrostatic testing has been developed thus far"
            end if
        elseif(income.eq.'Yes'.OR. income.eq.'YES'.OR.income.eq.'yes') then
            if(model.eq.1.And.loading.eq.2) then
                    write(*,*) 'Enter a value for initial von mises stres and the mean stress'
                    read(*,*) SEQ_not
                    read(*,*) SM_not
                    write(*,*) 'Enter a value for C'
                    read(*,*) C
                do i=1, maxnt 
                    call Set_porous_data(internal,ratio,extern,increment,maxnt,i,C)
                end do 
                !call set_moment_Array(internal,ratio,extern,increment,maxnt,UI,Moment_Array)
                Write(file5,*) extern, ',', ratio, ',', internal
            else
                write(*,*) "Sorry only hydrostatic testing has been developed"
                write(*,*) "for the HollowSphere only. Please "
            end if
            close(file1)
            close(file2)
            close(file3) 
            close(file4)
            close(file5)
            close(file6)
            close(file7) 
            close(file8)
        end if
    else
    end if
endif
!=========================================================================================
!============================================END OF PROGRAM===============================
end program Stress3dmodule





