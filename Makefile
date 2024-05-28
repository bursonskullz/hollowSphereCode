MaterialModelDriver: Math_lib.o DriverNew.o HOLLOWSPHERE.o HollowSphere_micromorphic.o\
	HollowSphere_micromorphic_porous.o set_moment_Array.o Set_porous_data.o
	gfortran  Math_lib.f95 DriverNew.f95 HOLLOWSPHERE.f95 HollowSphere_micromorphic.f95\
		HollowSphere_micromorphic_porous.f95 set_moment_Array.f95 Set_porous_data.f95 -o MaterialModelDriver  

Math_lib.mod: Math_lib.o 
	gfortran -c Math_lib.F95

Math_lib.o: Math_lib.F95
	gfortran -c Math_lib.F95

HollowSphere_micromorphic_porous.mod: HollowSphere_micromorphic_porous.o
	gfortran -c HollowSphere_micromorphic_porous.f95 

HollowSphere_micromorphic_porous.o: HollowSphere_micromorphic_porous.f95
	gfortran -c HollowSphere_micromorphic_porous.f95 

HollowSphere_micromorphic.mod: HollowSphere_micromorphic.o 
	gfortran -c HollowSphere_micromorphic.f95

HollowSphere_micromorphic.o: HollowSphere_micromorphic.f95
	gfortran -c HollowSphere_micromorphic.f95

HOLLOWSPHERE.o: HollowSphere_micromorphic.o 
	gfortran -c HOLLOWSPHERE.f95
	
set_moment_Array.o: HollowSphere_micromorphic_porous.o set_moment_Array.f95
	gfortran -c set_moment_Array.f95

Set_porous_data.o: HollowSphere_micromorphic.o HollowSphere_micromorphic_porous.o Set_porous_data.f95
	gfortran -c Set_porous_data.f95

DriverNew.o: Math_lib.o HOLLOWSPHERE.o set_moment_Array.o Set_porous_data.o DriverNew.F95
	gfortran -c DriverNew.F95

clean:
	rm Math_lib.o DriverNew.o HOLLOWSPHERE.o HollowSphere_micromorphic.o HollowSphere_micromorphic_porous.mod\
		HollowSphere_micromorphic_porous.o HollowSphere_micromorphic.mod MaterialModelDriver