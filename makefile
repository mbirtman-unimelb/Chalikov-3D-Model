#module load ifort
FC = ifort

#-CB: Performs run-time checks on  whether  array  sub script	 and   substring  references  are  within declared bounds. The default is -check nobounds.
#-save: Places variables, except those  declared	as  AUTOMATIC,  in static memory (same as -noauto or -noautomatic). The default is -auto-scalar.  However, if you  specify  -recursive or -openmp, the default is-automatic.
#-autodouble: Makes  default  real  and complex variables 8 bytes long. REAL declarations are treated as DOUBLE  PRECISION  (REAL(KIND=8)) and COMPLEX declarations are treated as DOUBLE COMPLEX	 (COMPLEX(KIND=8)).  This 	      option  is  the same as specifying -real_size 64 or 	      -r8.
#-o2: optimisation, second level
FCFLAGS= -real-size 64 -noauto -o -c #changed autodoudble and -save (updated)
PROGRAMS = mlb_wave3_nw

wave3sv: mlb_wave3_nw.f90
	$(FC) $(FCFLAGS) mlb_wave3_nw.f90 fft4f2d.f90 -o mlb_wave3_nw

clean:
	rm -f *.o
	rm -f *~ $(PROGRAMS)