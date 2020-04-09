
OBJ=   jetset.o pepsi423.o lepto.o leptoak.o lpriktak.o lflav.o polleptox.o akdn.o dnstrf.o newpdfs.o parton.o accepteg1.o  clasDIS.o getcos.o elacc_newnew.o lambda.o  write2filed.o write2filep.o radgen.o parjset4hermes.o smear_fun.o accep_fun.o clas_at12g.o read_par_clas12g.o accep11.o

# -lstdc++ for Gagik's stuff
#
FOR   =  -lstdc++  -lgfortran
CERNLIBS =  -L/apps/cernlib/x86_64_rhel6_4.7.2/2005/lib -lmathlib  -lpacklib
  
clasdis : $(OBJ)
	 gfortran -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore  -o	clasdis  $(OBJ)  $(FOR) 
$(OBJ) : %.o: %.F
	gfortran   -c $< -o $@  

clean:
	rm -f clasdis   $(OBJ)



##g77  -O2 -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore \
##        -DLinux \
##        -I. -I./ -I/group/clas/builds/release-4-14/packages/include -I/group/clas/builds/release-4-14/packages/inc_derived -I/apps/tcl/include -I/usr/X11R6/include -c \
##        aac.F  -o /home/avakian/w6/tmp/obj/LinuxRHEL3/generator/aac.o


















