FORTRAN = f77

SRC = pfgenmdn12081.f blkdta000.f inital.f bilder.f bld141.f gcd.f bldc9.f trvtym.f sortc9.f bldnet.f crtsnk.f blddpb.f crtipn.f crthsp.f crtsup.f crtiap.f bldlst.f bldmis.f crtpod.f crtlcl.f crtdos.f crtc9.f c9only.f wrnode.f wrtarc.f wrtmcc.f dcoder.f init.f insert.f remove.f 

OBJ = pfgenmdn12081.o blkdta000.o inital.o bilder.o bld141.o gcd.o bldc9.o trvtym.o sortc9.o bldnet.o crtsnk.o blddpb.o crtipn.o crthsp.o crtsup.o crtiap.o bldlst.o bldmis.o crtpod.o crtlcl.o crtdos.o crtc9.o c9only.o wrnode.o wrtarc.o wrtmcc.o dcoder.o init.o insert.o remove.o 



pfgen: $(OBJ)              
	$(FORTRAN) -g -o pfgen  $(OBJ)           
#	mv pfgen ../.


.f.o:
	echo "working on file ..." $*.f
	$(FORTRAN) -g -c $*.f	
