cd ../sorc
ifort -assume byterecl *.f *.F90
mv a.out ../test/noahlsm.x
