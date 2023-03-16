CC       = gcc
FC       = gfortran
LINK     = $(FC)

FCFLAGS  = -O3 -fopenmp
CFLAGS   = -O3 -fopenmp -Wunknown-pragmas
LDFLAGS  = $(FCFLAGS) 
LBLAS    = -llapack -lblas -L/usr/local/lib

OBJS = aux.o auxf.o lu_seq.o lu_par_tasks.o kernels.o main.o trace.o

main: clean $(OBJS)
	$(LINK) $(LDFLAGS) -o $@ $(OBJS) $(LBLAS) 

main_dbg: 
	($(MAKE) clean main CDEFS=-DDBG)

clean:
	(rm -f *.o *.svg main)



%.o: %.c
	$(CC)  $(CFLAGS)  $(CINCLUDES) $(INCLUDES) $(CDEFS) $(PREC) -c $<
%.o: %.f90				       
	$(FC)  $(FCFLAGS) $(FINCLUDES) $(INCLUDES) $(FDEFS) $(DEFINE_PREPEND)$(PREC) -c $<




