FC = mpif90
FCFLAGS = -g
NPROC = 10
FRAC = 0.5

all: sorteio

clean:
	rm -f $(EXAMPLES) *~ *.o

sorteio: src/sorteio.f90
	$(FC) $(FCFLAGS) $^ -o $@

run: sorteio
	mpirun -np $(NPROC) $< $(FRAC)

edit:
	vim -p src/* Makefile

.PHONY: all clean run edit
