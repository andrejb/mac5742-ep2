FC = mpif90
FCFLAGS = -g
NPROC = 10
FRAC = 0.5

all: sorteio

clean:
	rm -f $(EXAMPLES) *~ *.o sorteio

sorteio: src/sorteio.f90
	$(FC) $(FCFLAGS) $^ -o $@

run: sorteio
	mpirun -np $(NPROC) $< $(FRAC)

edit:
	vim -p src/* Makefile

stats:
	for j in `seq 1 4`; do for i in `seq 10 5 150`; do make run NPROC=$$i FRAC=0.$$j | awk '{total+=$$1} END {print '$$i' " " total}'; done > /tmp/testes/teste-0.$$j.txt; done

.PHONY: all clean run edit stats
