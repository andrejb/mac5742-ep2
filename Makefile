FC = mpif90
FCFLAGS = -g
NPROC = 10
FRAC = 0.5

PKGNAME = mac5742-ep2-andre-patricia
PKGFORMAT = zip

all: sorteio

clean:
	rm -f $(EXAMPLES) *~ *.o sorteio *.mod bin/* *.zip *.tar

sorteio: bin/sorteio.o bin/qsort.o
	${FC} ${FCFLAGS} ${FCOPTFLAGS} -o $@ $^

bin/sorteio.o: bin/qsort.o
bin/%.o: src/%.f90
	$(FC) $(FCFLAGS) -c $< -o $@

qsort_module.mod: src/qsort.f90
	$(FC) $(FCFLAGS) -c $< -o $@

run: sorteio
	mpirun -np $(NPROC) $< $(FRAC)

edit:
	vim -p src/* Makefile

stats:
	for j in `seq 1 4`; do for i in `seq 10 5 150`; do make run NPROC=$$i FRAC=0.$$j | awk '{total+=$$1} END {print '$$i' " " total}'; done > /tmp/testes/teste-0.$$j.txt; done

pkg: clean
	git-archive --format=$(PKGFORMAT) --prefix=$(PKGNAME)/ HEAD > $(PKGNAME).$(PKGFORMAT)
	md5sum $(PKGNAME).$(PKGFORMAT)


.PHONY: all clean run edit stats pkg
