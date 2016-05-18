all: create_gantt.out create_burndown.out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out
install:
	cp gantt.rda ../data
	cp burndown.rda ../data
	
