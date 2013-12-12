OPTS="GNAT_BUILDER=gnatmake"

default:
	cd gnat && $(MAKE) $(OPTS)

check:
	cd gnat && $(MAKE) $(OPTS) check

check_xml:
	cd gnat && $(MAKE) $(OPTS) check_xml


html:
	cd gnat && $(MAKE) html

install:
	cd gnat && $(MAKE) $(OPTS) install

clean:
	cd gnat && $(MAKE) $(OPTS) clean

control:
	rm -f objects/*.adt objects/*.ali
	cd objects && adactl -f ../rules/ahven.aru ../src/*.ad[bs] ../test/*.ad[bs] ../src/unix/*.ad[bs]
	rm -f objects/*.adt objects/*.ali
