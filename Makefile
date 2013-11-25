default:
	cd gnat && $(MAKE)

check:
	cd gnat && $(MAKE) check

html:
	cd gnat && $(MAKE) html

install:
	cd gnat && $(MAKE) install

clean:
	cd gnat && $(MAKE) clean
