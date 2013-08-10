default:
	cd gnat && $(MAKE)

check:
	cd gnat && $(MAKE) check

install:
	cd gnat && $(MAKE) install

clean:
	cd gnat && $(MAKE) clean
