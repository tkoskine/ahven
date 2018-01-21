#
# Copyright (c) 2007-2016 Tero Koskinen <tero.koskinen@iki.fi>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
#
# This Makefile is meant for Linux systems and uses
# comfignat.mk settings from gnat_linux/ directory.
#
# For more simple build process, use GNAT project files
# directly from gnat/ directory.
#

# By default, invoke Comfignat's default goal, which builds and stages the
# library, including the usage project file, but not the documentation.
default:
	${MAKE} --directory=gnat_linux

# Any goal that isn't named here is delegated to gnat_linux/Makefile.
# Variables defined on the command line are passed along.
%:
	${MAKE} --directory=gnat_linux $@

README.html: README.rst
	rst2html --stylesheet-path=css/html4css1.css,css/my-docutils.css README.rst > README.html

control:
	rm -f objects/*.adt objects/*.ali
	cd objects && adactl -f ../rules/ahven.aru ../src/*.ad[bs] ../test/*.ad[bs] ../src/unix/*.ad[bs]
	rm -f objects/*.adt objects/*.ali
