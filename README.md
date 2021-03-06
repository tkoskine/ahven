
## Ahven - Unit Testing Library for Ada Programming Language

Ahven is a simple unit test library (or a framework) for Ada programming language.
It is loosely modelled after [JUnit](https://junit.org/) and some ideas are taken from AUnit.

Ahven is free software distributed under permissive ISC license and should work
with any Ada 95, 2005, or 2012 compiler.


### Features


* Simple API
* Small size (Ahven about has 3K SLOC)
* JUnit-compatible test results in XML format;
  this allows integration with tools like [Jenkins](https://jenkins.io/) or TeamCity.
* Strict coding style (enforced by AdaControl)
* Plain Ada 95 code, no Ada 2005 features used,
  but can be compiled as Ada 2005 or Ada 2012 code if needed
* Portable across different compilers and operating systems
* Permissive Open Source license (ISC)

#### See also

* Author's blog at https://tero.stronglytyped.org/tag/ahven.html
* fedora/stable build status: [![fedora/stable status](https://builds.sr.ht/~tkoskine/ahven/fedora.yml.svg)](https://builds.sr.ht/~tkoskine/ahven/fedora.yml?)
* debian/stable build status: [![debian/stable status](https://builds.sr.ht/~tkoskine/ahven/debian.yml.svg)](https://builds.sr.ht/~tkoskine/ahven/debian.yml?)

### Platforms

Ahven 2.9 compiles and passes its test suite on following platforms

| OS                    |  Arch  | Compiler               |
|-----------------------|--------|------------------------|
| Fedora Linux 35       | x86_64 | FSF GCC 11.2.1         |
| Debian GNU/Linux 11   | x86_64 | FSF GCC 10             |
| Windows 10            | x86_64 | Janus/Ada 3.2.1a       |
| Windows 11            | x86_64 | GNAT GPL 2020          |


### News

#### Git mirrors available (2022-01-05)

Git mirrors of Ahven's Mercurial repository are available at:

- Sourcehut: https://git.sr.ht/~tkoskine/ahven
- Github: https://github.com/tkoskine/ahven

**NOTE:**
These mirrors might be out of sync occasionally. Please use
the Mercurial repository when contributing patches.

#### Ahven 2.8 (2020-10-17)

This is a minor maintenance release with updated build scripts
and new source code repository location: https://hg.sr.ht/~tkoskine/ahven

#### Source code repository changes to Sourcehut (2019-09-10)

As Bitbucket is shutting down Mercurial version control system support,
the source code repository of Ahven has been changed to Sourcehut.

New repository address is: https://hg.sr.ht/~tkoskine/ahven

#### Ahven 2.7 (2018-07-24)

This is a minor maintenance release with some new features.

In addition to existing comfignat based system, there is now
very simple GNAT project file provided for Ahven library.

The framework internals also got some performance increases
and smaller memory usage for each test.

Set_Up and Tear_Down procedures got improvements and fixes.

#### Ahven 2.6 (2015-08-30)


This release fixes release dates mentioned in the documentation.
Otherwise 2.6 is identical to 2.5.

#### Ahven 2.5 (2015-08-30)

This is a minor maintenance release with some new features.

The comfignat build system/Makefile is now default for GNAT.
Other compilers use their own build mechanisms as before.
In addition, some new command line options for the test runners
were added, see the documentation for details.

The hosting of Ahven's website and release files is changed
to a dedicated site: https://www.ahven-framework.com/

Downloads from Sourceforge are no longer supported.

#### Ahven 2.4 (2014-02-09)


This is a minor maintenance and bug fix release.

Ahven now compiles cleanly with Apex Ada (no errors)
and Irvine ICC Ada (no warnings). In addition,
the documentation was improved and experimental
comfignat-based build system was added.


#### Ahven 2.3 (2013-01-24)

This is a minor feature release.

Starting from this release, the exception backtraces are now
stored to the test results and printed out along with the results.
In addition, the documentation received some improvements and
output of multiline messages from TAP_Runner was fixed.


#### Ahven 2.2 (2012-03-05)

This is a bug fix release.

The release fixes the reporting of skipped tests in Ahven.XML_Runner.
Also, support for GNAT 3.15p was removed. Documentation generation
tool was changed from Adabrowse to [Sphinx](http://www.sphinx-doc.org/).


#### Ahven 2.1 (2011-09-24)

This is a bug fix release.

The release fixes the skipped test reporting in Ahven.Text_Runner.

#### Ahven 2.0 (2011-09-23)

This is a feature release.

The release adds possibility to stop tests after certain amount of time
and programmatically skip tests. In addition, the README document is
now in reStructured text format, like the manual.

#### Ahven 1.9 (2011-04-19)


This is a bug fix release.

The release includes new HTML documentation generated from reStructured text using Python-Sphinx and fixes compilation problems with GNAT GPL 2010.

#### Ahven 1.8 (2010-06-02)

This is a bug fix release.

Changes include a fix for double free when mixing dynamic test cases with static test suites, removal of some unfinished features like TAP 1.3 and Janus/Ada 3.1.1d support, and code cleanups.

#### Ahven website location changed again (2009-11-30)

The website location of Ahven changed once more. This time the change should be the last one for a while. At the same time, the layout was reverted to the older version, which is more friendly to the bandwidth.

Technical detail which should be interesting: The new website is running on Debian and Ada Web Server.

#### Ahven 1.7 (2009-09-14)

This is a bug fix release.

Changes include a fix for Constraint_Error with long test names and
special character filtering from the test names when generating XML results.
In addition, PDF report generation example was added to the contrib directory
and some internal code cleanups were done.

#### Mercurial repository, part 2 (2009-06-25)

Sourceforge.net has had some problems with their Mercurial repositories,
so now the previously unofficial Bitbucket Mercurial repository as
the official Mercurial repository for Ahven.

Also, bug reports are now at Bitbucket.

#### Mercurial repository (2009-03-17)

Sourceforge.net added support for Mercurial and now Ahven's source code repository is migrated from CVS to Mercurial.

#### Ahven 1.6 (2009-02-28)

This release fixes GNAT installation issues.

#### Ahven 1.5 (2009-02-23)

This is first release at SourceForge. The release includes only some build system changes.

#### SourceForge.net (2009-02-18)

Ahven project is now hosted by SourceForge.

#### Ahven 1.4 (2009-01-22)

This release introduces Test Anything Protocol (TAP) reporter, a new API for stack-based test cases, and improved Janus/Ada support. Also, some API changes were done, but they should affect you only if you have extented the framework.

#### Ahven 1.3 (2008-08-13)

A bug fix release. The major change is support for Janus/Ada.
Web site layout changes (2008-06-30)

The web site layout was changed to be "less boring". The new blueish theme should work better on different types of monitors. (Some low quality monitors and graphics cards didn't show light brown colors properly.)

#### Ahven 1.2 (2008-05-12)

A major new feature in this release is support for JUnit-compatible XML-based test result format. The release also includes bug fixes and code cleanups.

#### Ahven 1.1 (2008-01-30)

Incremental release including bug fixes and new features.

#### Ahven 1.0 (2007-10-24)

Initial release. (See [News](https://www.ahven-framework.com/NEWS) for details.)


### Download

Ahven is distributed in source code format only.
You can get the release packages from
https://www.ahven-framework.com/releases/

You can download the latest development source code from
Ahven's Mercurial repository:
https://hg.sr.ht/~tkoskine/ahven

Git mirrors of the Mercurial repository are available at:

- Sourcehut: https://git.sr.ht/~tkoskine/ahven
- Github: https://github.com/tkoskine/ahven

#### Debian package

The following command installs Ahven on Debian GNU/Linux 10 (buster).

```
sudo apt-get install libahven-doc libahven7-dev
```

#### Fedora package

Fedora provides Ahven as *ahven* and *ahven-devel* packages.
One can install the packages with the dnf command:

```
sudo dnf install ahven ahven-devel libgnat-static
```

### Installation


For building Ahven source code you need Ada 95/2005/2012 compiler,
for example GNAT, Janus/Ada, Irvine ICCAda, or ObjectAda.

Optionally, you need Sphinx_ (Python package)
to build the documentation and AdaControl to run coding style checks.

The default Makefile compiles code using gnatmake. Internally, gnatmake is
given a GNAT project file, which works with GNAT GPL series and relatively
recent FSF GNAT.

If you use another compiler, you need to customize the Makefile by yourself.
Please note, that 'src' directory has platform specific subdirectories 'unix'
and 'windows'. You need to select the sources from one of them also.

#### Installation: GNAT

When using GNAT, simple *make* will compile the library.

Command *make check* will compile and run the unit tests.

If you want to build the API documentation, you
need Sphinx_ tool. Command 'make docs' will
build the API documentation.

Installation happens by typing *make install*. This installs that which has
been built by earlier make commands. If nothing has been built, then
*make install* first builds the library, just like a plain *make*, and then
installs that.
Alternatively, you can simply copy the source code directory ('src')
to your project.

If you want to specify the installation directory, you need to
give it during the first *make* via prefix variable.

```
make clean # not necessary for the first build
make prefix=$HOME/my-libraries/ahven
make install
```

#### Installation: Janus/Ada

Build scripts for Janus/Ada are located in the 'janusada' directory.
To compile the source code, you need to first tweak file 'prepare.bat'.
Then run 'prepare.bat' and 'build.bat' from the top level directory.
That is the same directory where this README.md file is located.

Example:

```
janusada\prepare.bat
janusada\build.bat
```

When compilation is finished, you have tap_test.exe in the 'test_obj'
directory.

### Author

Tero Koskinen <tero.koskinen@iki.fi>

![Ahven (perch)](https://www.ahven-framework.com/ahven.png)

