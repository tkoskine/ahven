# Comfignat makefile foundation for configuring and building GNAT projects
# Copyright 2013 B. Persson, Bjorn@Rombobeorn.se
#
# This material is provided as is, with absolutely no warranty expressed
# or implied. Any use is at your own risk.
#
# Permission is hereby granted to use or copy this makefile
# for any purpose, provided the above notices are retained on all copies.
# Permission to modify the code and to distribute modified code is granted,
# provided the above notices are retained, and a notice that the code was
# modified is included with the above copyright notice.


# This file is part of Comfignat – common, convenient, command-line-controlled
# compile-time configuration of software built with the GNAT tools. For more
# information about Comfignat, see http://www.Rombobeorn.se/Comfignat/.


# This file contains generic Make code. It is designed to be included by other
# makefiles, called containing makefiles, which add information specific to the
# project at hand. Builds are controlled by GNAT project files which import the
# abstract project Comfignat and use the directory variables it defines. For
# libraries there shall also be usage projects to be installed on the target
# system. Usage projects and the Comfignat project will be preprocessed with
# Gnatprep. (Build projects may also be preprocessed.)
#
# If a directories project is provided, then the project files will get the
# directory variables from there, otherwise the Make variables will be used.
#
# This file may not work with other Make clones than GNU Make. (Reusable Make
# code is pretty much impossible to write without advanced Make features.) If
# Make cannot be used for whatever reason, then it's not too difficult to run
# the project files through Gnatprep manually.


#
# Program-name variables and the usual options variables are picked up from the
# environment or the command line:
#

GNATPREP     ?= gnatprep
GNAT_BUILDER ?= gprbuild

# If GNAT_BUILDER looks like it will invoke Gnatmake, then make the default
# value of GNATFLAGS compatible with Gnatmake. Otherwise make it suitable for
# building multi-language projects with GPRbuild.
GNATFLAGS ?= ${if ${findstring gnatmake, \
                               ${notdir ${call mung_string,${GNAT_BUILDER}}}}, \
                  ${GNAT_BUILDER_FLAGS} \
                     -cargs ${ADAFLAGS} \
                     -bargs ${GNATBINDFLAGS} \
                     -largs ${GNATLINKFLAGS} ${LDFLAGS}, \
                  ${GNAT_BUILDER_FLAGS} \
                     -cargs:Ada ${ADAFLAGS} \
                     -cargs:C ${CPPFLAGS} ${CFLAGS} \
                     -cargs:C++ ${CPPFLAGS} ${CXXFLAGS} \
                     -cargs:Fortran ${FFLAGS} \
                     -bargs ${GNATBINDFLAGS} \
                     -largs ${LDFLAGS}}

# (DESTDIR is also supported.)

# Containing makefiles may assign default values to the options variables
# GNAT_BUILDER_FLAGS, ADAFLAGS, CPPFLAGS, CFLAGS, CXXFLAGS, FFLAGS,
# GNATBINDFLAGS, GNATLINKFLAGS and LDFLAGS if they are undefined in the
# environment, but should expect that users and distributions may override
# those defaults.


#
# These variables should be overridden on the command line as needed, but will
# not be picked up from the environment:
#

dirgpr =
# dirgpr should be the filename of the target system's directories project if
# there is one. The Gnatprep symbols Directories_GPR and Directories_Project
# will be derived from dirgpr, and project files will be configured to use the
# directories project.

relocatable_package = false
# If relocatable_package is true, then directory variables in project files
# will be configured with relative pathnames so that the installed directory
# tree as a whole can be moved to another location in the filesystem without
# breaking the project files.
# dirgpr takes precedence over relocatable_package.

prefix        = /usr/local
exec_prefix   = ${prefix}
datarootdir   = ${prefix}/share
localstatedir = ${prefix}/var
# These variables are used in constructing the default values of the directory
# variables below.

bindir     = ${exec_prefix}/bin
libexecdir = ${exec_prefix}/libexec
datadir    = ${datarootdir}
sysconfdir = ${prefix}/etc
statedir   = ${localstatedir}/lib
cachedir   = ${localstatedir}/cache
logdir     = ${localstatedir}/log
includedir = ${prefix}/include
libdir     = ${exec_prefix}/lib
gprdir     = ${datarootdir}/gpr
localedir  = ${datarootdir}/locale
mandir     = ${datarootdir}/man
infodir    = ${datarootdir}/info
miscdocdir = ${datarootdir}/doc
runtimedir = /run
lockdir    = ${runtimedir}/lock
# These are the directories where different kinds of files will be located on
# the target system.

builddir = ${CURDIR}
objdir   = ${builddir}/obj
stagedir = ${builddir}/stage
# builddir is the build directory, which may be separate from the source tree.
# Intermediate files produced during the build are kept in objdir. Files to be
# installed are written under stagedir, and then copied to their destination in
# the installation step.

# Containing makefiles should avoid modifying the directory variables. Users
# should be able to rely on these defaults.

install_cp_flags = ${if ${DESTDIR},--preserve=timestamps}
# Timestamps are preserved when installation is done to a staging directory.
# This matters for files that aren't generated during the build but copied from
# the source tree. Timestamps are not preserved when installation is done
# directly to the target system, because that would change the timestamps of
# existing directories.


#
# Containing makefiles may use these variables in their rules, but nothing
# should modify them:
#

srcdir := ${abspath ${dir ${lastword ${MAKEFILE_LIST}}}}
# srcdir is the root of the source tree, computed as the directory part of the
# last pathname in MAKEFILE_LIST – which is this file since there is no include
# directive above this point.

stage_bindir     = ${stagedir}${bindir}
stage_libexecdir = ${stagedir}${libexecdir}
stage_datadir    = ${stagedir}${datadir}
stage_sysconfdir = ${stagedir}${sysconfdir}
stage_statedir   = ${stagedir}${statedir}
stage_cachedir   = ${stagedir}${cachedir}
stage_logdir     = ${stagedir}${logdir}
stage_includedir = ${stagedir}${includedir}
stage_libdir     = ${stagedir}${libdir}
stage_gprdir     = ${stagedir}${gprdir}
stage_localedir  = ${stagedir}${localedir}
stage_mandir     = ${stagedir}${mandir}
stage_infodir    = ${stagedir}${infodir}
stage_miscdocdir = ${stagedir}${miscdocdir}
# These are the directories where different kinds of files to be installed are
# written during the build.

preprocess_file = "${GNATPREP}" ${firstword ${filter %.gp,$^}} $@ \
                  ${options_preprocessing} ${Gnatprep_arguments} \
                  ${if ${filter ${notdir $@},${notdir ${usage_GPRs}}}, \
                       ${usage_directories}, \
                       '-DSrcdir="${srcdir}"'}
# preprocess_file is a command for use in recipes. It runs the first .gp file
# among the rule's prerequisites through Gnatprep to produce the target. If the
# target is a usage project, then the usage-relevant directory variables are
# conveyed to it as Gnatprep symbols. Otherwise srcdir is conveyed.

build_GPR = "${GNAT_BUILDER}" -P ${firstword ${filter %.gpr,$^}} \
            -aP${srcdir} -aP${builddir} -p \
            ${options_building} ${builder_arguments} ${GNATFLAGS}
# build_GPR is a command for use in recipes. It performs a build controlled by
# the first project file among the rule's prerequisites.


#
# Containing makefiles should assign or append to these variables as needed:
#

ifneq (${origin build_GPRs},file)
   build_GPRs =
endif
# build_GPRs shall name one or more project files for building the software.
# These project files will be used when "make" or "make build" is invoked.

ifneq (${origin usage_GPRs},file)
   usage_GPRs =
endif
# If the build produces libraries, then usage_GPRs shall name the project files
# that other projects should import to link to the libraries. These project
# files will be installed to the target system.

ifneq (${origin preprocessed_files},file)
   preprocessed_files = \
      ${filter-out ${notdir ${usage_GPRs}}, \
                   ${basename ${notdir ${wildcard ${srcdir}/*.gp}}}}
endif
# preprocessed_files is a list of files to be produced in the preprocessing
# step at the beginning of the build. Containing makefiles may override it or
# append additional filenames to it.
# The files are assumed to be needed during the build. The default list is all
# the .gp files in srcdir except for usage projects, minus the .gp suffix. This
# includes comfignat.gpr.

ifneq (${origin options},file)
   options =
endif
# options may be assigned a list of variable names. Those variables may be
# overridden on the command line, and will be defined as Gnatprep symbols.
# Their values must be "true" or "false".
# The containing makefile should assign a default value to each variable unless
# it shall be mandatory to always set the option on the command line.

ifneq (${origin Gnatprep_arguments},file)
   Gnatprep_arguments =
endif
# Any text assigned to Gnatprep_arguments will be included in the Gnatprep
# command line. It may be used for additional symbol definitions.

ifneq (${origin builder_arguments},file)
   builder_arguments =
endif
# Any text assigned to builder_arguments will be included in the GPRbuild or
# Gnatmake command line. It may be used for external variables for project
# files or other arguments that are essential for the build to work. Global
# default values for optional arguments should be set in the options variables
# instead.

VPATH += ${srcdir} ${builddir}
# VPATH is a list of directories that Make should search for prerequisites.

configuration_variables += \
   GNATPREP GNAT_BUILDER \
   GNAT_BUILDER_FLAGS ADAFLAGS CPPFLAGS CFLAGS CXXFLAGS FFLAGS GNATBINDFLAGS \
   GNATLINKFLAGS LDFLAGS GNATFLAGS \
   DESTDIR \
   dirgpr relocatable_package \
   prefix exec_prefix datarootdir localstatedir \
   bindir libexecdir datadir sysconfdir statedir cachedir logdir includedir \
   libdir gprdir localedir mandir infodir miscdocdir runtimedir lockdir \
   objdir stagedir \
   install_cp_flags \
   ${options}
# configuration_variables is a list of variables that can be saved in the
# persistent configuration with "make configure". Containing makefiles may
# append additional variable names.


#
# Read the configuration file if there is one:
#

configuration = ${builddir}/comfignat_configuration.mk

-include ${configuration}


#
# Ensure that builddir, objdir and stagedir are absolute pathnames:
#

ifeq (${origin builddir},command line)
   override builddir := ${abspath ${builddir}}
endif
ifeq (${origin objdir},command line)
   override objdir := ${abspath ${objdir}}
   objdir_is_configured = true
endif
ifeq (${origin stagedir},command line)
   override stagedir := ${abspath ${stagedir}}
   stagedir_is_configured = true
endif
# These pathnames need to be absolute in project files, because a pathname
# relative to a project file can be wrong when a separate build directory is
# used and project files are both in srcdir and in builddir. objdir and
# stagedir also need to be absolute in the configuration file because the
# working directory might change between Make invocations.
# Once modified the variables are no longer of command line origin, so they are
# marked as configured so that "make configure" will save them.


#
# Compute symbol definitions for Gnatprep and external variable assignments for
# build projects:
#

nil =
inert_space = _Comfignat_magic_protective_space_character_substitute_
inert_percent = _Comfignat_magic_protective_percent_character_substitute_
mung_string = ${subst %,${inert_percent},${subst ${nil} ,${inert_space},${1}}}
unmung_string = ${subst ${inert_percent},%,${subst ${inert_space}, ,${1}}}
# mung_string and unmung_string are used to prevent Make from interpreting
# space and percent characters in strings.

relativize = ${if ${filter ${2}%,${1}}, \
                  ${3}${1:${2}%=%}, \
                  ${call relativize,${1},${dir ${2:%/=%}},${3}../}}
# relativize is the recursive algorithm that converts an absolute pathname into
# a relative one.
# Parameters:
#    1: an absolute pathname to convert to relative
#    2: the absolute base pathname, being shortened until it's a prefix of 1
#    3: a growing series of "../" to lead the relative pathname with
# If 2 is a prefix of 1, then return 3 concatenated with the part of 1 that
# differs from 2. Otherwise delete the last element of 2, add one level of
# "../" to 3, and repeat.
# Within relativize all pathnames have one trailing slash so that only whole
# directory names will match. Otherwise "/usr/lib" could match "/usr/lib64" for
# example.

prepare_pathname = ${subst //,/,${abspath ${call mung_string,${1}}}/}
# prepare_pathname prepares a pathname for use as a parameter to relativize.
#    · Protect space and percent characters from interpretation by Make.
#    · Normalize the pathname, eliminating ".", ".." and "//".
#    · Append a slash.
#    · If the input was "/", then it is now "//". Change that back to "/".

relative_to = \
   ${or ${call unmung_string \
              ,${patsubst %/,%,${call relativize \
                                     ,${call prepare_pathname,${1}} \
                                     ,${call prepare_pathname,${2}},}}},.}
# relative_to converts an absolute pathname into a relative one. What it
# actually does is to prepare the input to relativize and fix up its output.
# Parameters:
#    1: an absolute pathname to convert to relative
#    2: the absolute base pathname that 1 shall be made relative to
# Processing:
#    · Prepare the two input pathnames with prepare_pathname.
#    · Call relativize with the prepared pathnames for parameters 1 and 2, and
#      an empty string for 3.
#    · Strip the result of surrounding spaces and the trailing slash.
#    · Reverse the protection of space and percent characters.
#    · If the result is an empty string, then return "." instead.

maybe_relative_to = \
   ${if ${or ${filter-out 1,${words ${relocatable_package}}}, \
             ${filter-out true false,${relocatable_package}}}, \
        ${error relocatable_package must be "true" or "false"} \
       ,${if ${filter true,${relocatable_package}} \
            ,${call relative_to,${1},${2}},${1}}}
# maybe_relative_to converts an absolute pathname into a relative one if a
# relocatable package is desired.
# Parameters:
#    1: an absolute pathname to convert to relative
#    2: the absolute base pathname that 1 may be made relative to
# First check that the value of relocatable_package is a single word and that
# that word is either "true" or "false". Complain and stop if that isn't so.
# Then, if relocatable_package is "true", let relative_to convert the pathname,
# otherwise return parameter 1 unchanged.

embed_pathname = ${call maybe_relative_to,${1},${bindir}}
# embed_pathname converts an absolute pathname into the right form for
# inclusion in a program, which means that it is made relative to bindir if a
# relocatable package is desired.

usage_pathname = ${call maybe_relative_to,${1},${gprdir}}
# usage_pathname converts an absolute pathname into the right form for
# inclusion in a usage project, which means that it is made relative to gprdir
# if a relocatable package is desired.

# Convey builddir, objdir and stagedir to comfignat.gpr.
all_directories = '-DBuilddir="${builddir}"' '-DObjdir="${objdir}"' \
                  '-DStagedir="${stagedir}"'

# Convey the builder-irrelevant directory variables, making them available to
# build projects for inclusion in binaries. Make most of the pathnames relative
# if a relocatable package is desired.
all_directories += '-DDatadir="${call embed_pathname,${datadir}}"'
all_directories += '-DSysconfdir="${call embed_pathname,${sysconfdir}}"'
all_directories += '-DStatedir="${call embed_pathname,${statedir}}"'
all_directories += '-DCachedir="${call embed_pathname,${cachedir}}"'
all_directories += '-DLogdir="${call embed_pathname,${logdir}}"'
all_directories += '-DGPRdir="${call embed_pathname,${gprdir}}"'
all_directories += '-DLocaledir="${call embed_pathname,${localedir}}"'
all_directories += '-DMandir="${call embed_pathname,${mandir}}"'
all_directories += '-DInfodir="${call embed_pathname,${infodir}}"'
all_directories += '-DMiscdocdir="${call embed_pathname,${miscdocdir}}"'
all_directories += '-DRuntimedir="${runtimedir}"'
all_directories += '-DLockdir="${lockdir}"'
# runtimedir and lockdir belong to the operating system and are used for
# communication between subsystems. It wouldn't make sense for an application
# to have its own runtimedir. Therefore these variables are always absolute
# pathnames.

ifeq (${dirgpr},)

   # No directories project was provided, so convey even the builder-relevant
   # directory variables to comfignat.gpr, and convey the usage-relevant ones
   # to usage projects in the form that usage projects need.

   all_directories += '-DBindir="${call embed_pathname,${bindir}}"'
   all_directories += '-DLibexecdir="${call embed_pathname,${libexecdir}}"'
   all_directories += '-DIncludedir="${call embed_pathname,${includedir}}"'
   all_directories += '-DLibdir="${call embed_pathname,${libdir}}"'

   all_directories += '-DStage_Bindir="${stage_bindir}"'
   all_directories += '-DStage_Libexecdir="${stage_libexecdir}"'
   all_directories += '-DStage_Includedir="${stage_includedir}"'
   all_directories += '-DStage_Libdir="${stage_libdir}"'

   usage_directories = '-DIncludedir="${call usage_pathname,${includedir}}"' \
                       '-DLibdir="${call usage_pathname,${libdir}}"'

else

   # A directories project is used, so make project files take the builder-
   # relevant directory variables from there.

   directories_project := ${basename ${notdir ${call mung_string,${dirgpr}}}}

   all_directories += '-DDirectories_GPR="${dirgpr}"'
   all_directories += '-DDirectories_Project=${directories_project}'
   all_directories += '-DBindir=${directories_project}.Bindir'
   all_directories += '-DLibexecdir=${directories_project}.Libexecdir'
   all_directories += '-DIncludedir=${directories_project}.Includedir'
   all_directories += '-DLibdir=${directories_project}.Libdir'

   usage_directories = '-DDirectories_GPR="${dirgpr}"' \
                       '-DIncludedir=${directories_project}.Includedir' \
                       '-DLibdir=${directories_project}.Libdir'

endif

option_values = \
   ${foreach option,${options}, \
             ${if ${and ${filter-out environment,${origin ${option}}}, \
                        ${filter 1,${words ${${option}}}}, \
                        ${filter true false,${${option}}}}, \
                  ${option}=${${option}}, \
                  ${error ${option} must be "true" or "false"}}}
# For each variable listed in options, check that it didn't come from the
# environment (to prevent accidents), that its value is a single word, and that
# that word is either "true" or "false". If so, output a name/value pair;
# otherwise complain and stop.

# Convey boolean options to Gnatprep.
options_preprocessing = ${addprefix -D,${option_values}}

# Convey boolean options to build projects.
options_building = ${addprefix -X,${option_values}}


#
# Some other data that the rules below need:
#

delegation_command := @$${MAKE} --file=${abspath ${firstword ${MAKEFILE_LIST}}}\
                     --include-dir=${srcdir} --no-print-directory
# delegation_command is the Make command line that delegating makefiles in
# separate build directories use to delegate commands to the main makefile. The
# first pathname in MAKEFILE_LIST is the main makefile.

build_targets = ${addsuffix .phony_target,${build_GPRs}}
# A phony target is defined for each build project, and the job of determining
# whether the project needs rebuilding is delegated to the builder.

staged_usage_GPRs = ${addprefix ${stage_gprdir}/,${usage_GPRs}}
preprocessed_files_in_builddir = ${addprefix ${builddir}/,${preprocessed_files}}
# When usage projects are preprocessed they are written to stage_gprdir. Other
# preprocessed files are assumed to be needed during the build and are written
# to builddir.


#
# Make rules:
#

.SECONDEXPANSION:

Comfignat_default_goal: build

# How to make directories:
${builddir} ${stage_gprdir}:
	mkdir -p $@

# How to initialize a build directory with a delegating makefile:
${builddir}/Makefile: | ${builddir}
	@echo 'Writing $@.'
	@( echo 'Comfignat_default_goal: force ; ${delegation_command}'; \
	   echo '%: force ; ${delegation_command} $$@'; \
	   echo 'force: ;'; \
	   echo 'Makefile: ;' \
	 ) > $@
# This rule generates a delegating makefile in a separate build directory. The
# generated makefile delegates all commands to the main makefile. The default
# rule invokes the main makefile without a specified goal, triggering the main
# makefile's default goal. A match-anything rule forwards any specified goals
# to the main makefile. An empty recipe for "Makefile" prevents Make from using
# the match-anything rule to update the makefile.

# How to save configured variables:
configure:: ${builddir}/Makefile
	@echo "Writing ${configuration}."
	@( ${foreach variable,${configuration_variables}, \
	             ${if ${or ${findstring command line, \
	                                    ${origin ${variable}}}, \
	                       ${filter true,${${variable}_is_configured}}}, \
	                  echo 'ifneq "$${origin ${variable}}" "command line"';\
	                  echo 'override ${variable} = ${value ${variable}}'; \
	                  echo 'endif'; \
	                  echo '${variable}_is_configured = true';}} \
	   true \
	 ) > "${configuration}"
# Out of the variables listed in configuration_variables, all that were
# overridden on the command line, and all that were previously configured, are
# written to the configuration file. Configured values override defaults
# defined later in the containing makefile, but can be overridden on the
# command line. A variable is considered previously configured if there is
# another variable with "_is_configured" appended to its name and a value of
# "true". Such a variable is also written for each configured variable. As a
# side effect of this it is possible to delete a variable V from the
# configuration by running "make configure V_is_configured=false".

# How to preprocess the project Comfignat:
${builddir}/comfignat.gpr: comfignat.gpr.gp | ${builddir}
	"${GNATPREP}" $< $@ -DInvoked_By_Makefile ${all_directories}

# How to preprocess files that are needed during the build:
${builddir}/%: %.gp | ${builddir}
	${preprocess_file}

# How to preprocess usage projects:
${stage_gprdir}/%: %.gp | ${stage_gprdir}
	${preprocess_file}

# How to stage usage projects that don't need preprocessing:
${stage_gprdir}/%: % | ${stage_gprdir}
	cp -p $< $@

preprocess: $${preprocessed_files_in_builddir}

# How to build a project:
%.gpr.phony_target: %.gpr preprocess
	${build_GPR}
# Instead of tracking dependencies between project files, this rule simply
# requires that all preprocessing of files that are needed during the build is
# done before any project is built.

build: $${build_targets} $${staged_usage_GPRs}

${stagedir}:
	@${MAKE} build --no-print-directory
# "make install" straight out of a source package triggers a build, but if
# something has been built then "make install" doesn't rebuild anything, just
# copies the built files to their destination.

# How to install what has been built and staged:
install: ${stagedir}
	mkdir -p "${DESTDIR}/"
	cp -RPf ${install_cp_flags} "${stagedir}"/* "${DESTDIR}/"
.PHONY: install

clean::
	rm -Rf "${objdir}" "${stagedir}" ${preprocessed_files_in_builddir}

unconfigure::
	rm -f "${configuration}"

distclean: clean unconfigure
