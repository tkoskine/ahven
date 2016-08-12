# Comfignat makefile foundation for configuring and building GNAT projects
# Copyright 2013 - 2016 B. Persson, Bjorn@Rombobeorn.se
#
# This material is provided as is, with absolutely no warranty expressed
# or implied. Any use is at your own risk.
#
# Permission is hereby granted to use or copy this makefile
# for any purpose, provided the above notices are retained on all copies.
# Permission to modify the code and to distribute modified code is granted,
# provided the above notices are retained, and a notice that the code was
# modified is included with the above copyright notice.


# This file is part of Comfignat 1.5 beta – common, convenient, command-line-
# controlled compile-time configuration of software built with the GNAT tools.
# For information about Comfignat, see http://www.Rombobeorn.se/Comfignat/.


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
# First of all, define some functions and constants for processing directory
# variables:
#

nil =
inert_space = _Comfignat_magic_protective_space_character_substitute_
inert_tab = _Comfignat_magic_protective_tab_character_substitute_
inert_percent = _Comfignat_magic_protective_percent_character_substitute_
mung_string = ${subst %,${inert_percent},${subst ${nil}	,${inert_tab},${subst ${nil} ,${inert_space},${1}}}}
unmung_string = ${subst ${inert_percent},%,${subst ${inert_tab},	,${subst ${inert_space}, ,${1}}}}
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

Make_pathname = ${call relative_to,${${1}},${CURDIR}}
# Make_pathname takes the name of a variable whose value is an absolute
# pathname, and converts that pathname into the right form for usage in Make
# targets, prerequisites and functions, which means that it is made relative
# to the current working directory to prevent spaces in parent directories'
# names from breaking Make.


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
# GNATPREPFLAGS, GNAT_BUILDER_FLAGS, ADAFLAGS, CPPFLAGS, CFLAGS, CXXFLAGS,
# FFLAGS, GNATBINDFLAGS, GNATLINKFLAGS and LDFLAGS if they are undefined in the
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

library_type = dynamic
# If a library can be built as either shared or static, then library_type shall
# be used to set the attribute Library_Kind in the project files.

prefix        = /usr/local
exec_prefix   = ${prefix}
datarootdir   = ${prefix}/share
localstatedir = ${prefix}/var
# These variables are used in constructing the default values of the directory
# variables below.

bindir         = ${exec_prefix}/bin
libexecdir     = ${exec_prefix}/libexec
datadir        = ${datarootdir}
sysconfdir     = ${prefix}/etc
statedir       = ${localstatedir}/lib
cachedir       = ${localstatedir}/cache
logdir         = ${localstatedir}/log
runstatedir    = /run
lockdir        = ${runstatedir}/lock
includedir     = ${prefix}/include
archincludedir = ${includedir}
libdir         = ${exec_prefix}/lib
alidir         = ${libdir}
gprdir         = ${datarootdir}/gpr
localedir      = ${datarootdir}/locale
mandir         = ${datarootdir}/man
infodir        = ${datarootdir}/info
miscdocdir     = ${datarootdir}/doc
# These are the directories where different kinds of files will be located on
# the target system.

builddir = ${CURDIR}
objdir   = ${builddir}/obj
stagedir = ${builddir}/stage
# builddir is the build directory, which may be separate from the source tree.
# Intermediate files produced during the build are kept in objdir. Files to be
# installed are written under stagedir in the build phase, and then copied to
# their destination in the installation phase.

# Containing makefiles should avoid modifying the directory variables. Users
# should be able to rely on these defaults.

install_cp_flags = ${if ${DESTDIR},--preserve=timestamps}
# Timestamps are preserved when installation is done to a staging directory.
# This matters for files that aren't generated during the build but copied from
# the source tree. Timestamps are not preserved when installation is done
# directly to the target system, because that would change the timestamps of
# existing directories.

do_preinstall  = ${if ${DESTDIR},false,true}
do_postinstall = ${if ${DESTDIR},false,true}
# Any pre- and post-installation commands that the containing makefile may
# specify are executed when installation is done directly to the target system,
# but not when installation is done to a staging directory, because such
# commands need to be run on the target system, not on a build server.

#
# Containing makefiles may use these variables in their rules, but nothing
# should modify them:
#

srcdir := ${abspath ${dir ${lastword ${MAKEFILE_LIST}}}}
# srcdir is the directory in the source tree where makefiles and project files
# are. It may be the root of the source tree or a subdirectory. It is computed
# as the directory part of the last pathname in MAKEFILE_LIST – which is this
# file since there is no include directive above this point.

stage_bindir         = ${stagedir}${bindir}
stage_libexecdir     = ${stagedir}${libexecdir}
stage_datadir        = ${stagedir}${datadir}
stage_sysconfdir     = ${stagedir}${sysconfdir}
stage_statedir       = ${stagedir}${statedir}
stage_cachedir       = ${stagedir}${cachedir}
stage_logdir         = ${stagedir}${logdir}
stage_includedir     = ${stagedir}${includedir}
stage_archincludedir = ${stagedir}${archincludedir}
stage_libdir         = ${stagedir}${libdir}
stage_alidir         = ${stagedir}${alidir}
stage_gprdir         = ${stagedir}${gprdir}
stage_localedir      = ${stagedir}${localedir}
stage_mandir         = ${stagedir}${mandir}
stage_infodir        = ${stagedir}${infodir}
stage_miscdocdir     = ${stagedir}${miscdocdir}
# These are the directories where different kinds of files to be installed are
# written during the build.

Make_srcdir         = ${call Make_pathname,srcdir}
Make_builddir       = ${call Make_pathname,builddir}
Make_objdir         = ${call Make_pathname,objdir}
Make_stagedir       = ${call Make_pathname,stagedir}
Make_bindir         = ${call Make_pathname,stage_bindir}
Make_libexecdir     = ${call Make_pathname,stage_libexecdir}
Make_datadir        = ${call Make_pathname,stage_datadir}
Make_sysconfdir     = ${call Make_pathname,stage_sysconfdir}
Make_statedir       = ${call Make_pathname,stage_statedir}
Make_cachedir       = ${call Make_pathname,stage_cachedir}
Make_logdir         = ${call Make_pathname,stage_logdir}
Make_includedir     = ${call Make_pathname,stage_includedir}
Make_archincludedir = ${call Make_pathname,stage_archincludedir}
Make_libdir         = ${call Make_pathname,stage_libdir}
Make_alidir         = ${call Make_pathname,stage_alidir}
Make_gprdir         = ${call Make_pathname,stage_gprdir}
Make_localedir      = ${call Make_pathname,stage_localedir}
Make_mandir         = ${call Make_pathname,stage_mandir}
Make_infodir        = ${call Make_pathname,stage_infodir}
Make_miscdocdir     = ${call Make_pathname,stage_miscdocdir}
# These variables are for use in Make targets, prerequisites and other places
# where Make expects space-separated lists.

preprocess_file = "${GNATPREP}" ${firstword ${filter %.gp,$^}} $@ \
                  ${options_preprocessing} ${Gnatprep_arguments} \
                  ${if ${filter ${notdir $@},${notdir ${usage_GPRs}}}, \
                       ${usage_directories} '-DLibrary_Type="${library_type}"', \
                       '-DSrcdir="${srcdir}"'} \
                  ${GNATPREPFLAGS}
# preprocess_file is a command for use in recipes. It runs the first .gp file
# among the rule's prerequisites through Gnatprep to produce the target. If the
# target is a usage project, then the usage-relevant variables are conveyed to
# it as Gnatprep symbols. Otherwise srcdir is conveyed, as it's needed by
# preprocessed build projects.

build_GPR = "${GNAT_BUILDER}" -P ${firstword ${filter %.gpr,$^}} \
            ${addprefix -aP,${VPATH}} -p \
            ${options_building} ${builder_arguments} ${GNATFLAGS}
# build_GPR is a command for use in recipes. It performs a build controlled by
# the first project file among the rule's prerequisites.


#
# Adjust the build directory variables, and load the configuration:
#

# Ensure that builddir is an absolute pathname and is inherited by sub-Makes:

ifneq (${Comfignat_overriding_absolute_builddir},)
   override builddir := ${Comfignat_overriding_absolute_builddir}
else ifeq (${origin builddir},command line)
   override builddir := ${abspath ${builddir}}
endif
export Comfignat_overriding_absolute_builddir := ${builddir}

# Read the configuration file if there is one:

configuration = ${Make_builddir}/comfignat_configuration.mk

-include ${configuration}

# Ensure that objdir and stagedir are absolute pathnames and are inherited by
# sub-Makes:

ifneq (${Comfignat_overriding_absolute_objdir},)
   override objdir := ${Comfignat_overriding_absolute_objdir}
else ifeq (${origin objdir},command line)
   override objdir := ${abspath ${objdir}}
   export Comfignat_overriding_absolute_objdir := ${objdir}
   objdir_is_overridden = true
endif

ifneq (${Comfignat_overriding_absolute_stagedir},)
   override stagedir := ${Comfignat_overriding_absolute_stagedir}
else ifeq (${origin stagedir},command line)
   override stagedir := ${abspath ${stagedir}}
   export Comfignat_overriding_absolute_stagedir := ${stagedir}
   stagedir_is_overridden = true
endif

# builddir, objdir and stagedir need to be absolute in project files, because a
# pathname relative to a project file can be wrong when a separate build
# directory is used and project files are both in srcdir and in builddir.
# objdir and stagedir also need to be absolute in the configuration file
# because the working directory might change between Make invocations.
# Sub-Makes must use the same builddir, objdir and stagedir as the parent, so
# the absolute pathnames are conveyed to child processes in environment
# variables that won't normally be overridden and are unlikely to be defined by
# accident.
# The correction of builddir in sub-Makes must happen before builddir is used
# in VPATH and in the pathname of the configuration file.
# The inclusion of the configuration file must happen after MAKEFILE_LIST has
# been used to define srcdir.
# The changes to objdir and stagedir must be done after the configuration file
# is read because otherwise the configuration would override the command line.
# Once modified the variables are no longer of command line origin, so they are
# marked as overridden so that "make configure" will save them.


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
                   ${basename ${notdir ${wildcard ${Make_srcdir}/*.gp}}}}
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
# overridden on the command line, and will be defined as Gnatprep symbols and
# as external variables for build projects.
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

VPATH += ${filter-out .,${Make_srcdir} ${Make_builddir}}
# VPATH is a list of directories that Make should search for prerequisites.

# If VPATH has been defined as simply expanded before this file was included,
# then Make_srcdir and Make_builddir will be expanded now, so everything that's
# involved in their values must be defined before this point.

configuration_variables += \
   GNATPREP GNAT_BUILDER \
   GNATPREPFLAGS GNAT_BUILDER_FLAGS ADAFLAGS CPPFLAGS CFLAGS CXXFLAGS FFLAGS \
   GNATBINDFLAGS GNATLINKFLAGS LDFLAGS GNATFLAGS \
   DESTDIR \
   dirgpr relocatable_package library_type \
   prefix exec_prefix datarootdir localstatedir \
   bindir libexecdir \
   datadir sysconfdir statedir cachedir logdir runstatedir lockdir \
   includedir archincludedir libdir alidir gprdir \
   localedir mandir infodir miscdocdir \
   objdir stagedir \
   install_cp_flags \
   do_preinstall do_postinstall \
   ${options}
# configuration_variables is a list of variables that can be saved in the
# persistent configuration with "make configure". Containing makefiles may
# append additional variable names.


#
# Compute symbol definitions for Gnatprep and external variable assignments for
# build projects:
#

# For this some more functions and constants for processing directory variables
# are needed.

usage_directory_variables = includedir archincludedir libdir alidir
# These are the usage-relevant directory variables. They are needed in usage
# projects after installation.

builder_directory_variables = bindir libexecdir ${usage_directory_variables}
# These are the builder-relevant directory variables. They control where the
# GNAT tools write files to be installed. These are the variables that
# Comfignat-compatible directories projects must provide.

usage_relevant = ${filter ${usage_directory_variables},${1}}
# usage_relevant returns a list of the words in the input list that are usage-
# relevant directory variables. If given a single variable name, it returns
# that name if the variable is usage-relevant, or an empty string if it isn't.

checked_boolean = ${or ${and ${filter 1,${words ${${1}}}}, \
                             ${filter true false,${${1}}}}, \
                       ${error ${1} must be "true" or "false"}}
# checked_boolean takes the name of a variable and checks that its value is a
# single word, and that that word is either "true" or "false". If so it returns
# the value; otherwise it complains and stops the execution.

checked_true = ${filter true,${call checked_boolean,${1}}}
# checked_true takes the name of a variable and checks that it has a boolean
# value. It then returns an empty string for "false" or a non-empty string for
# "true".

maybe_relative_to = ${if ${call checked_true,relocatable_package} \
                        ,${call relative_to,${1},${2}},${1}}
# maybe_relative_to converts an absolute pathname into a relative one if a
# relocatable package is desired.
# Parameters:
#    1: an absolute pathname to maybe convert to relative
#    2: the absolute base pathname that 1 may be made relative to
# If relocatable_package is "true", then let relative_to convert the pathname,
# otherwise return parameter 1 unchanged.
# It is checked that relocatable_package has a boolean value.

embed_pathname = ${call maybe_relative_to,${${1}},${if ${filter bindir,${1}} \
                                                      ,${libexecdir},${bindir}}}
# embed_pathname takes the name of a variable whose value is an absolute
# pathname, and converts that pathname into the right form for inclusion in a
# program, which means that bindir is made relative to libexecdir and other
# variables are made relative to bindir if a relocatable package is desired.

usage_pathname = ${call maybe_relative_to,${${1}},${gprdir}}
# usage_pathname takes the name of a variable whose value is an absolute
# pathname, and converts that pathname into the right form for inclusion in a
# usage project, which means that it is made relative to gprdir if a
# relocatable package is desired.

define convey_builder_directory_variable
   all_directories   += '-D${1}="${call embed_pathname,${1}}"'
   all_directories   += '-Dstage_${1}="${stage_${1}}"'
   usage_directories += ${if ${call usage_relevant,${1}}, \
                             '-D${1}="${call usage_pathname,${1}}"'}
endef
# convey_builder_directory_variable takes the name of a builder-relevant
# directory variable and returns Make code that conveys that variable to
# project files.
#    · Append a symbol definition to all_directories to convey the variable to
#      comfignat.gpr in the right form for inclusion in a program.
#    · Also convey to comfignat.gpr the corresponding pathname under the
#      staging directory, which wouldn't be derived correctly from a relative
#      pathname.
#    · If the variable is also usage-relevant, then append a symbol definition
#      to usage_directories to convey it to usage projects in the form that
#      usage projects need.

define use_directories_project_variable
   all_directories   += '-D${1}=${directories_project}.${1}'
   usage_directories += ${if ${call usage_relevant,${1}}, \
                             '-D${1}=${directories_project}.${1}'}
endef
# use_directories_project_variable takes the name of a builder-relevant
# directory variable and returns Make code that makes project files get that
# variable from a directories project.
#    · Append a symbol definition to all_directories for comfignat.gpr.
#    · If the variable is also usage-relevant, then append a symbol definition
#      to usage_directories for usage projects.

# Now that all those functions are defined, compute the symbol definitions for
# the directory variables.

# Convey builddir, objdir and stagedir to comfignat.gpr.
all_directories = '-DBuilddir="${builddir}"' '-DObjdir="${objdir}"' \
                  '-DStagedir="${stagedir}"'

usage_directories =

# Make project files import the directories project if one has been provided.
ifneq (${dirgpr},)
   directories_project := ${basename ${notdir ${call mung_string,${dirgpr}}}}
   all_directories     += '-DDirectories_GPR="${dirgpr}"'
   all_directories     += '-DDirectories_Project=${directories_project}'
   usage_directories   += '-DDirectories_GPR="${dirgpr}"'
endif

# Convey the builder-irrelevant directory variables, making them available to
# build projects for inclusion in binaries. Make most of the pathnames relative
# if a relocatable package is desired.
all_directories += '-DDatadir="${call embed_pathname,datadir}"'
all_directories += '-DSysconfdir="${call embed_pathname,sysconfdir}"'
all_directories += '-DStatedir="${call embed_pathname,statedir}"'
all_directories += '-DCachedir="${call embed_pathname,cachedir}"'
all_directories += '-DLogdir="${call embed_pathname,logdir}"'
all_directories += '-DGPRdir="${call embed_pathname,gprdir}"'
all_directories += '-DLocaledir="${call embed_pathname,localedir}"'
all_directories += '-DMandir="${call embed_pathname,mandir}"'
all_directories += '-DInfodir="${call embed_pathname,infodir}"'
all_directories += '-DMiscdocdir="${call embed_pathname,miscdocdir}"'
all_directories += '-DRunstatedir="${runstatedir}"'
all_directories += '-DLockdir="${lockdir}"'
# runstatedir and lockdir belong to the operating system and are used for
# communication between subsystems. It wouldn't make sense for an application
# to have its own runstatedir. Therefore these variables are always absolute
# pathnames.

# Set the builder-relevant directory variables.
${foreach var,${builder_directory_variables}, \
          ${if ${or ${findstring command line,${origin ${var}}}, \
                    ${filter true,${${var}_is_configured}}, \
                    ${filter 0,${words ${dirgpr}}}}, \
               ${eval ${call convey_builder_directory_variable,${var}}}, \
               ${eval ${call use_directories_project_variable,${var}}}}}
# For each builder-relevant directory variable, check whether its value in
# project files should be taken from the corresponding Make variable or from a
# directories project, and construct symbol definitions accordingly.
# If a variable is of command line origin or marked as configured, or if dirgpr
# is empty (that is, no directories project has been provided), then convey the
# variable to project files. Otherwise make project files use the variable that
# the directories project provides.

# And now process any boolean options.

option_values = \
   ${foreach option,${options}, \
             ${if ${filter-out undefined environment,${origin ${option}}}, \
                  ${option}=${call checked_boolean,${option}}, \
                  ${error ${option} has no default value and must be set to \
                          "true" or "false" on the command line}}}
# For each variable listed in options, check that it exists, that it didn't
# come from the environment (to prevent accidents), and that it has a boolean
# value. If so, output a name/value pair; otherwise complain and stop.

# Convey boolean options to Gnatprep.
options_preprocessing = ${addprefix -D,${option_values}}

# Convey boolean options to build projects.
options_building = ${addprefix -X,${option_values}}


#
# Some other data that the rules below need:
#

main_makefile := ${firstword ${MAKEFILE_LIST}}
delegation_command = @$${MAKE} --file=${abspath ${main_makefile}} \
                     --include-dir=${abspath ${dir ${main_makefile}}}
# delegation_command is the Make command line that delegating makefiles in
# separate build directories use to delegate commands to the main makefile. The
# first pathname in MAKEFILE_LIST is the main makefile.

build_targets = ${addsuffix .phony_target,${build_GPRs}}
# A phony target is defined for each build project, and the job of determining
# whether the project needs rebuilding is delegated to the builder.

staged_usage_GPRs = ${addprefix ${Make_gprdir}/,${usage_GPRs}}
preprocessed_files_in_builddir = ${addprefix ${Make_builddir}/,${preprocessed_files}}
# When usage projects are preprocessed they are written to stage_gprdir. Other
# preprocessed files are assumed to be needed during the build and are written
# to builddir.


#
# Make rules:
#

.SECONDEXPANSION:

.PHONY: Comfignat_default_goal
Comfignat_default_goal: build

# How to make directories:
%/:
	mkdir -p $@
.PRECIOUS: %/

# This rule appears to work around a bug that was fixed in GNU Make 3.82:
${Make_gprdir}/:
	mkdir -p $@

# How to initialize a build directory with a delegating makefile:
${Make_builddir}/Makefile: | ${Make_builddir}/
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
.PHONY: configure
configure:: ${Make_builddir}/Makefile
	@echo "Writing ${configuration}."
	@( ${foreach var,${configuration_variables}, \
	             ${if ${or ${findstring command line,${origin ${var}}}, \
	                       ${filter true,${${var}_is_overridden}}, \
	                       ${filter true,${${var}_is_configured}}}, \
	                  echo 'ifneq "$${origin ${var}}" "command line"'; \
	                  echo '   override ${var} = ${value ${var}}';\
	                  echo 'endif'; \
	                  echo '${var}_is_configured = true'; \
	                  echo;, \
	             ${if ${or ${findstring environment,${origin ${var}}}, \
	                       ${filter true,${${var}_is_weakly_configured}}}, \
	                  echo 'ifneq "$${origin ${var}}" "environment"'; \
	                  echo '   ${var} = ${value ${var}}'; \
	                  echo 'endif'; \
	                  echo '${var}_is_weakly_configured = true'; \
	                  echo;}}} \
	   true \
	 ) > "${configuration}"
# Out of the variables listed in configuration_variables, all that were
# overridden on the command line, all that are set in the environment and not
# overridden elsewhere, and all that were previously configured, are written
# to the configuration file. Command-line-configured values override defaults
# assigned later in the containing makefile, but can be overridden on the
# command line. Environment-configured values override defaults assigned with
# "?=", but can be overridden in the environment or on the command line. A
# variable is considered previously configured if there is another variable
# with "_is_configured" or "_is_weakly_configured" appended to its name and a
# value of "true". Such a variable is also written for each configured
# variable. As a side effect of this it is possible to delete a variable V from
# the configuration by running "make configure V_is_configured=false
# V_is_weakly_configured=false".

# How to show the values of configured variables:
.PHONY: show_configuration
show_configuration::
	@${foreach var,${configuration_variables}, \
	           ${if ${filter true,${${var}_is_configured}}, \
	                echo '${var} = ${value ${var}}';} \
	           ${if ${filter true,${${var}_is_weakly_configured}}, \
	                echo '${var} ?= ${value ${var}}';}} \
	 true

# How to preprocess the project Comfignat:
${Make_builddir}/comfignat.gpr: comfignat.gpr.gp | ${Make_builddir}/
	"${GNATPREP}" $< $@ -DInvoked_By_Makefile ${all_directories} \
	              '-DLibrary_Type="${library_type}"' ${GNATPREPFLAGS}

# How to preprocess files that are needed during the build:
${Make_builddir}/%: %.gp | ${Make_builddir}/
	${preprocess_file}

# How to preprocess usage projects:
${Make_gprdir}/%: %.gp | ${Make_gprdir}/
	${preprocess_file}

# How to stage usage projects that don't need preprocessing:
${Make_gprdir}/%: % | ${Make_gprdir}/
	cp -p $< $@

.PHONY: preprocess
preprocess: $${preprocessed_files_in_builddir}

# How to build a project:
%.gpr.phony_target: %.gpr preprocess
	${build_GPR}
# Instead of tracking dependencies between project files, this rule simply
# requires that all preprocessing of files that are needed during the build is
# done before any project is built.

.PHONY: base
base: $${build_targets}
# This builds the projects listed in build_GPRs, plus any additional
# prerequisites that the containing makefile might add.

.PHONY: build
build: base $${staged_usage_GPRs}
# This is the default build. Additional targets that should be built by default
# may be added as prerequisites.

.PHONY: all
all: build
# Optional targets may be added as prerequisites of "all".

${Make_stagedir}:
	@${MAKE} build
# "make install" straight out of a source package triggers a build, but if
# something has been built then "make install" doesn't rebuild anything, just
# copies the built files to their destination.

.PHONY: preinstall
preinstall:
# A recipe may be added to "preinstall" with commands that need to be run
# before the files are installed when installation is done directly to the
# target system, but should be skipped when installation is done to a staging
# directory.

# How to install what has been built and staged:
.PHONY: install_stage
install_stage: ${Make_stagedir} ${if ${call checked_true,do_preinstall},preinstall}
	if [ "`echo "${stagedir}"/*`" != "${stagedir}/*" ]; then \
	   mkdir -p "${DESTDIR}/"; \
	   cp -RPf ${install_cp_flags} "${stagedir}"/* "${DESTDIR}/"; \
	fi
# If stagedir doesn't exist, then the rule to make it by running the build is
# invoked. If stagedir then exists and contains some files (the asterisk gets
# expanded) then those files are copied recursively to DESTDIR or to the
# filesystem root.

.PHONY: install_files
install_files: install_stage
# A recipe may be added to "install_files" if any files have to be written,
# deleted or moved after the staged directory tree has been installed. This
# should be used only for workarounds. It's better to stage all the files
# correctly under stagedir in the build phase.

.PHONY: postinstall
postinstall: install_files
# A recipe may be added to "postinstall" with commands that need to be run
# after the files are installed when installation is done directly to the
# target system, but should be skipped when installation is done to a staging
# directory. This will typically be commands that modify existing files on the
# target system.

.PHONY: install
install: install_files ${if ${call checked_true,do_postinstall},postinstall}

.PHONY: clean
clean::
	rm -Rf "${objdir}" "${stagedir}" ${preprocessed_files_in_builddir}

.PHONY: unconfigure
unconfigure::
	rm -f "${configuration}"

.PHONY: distclean
distclean: clean unconfigure
