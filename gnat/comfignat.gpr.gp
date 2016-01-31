-- Comfignat configuration variables for GNAT project files
-- Copyright 2013 - 2016 B. Persson, Bjorn@Rombobeorn.se
--
-- This material is provided as is, with absolutely no warranty expressed
-- or implied. Any use is at your own risk.
--
-- Permission is hereby granted to use or copy this project file
-- for any purpose, provided the above notices are retained on all copies.
-- Permission to modify the code and to distribute modified code is granted,
-- provided the above notices are retained, and a notice that the code was
-- modified is included with the above copyright notice.


-- This file is part of Comfignat 1.5 beta – common, convenient, command-line-
-- controlled compile-time configuration of software built with the GNAT tools.
-- For information about Comfignat, see http://www.Rombobeorn.se/Comfignat/.


-- This project file defines directory variables for use in build-controlling
-- project files. It is not to be installed on the target system.
--
-- Normally the preprocessing of this file will be controlled by comfignat.mk,
-- which will make it import the directories project if one is provided. It can
-- also be preprocessed manually if Make cannot be used for some reason. There
-- are defaults that will be used if no preprocessor symbols are defined.


#if Directories_GPR'Defined then
   with $Directories_GPR;
#end if;

abstract project Comfignat is

   #if not Invoked_By_Makefile'Defined then

      --
      -- These variables are used in constructing the default values of the
      -- directory variables below. They're only needed when this file is
      -- preprocessed manually.
      --

      #if Prefix'Defined then
         Prefix := $Prefix;
      #else
         Prefix := "/usr/local";
      #end if;
      -- Prefix is used in the default locations for almost all files.

      #if Exec_Prefix'Defined then
         Exec_Prefix := $Exec_Prefix;
      #else
         Exec_Prefix := Prefix;
      #end if;
      -- Exec_Prefix is used in the default locations for programs, binary
      -- libraries and other architecture-specific files.

      #if Datarootdir'Defined then
         Datarootdir := $Datarootdir;
      #else
         Datarootdir := Prefix & "/share";
      #end if;
      -- Datarootdir is the root of the directory tree for read-only
      -- architecture-independent data files.

      #if Localstatedir'Defined then
         Localstatedir := $Localstatedir;
      #else
         Localstatedir := Prefix & "/var";
      #end if;
      -- Localstatedir is the root of the directory tree for data files that
      -- programs modify while they run.

   #end if;


   --
   -- The following variables may be compiled into programs or libraries to
   -- tell them where to find or write different kinds of files at run time.
   -- Most of the directory names are relative to Bindir if the software was
   -- configured as a relocatable package. Otherwise they are absolute
   -- pathnames. Bindir is relative to Libexecdir when the package is
   -- relocatable.
   --

   -- Programs that can be run from a command prompt are in Bindir. This is
   -- usually the same directory that the program itself is in, so this
   -- variable is probably useful only to programs under Libexecdir.
   #if Bindir'Defined then
      Bindir := $Bindir;
   #else
      Bindir := Exec_Prefix & "/bin";
   #end if;

   -- Programs that are intended to be run by other programs rather than by
   -- users are under Libexecdir.
   #if Libexecdir'Defined then
      Libexecdir := $Libexecdir;
   #else
      Libexecdir := Exec_Prefix & "/libexec";
   #end if;

   -- Idiosyncratic read-only architecture-independent data files are under an
   -- application-specific subdirectory of Datadir.
   #if Datadir'Defined then
      Datadir := $Datadir;
   #else
      Datadir := Datarootdir;
   #end if;

   -- Host-specific configuration files are under Sysconfdir.
   #if Sysconfdir'Defined then
      Sysconfdir := $Sysconfdir;
   #else
      Sysconfdir := Prefix & "/etc";
   #end if;

   -- Idiosyncratic variable data files shall be kept under an application-
   -- specific subdirectory of Statedir.
   #if Statedir'Defined then
      Statedir := $Statedir;
   #else
      Statedir := Localstatedir & "/lib";
   #end if;

   -- Cached data files that the application can regenerate if they are deleted
   -- shall be kept under an application-specific subdirectory of Cachedir.
   #if Cachedir'Defined then
      Cachedir := $Cachedir;
   #else
      Cachedir := Localstatedir & "/cache";
   #end if;

   -- Log files shall be written under Logdir.
   #if Logdir'Defined then
      Logdir := $Logdir;
   #else
      Logdir := Localstatedir & "/log";
   #end if;

   -- Small files that take part in describing the state of the system and that
   -- exist only while the program is running, such as process identifier files
   -- and transient Unix-domain sockets, shall be sought and created under
   -- Runstatedir. (This is NOT the place for temporary files in general.)
   #if Runstatedir'Defined then
      Runstatedir := $Runstatedir;
   #else
      Runstatedir := "/run";
   #end if;

   -- Lock files that are used to prevent multiple programs from trying to
   -- access a device or other resource at the same time shall be sought and
   -- created under Lockdir.
   #if Lockdir'Defined then
      Lockdir := $Lockdir;
   #else
      Lockdir := Runstatedir & "/lock";
   #end if;

   -- Source files to be used in the compilation of software using libraries
   -- are under Includedir.
   #if Includedir'Defined then
      Includedir := $Includedir;
   #else
      Includedir := Prefix & "/include";
   #end if;

   -- If a library has installed architecture-specific source files to be used
   -- in compilation, then those files may also be under a library-specific
   -- subdirectory of Archincludedir.
   #if Archincludedir'Defined then
      Archincludedir := $Archincludedir;
   #else
      Archincludedir := Includedir;
   #end if;

   -- Binary libraries and other architecture-specific files are under Libdir.
   #if Libdir'Defined then
      Libdir := $Libdir;
   #else
      Libdir := Exec_Prefix & "/lib";
   #end if;

   -- ALI files are under a library-specific subdirectory of Alidir.
   #if Alidir'Defined then
      Alidir := $Alidir;
   #else
      Alidir := Libdir;
   #end if;

   -- A program or library that has Archincludedir, Libdir and/or Alidir
   -- compiled-in will in those directories find only libraries compiled for
   -- the same architecture as itself.

   -- GNAT project files are under GPRdir.
   #if GPRdir'Defined then
      GPRdir := $GPRdir;
   #else
      GPRdir := Datarootdir & "/gpr";
   #end if;

   -- Locale-specific message catalogs are under Localedir.
   #if Localedir'Defined then
      Localedir := $Localedir;
   #else
      Localedir := Datarootdir & "/locale";
   #end if;

   -- Documentation in the Man format is under Mandir.
   #if Mandir'Defined then
      Mandir := $Mandir;
   #else
      Mandir := Datarootdir & "/man";
   #end if;

   -- Documentation in the Info format is in Infodir.
   #if Infodir'Defined then
      Infodir := $Infodir;
   #else
      Infodir := Datarootdir & "/info";
   #end if;

   -- Other documentation files are under an application-specific subdirectory
   -- of Miscdocdir.
   #if Miscdocdir'Defined then
      Miscdocdir := $Miscdocdir;
   #else
      Miscdocdir := Datarootdir & "/doc";
   #end if;


   --
   -- The following variables are for use in attributes to control where
   -- generated files are placed.
   --

   -- Various generated files are kept in Builddir.
   #if Builddir'Defined then
      Builddir := $Builddir;
   #else
      Builddir := ".";
   #end if;

   -- Intermediate files produced during the build shall be kept in Objdir.
   #if Objdir'Defined then
      Objdir := $Objdir;
   #else
      Objdir := Builddir & "/obj";
   #end if;

   #if Directories_Project'Defined then
      -- Put intermediate files for different architectures in subdirectories
      -- where they won't conflict with each other. (This is useful especially
      -- with binder files when they are packaged in debug information packages
      -- for multiarch systems.)
      Objdir := Objdir & "/" & $Directories_Project.Hardware_Platform;
   #end if;

   -- Files to be installed shall be placed under Stagedir instead of the root
   -- directory. (This variable is unused by Comfignat when Make is used and no
   -- directories project is provided.)
   #if Stagedir'Defined then
      Stagedir := $Stagedir;
   #else
      Stagedir := external("DESTDIR", "");
   #end if;

   -- Programs that can be run from a command prompt shall be installed in
   -- Stage_Bindir.
   #if Stage_Bindir'Defined then
      Stage_Bindir := $Stage_Bindir;
   #else
      Stage_Bindir := Stagedir & Bindir;
   #end if;

   -- Programs that are only intended to be run by other programs, not by
   -- users, shall be installed under an application-specific subdirectory of
   -- Stage_Libexecdir.
   #if Stage_Libexecdir'Defined then
      Stage_Libexecdir := $Stage_Libexecdir;
   #else
      Stage_Libexecdir := Stagedir & Libexecdir;
   #end if;

   -- Source files needed for compiling code that uses a library shall be
   -- installed under Stage_Includedir.
   #if Stage_Includedir'Defined then
      Stage_Includedir := $Stage_Includedir;
   #else
      Stage_Includedir := Stagedir & Includedir;
   #end if;

   -- If architecture-specific source files absolutely must be installed, then
   -- those files may be placed under a library-specific subdirectory of
   -- Stage_Archincludedir.
   #if Stage_Archincludedir'Defined then
      Stage_Archincludedir := $Stage_Archincludedir;
   #else
      Stage_Archincludedir := Stagedir & Archincludedir;
   #end if;

   -- Binary libraries shall be installed in Stage_Libdir.
   #if Stage_Libdir'Defined then
      Stage_Libdir := $Stage_Libdir;
   #else
      Stage_Libdir := Stagedir & Libdir;
   #end if;

   -- ALI files shall be installed under a library-specific subdirectory of
   -- Stage_Alidir.
   #if Stage_Alidir'Defined then
      Stage_Alidir := $Stage_Alidir;
   #else
      Stage_Alidir := Stagedir & Alidir;
   #end if;


   --
   -- Other configuration than directories:
   --

   -- If a library can be built as either shared or static, then Library_Type
   -- shall be used to set the attribute Library_Kind. It can be overridden on
   -- the builder command line, which makes it possible to write a makefile
   -- that builds both a shared and a static library.
   type Library_Kind is ("dynamic", "relocatable", "static");
   #if Library_Type'Defined then
      Library_Type : Library_Kind := external("LIBRARY_TYPE", $Library_Type);
   #else
      Library_Type : Library_Kind := external("LIBRARY_TYPE", "dynamic");
   #end if;

end Comfignat;
