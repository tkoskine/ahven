Name:		ahven
Version:	1.7
Release:	1%{?dist}
Summary:	Unit Test Framework for Ada 95 Programming Language

Group:		Development/Libraries
License:	ISC
URL:		http://ahven.stronglytyped.org/
Source0:	ahven-1.7.tar.gz
Patch0:		ahven.static_lib.patch
BuildRoot:	%(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:	gcc-gnat
BuildRequires:	libgnat

%description
Ahven is a unit testing framework for Ada 95.

%prep
%setup -q
%patch0 -p1 -b .static_lib


%build
make %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
make install PREFIX=$RPM_BUILD_ROOT/usr


%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_includedir}/ahven
%{_libdir}/ahven
%{_libdir}/gnat

%changelog
* Fri Feb 19 2010 Tero Koskinen <tero.koskinen@iki.fi> - 1.7-1
- Initial version.
