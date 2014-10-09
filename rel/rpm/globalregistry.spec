# Turn off binary file stripping
%global __os_install_post %{nil}

%define __prelink_undo_cmd %{nil}
%define debug_package %{nil}

%define _topdir     /tmp/globalregistry_rpmbuild
%define _tmppath    %{_topdir}/tmp
%define _prefix     /opt/globalregistry

%define name        globalregistry
%define summary     globalregistry service
%define version     {{version}}
%define release     1
%define license     MIT
%define arch        x86_64
%define group       System/Base
%define source      %{name}.tar.gz
%define url         http://onedata.org
%define vendor      onedata
%define packager    onedata
%define buildroot   %{_tmppath}/%{name}-build


Name:      %{name}
Version:   %{version}
Release:   %{release}
Packager:  %{packager}
Vendor:    %{vendor}
License:   %{license}
Summary:   %{summary}
Group:     %{group}
Source:    %{source}
URL:       %{url}
Prefix:    %{_prefix}
BuildRoot: %{buildroot}
BuildArch: %{arch}

# Disable auto dependency recognition and list required deps explicitely
AutoReqProv: no
requires: /bin/bash /bin/sh /usr/bin/env ld-linux-x86-64.so.2()(64bit) ld-linux-x86-64.so.2(GLIBC_2.3)(64bit)

%description
globalregistry service - allows installation of globalregistry nodes.

%prep

%setup -n %{name}

%build

%install
./install_rpm $RPM_BUILD_ROOT %{_prefix}

%post
sh %{_prefix}/scripts/onepanel_setup %{_prefix}
chkconfig --add globalregistry
chkconfig --add onepanel
service onepanel start
ln -sf %{_prefix}/scripts/onepanel_admin /usr/bin/onepanel_admin

%preun
service globalregistry stop
service onepanel stop
chkconfig --del globalregistry
chkconfig --del onepanel
rm -f /usr/bin/onepanel_admin
rm -rf %{_prefix}
rm -rf /opt/bigcouch

%clean
rm -rf $RPM_BUILD_ROOT
rm -rf %{_tmppath}/%{name}
rm -rf %{_topdir}/BUILD/%{name}

# list files owned by the package here
%files
%defattr(-,root,root)
%{_prefix}
/etc/init.d/globalregistry
/etc/init.d/onepanel