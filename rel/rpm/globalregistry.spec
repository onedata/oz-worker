# Turn off binary file stripping
%global __os_install_post %{nil}

%global __prelink_undo_cmd %{nil}
%global debug_package %{nil}
%global _prefix /opt/globalregistry
%global builddir %{_builddir}/%{buildsubdir}
%global version @version@

Name:      globalregistry
Version:   %{version}
Release:   1%{?dist}
License:   MIT
Summary:   globalregistry service
Group:     System/Base
Source:    globalregistry-%{version}.tar.gz
URL:       https://onedata.org

BuildRequires: automake
BuildRequires: bc
BuildRequires: botan-devel >= 1.10
BuildRequires: cmake >= 3.0.0
BuildRequires: erlang >= 18
BuildRequires: gcc-c++ >= 4.9.0
BuildRequires: git
BuildRequires: golang
BuildRequires: libsodium-devel
BuildRequires: libtool
BuildRequires: make
BuildRequires: ninja-build
BuildRequires: openssh-clients
BuildRequires: protobuf-compiler >= 2.6.0
BuildRequires: protobuf-devel >= 2.6.0
BuildRequires: python

Requires: openssl
Requires: sed

%description
globalregistry service - allows installation of globalregistry nodes.

%prep
%setup -q

%build
make -C onepanel rel CONFIG=config/globalregistry.config
make rel

%install
%define SOURCES %{builddir}/SOURCES
mkdir %{SOURCES}

# Attach globalregistry release
mkdir -p %{SOURCES}/files/globalregistry_node
cp %{builddir}/rel/rpm/install_rpm %{SOURCES}
cp %{builddir}/rel/rpm/globalregistry_init_d %{SOURCES}
cp -R %{builddir}/rel/scripts %{SOURCES}
cp -R %{builddir}/rel/globalregistry/* %{SOURCES}/files/globalregistry_node

# Attach bigcouch release
mkdir -p %{SOURCES}/files/database_node
cp -R %{builddir}/bigcouchdb/database_node/* %{SOURCES}/files/database_node

# Attach onepanel release
mkdir -p %{SOURCES}/files/onepanel_node
cp -R %{builddir}/onepanel/rel_globalregistry/onepanel/* %{SOURCES}/files/onepanel_node

cd %{SOURCES}
./install_rpm %{buildroot} %{_prefix}

%post
chkconfig --add globalregistry
chkconfig --add onepanel
%{_prefix}/scripts/onepanel_setup %{_prefix}
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

# list files owned by the package here
%files
%defattr(-,root,root)
%{_prefix}
/etc/init.d/globalregistry
/etc/init.d/onepanel

%changelog
