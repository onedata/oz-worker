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
requires: /bin/sh botan ld-linux-x86-64.so.2()(64bit) ld-linux-x86-64.so.2(GLIBC_2.3)(64bit)
requires: libGL.so.1()(64bit) libGLU.so.1()(64bit) libc.so.6()(64bit)
requires: libc.so.6(GLIBC_2.10)(64bit) libc.so.6(GLIBC_2.11)(64bit)
requires: libc.so.6(GLIBC_2.14)(64bit) libc.so.6(GLIBC_2.15)(64bit)
requires: libc.so.6(GLIBC_2.2.5)(64bit) libc.so.6(GLIBC_2.3)(64bit)
requires: libc.so.6(GLIBC_2.3.2)(64bit) libc.so.6(GLIBC_2.3.4)(64bit)
requires: libc.so.6(GLIBC_2.4)(64bit) libc.so.6(GLIBC_2.7)(64bit)
requires: libc.so.6(GLIBC_2.8)(64bit) libc.so.6(GLIBC_2.9)(64bit)
requires: libcrypto.so.10()(64bit) libcrypto.so.10(OPENSSL_1.0.1_EC)(64bit)
requires: libcrypto.so.10(OPENSSL_1.0.2)(64bit) libcrypto.so.10(libcrypto.so.10)(64bit)
requires: libdl.so.2()(64bit) libdl.so.2(GLIBC_2.2.5)(64bit) libgcc_s.so.1()(64bit)
requires: libgcc_s.so.1(GCC_3.0)(64bit) libm.so.6()(64bit) libm.so.6(GLIBC_2.2.5)(64bit)
requires: libpthread.so.0()(64bit) libpthread.so.0(GLIBC_2.12)(64bit)
requires: libpthread.so.0(GLIBC_2.2.5)(64bit) libpthread.so.0(GLIBC_2.3.2)(64bit)
requires: librt.so.1()(64bit) librt.so.1(GLIBC_2.2.5)(64bit) libsodium.so.13()(64bit)
requires: libstdc++.so.6()(64bit) libstdc++.so.6(CXXABI_1.3)(64bit)
requires: libstdc++.so.6(CXXABI_1.3.1)(64bit) libstdc++.so.6(CXXABI_1.3.3)(64bit)
requires: libstdc++.so.6(CXXABI_1.3.7)(64bit) libstdc++.so.6(CXXABI_1.3.9)(64bit)
requires: libstdc++.so.6(GLIBCXX_3.4)(64bit) libstdc++.so.6(GLIBCXX_3.4.11)(64bit)
requires: libstdc++.so.6(GLIBCXX_3.4.14)(64bit) libstdc++.so.6(GLIBCXX_3.4.15)(64bit)
requires: libstdc++.so.6(GLIBCXX_3.4.17)(64bit) libstdc++.so.6(GLIBCXX_3.4.18)(64bit)
requires: libstdc++.so.6(GLIBCXX_3.4.19)(64bit) libstdc++.so.6(GLIBCXX_3.4.21)(64bit)
requires: libsystemd.so.0()(64bit) libsystemd.so.0(LIBSYSTEMD_209)(64bit)
requires: libtinfo.so.5()(64bit) libutil.so.1()(64bit) libutil.so.1(GLIBC_2.2.5)(64bit)
requires: libwx_baseu-2.8.so.0()(64bit) libwx_baseu-2.8.so.0(WXU_2.8)(64bit)
requires: libwx_baseu_xml-2.8.so.0()(64bit) libwx_gtk2u_adv-2.8.so.0()(64bit)
requires: libwx_gtk2u_adv-2.8.so.0(WXU_2.8)(64bit) libwx_gtk2u_aui-2.8.so.0()(64bit)
requires: libwx_gtk2u_aui-2.8.so.0(WXU_2.8)(64bit) libwx_gtk2u_aui-2.8.so.0(WXU_2.8.5)(64bit)
requires: libwx_gtk2u_core-2.8.so.0()(64bit) libwx_gtk2u_core-2.8.so.0(WXU_2.8)(64bit)
requires: libwx_gtk2u_gl-2.8.so.0()(64bit) libwx_gtk2u_gl-2.8.so.0(WXU_2.8)(64bit)
requires: libwx_gtk2u_html-2.8.so.0()(64bit) libwx_gtk2u_html-2.8.so.0(WXU_2.8)(64bit)
requires: libwx_gtk2u_stc-2.8.so.0()(64bit) libwx_gtk2u_stc-2.8.so.0(WXU_2.8)(64bit)
requires: libwx_gtk2u_xrc-2.8.so.0()(64bit) libwx_gtk2u_xrc-2.8.so.0(WXU_2.8)(64bit)
requires: libz.so.1()(64bit) libz.so.1(ZLIB_1.2.2)(64bit) rtld(GNU_HASH) openssl protobuf

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
