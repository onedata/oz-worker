#!/usr/bin/env bash

#####################################################################
# @author Lukasz Opiola
# @copyright (C): 2016 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# usage:
#       This script is not used directly - it should be passed to
#       deps/gui/pull-gui.sh.
#
# This script contains configuration that is required to pull GUI to OZ
# worker (by copying static files from a static docker).
#####################################################################

# Configuration
# Directory relative to this script, to which static GUI files will be copied.
# First put them in deps, later after release generation they will be copied
# from there to release (see Makefile).
TARGET_DIR='_build/default/lib'
ARCHIVE_NAME='gui_static.tar.gz'
# Image which will be used by default to get the static files. If it cannot
# be resolved, the script will fall back to secondary.
# @fixme proper GUI image
PRIMARY_IMAGE='docker.onedata.org/onezone-gui:SHA256-4493496881286e6015289344854549f31541eec7f35eb8ef6fce41084da432fd'
# Image which will be used if primary image is not resolved.
SECONDARY_IMAGE='onedata/onezone-gui:SHA256-4493496881286e6015289344854549f31541eec7f35eb8ef6fce41084da432fd'
