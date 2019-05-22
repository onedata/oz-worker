#!/usr/bin/env bash

#####################################################################
# @author Lukasz Opiola
# @copyright (C) 2016 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# This is a configuration file for the pull-gui.sh script, for more see:
#   _build/default/lib/gui/pull-gui.sh
#
# The pull-gui.sh script is used to inject GUI to the OZ worker release.
# First, the gui package is copied from a docker to the deps directory.
# After release generation, it is copied to the release package (see Makefile).
#####################################################################

# Path relative to this script, to which static GUI package will be copied.
TARGET_PATH='_build/default/lib/gui_static.tar.gz'
# Image which will be used by default to get the static files.
PRIMARY_IMAGE='docker.onedata.org/onezone-gui:ID-23d24b4450'
# Image which will be used if the primary image cannot be resolved.
SECONDARY_IMAGE='onedata/onezone-gui:ID-23d24b4450'