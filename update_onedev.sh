#!/bin/bash

# ===================================================================
# Author: Lukasz Opiola
# Copyright (C): 2014 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
# ===================================================================
# This simple script clones or updates onedev repository if it is possible
# i. e. the user has rights to clone it. If it succeeds, it swaps the empty
# auth config file with the one from repository making it possible to login
# via OAuth test apps.
#
# The script is run at every get-deps rebar command.
# ===================================================================

# Enter the script directory
RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd)
cd $RUNNER_SCRIPT_DIR

echo "Trying to clone/update Onedev repository..."
mkdir -p deps
cd deps

# Check if repo exists
if [ ! -d "onedev/.git" ]; then
    # Try to clone the repo
    git clone ssh://git@git.plgrid.pl:7999/vfs/onedev.git
    if [ $? -eq 0 ]; then
        # The repo was cloned, update to the newest version.
        echo "OK"
        cd onedev
        git checkout develop
        git pull
    else
        # The repo was not cloned, skip
        echo "Failed, skipping."
        exit 0
    fi
else
    # The repo was not cloned, but it already exists. Update to the newest version.
    cd onedev
    git checkout develop
    git pull
fi

# Replace auth.config
cd $RUNNER_SCRIPT_DIR
echo "Replacing auth.config"
cp deps/onedev/auth.config rel/data/auth.config
exit 0
