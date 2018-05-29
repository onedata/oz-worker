#!/bin/bash

# ===================================================================
# Author: Lukasz Opiola
# Copyright (C): 2014 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
# ===================================================================
# This simple script clones or updates onedev repository if it is possible
# i. e. the user has rights to clone it. If it succeeds, it swaps the example
# auth config file with the one from repository making it possible to login
# via OAuth test apps.
#
# The script is run on test_rel make target.
# ===================================================================

BRANCH="develop"

# Enter the script directory
RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd)
cd ${RUNNER_SCRIPT_DIR}

echo -n "Trying to clone/update onedev repository..."
mkdir -p _build/default/lib
cd _build/default/lib

# Check if repo exists
if [ ! -d "onedev/.git" ]; then
    # Try to clone the repo
    git clone ssh://git@git.plgrid.pl:7999/vfs/onedev.git
    if [ $? -ne 0 ]; then
        # The repo was not cloned, terminate
        echo -e "\t[FAILED]"
        exit 0
    fi
fi

# The repo was either cloned or existed. Update to the newest version.
echo -e "\t[  OK  ]"
cd onedev
git fetch
git checkout ${BRANCH}
git pull origin ${BRANCH}

# Replace auth.config
cd ${RUNNER_SCRIPT_DIR}
echo "Replacing auth.config."
cp _build/default/lib/onedev/auth.config _build/default/rel/oz_worker/etc/auth.config
exit 0
