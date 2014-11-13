#!/bin/bash

## ===================================================================
## @author Tomasz Lichon
## @copyright (C): 2014 ACK CYFRONET AGH
## This software is released under the MIT license
## cited in 'LICENSE.txt'.
## ===================================================================
## @doc: This script starts distributed tests.
## ===================================================================

# Get host ip, to set cookie
COOKIE=`hostname -f`

#prepare
cp rel/app.config test_distributed/sys.config
cp -R rel/resources test_distributed/
sed -i "s/{globalregistry,.*/{globalregistry_test, [/g" test_distributed/sys.config
find test_distributed -name "TEST-report.xml" -exec rm -rf {} \;

# Run tests
ct_run -pa ./deps -pa ./deps/**/ebin -noshell -spec test_distributed/test.spec -name tester -setcookie $COOKIE

#cleanup
rm -f test_distributed/sys.config
rm -rf test_distributed/resources
