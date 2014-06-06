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
IFCONFIG_LINE=`ifconfig | grep "inet addr:.*Bcast:"`
COLON_INDEX=`awk -v a="$IFCONFIG_LINE" -v b=":" 'BEGIN{print index(a, b)}'`
BCAST_INDEX=`awk -v a="$IFCONFIG_LINE" -v b="Bcast" 'BEGIN{print index(a, b)}'`
COOKIE=${IFCONFIG_LINE:COLON_INDEX:((BCAST_INDEX - COLON_INDEX - 3))}

#prepare
cp rel/files/app.config test_distributed/sys.config

# Run tests
ct_run -pa ./deps/**/ebin -noshell -spec test_distributed/test.spec -name tester -setcookie $COOKIE

#cleanup
rm -f test_distributed/sys.config
