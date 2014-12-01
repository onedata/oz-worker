#!/bin/bash

## ===================================================================
## @author Tomasz Lichon
## @copyright (C): 2014 ACK CYFRONET AGH
## This software is released under the MIT license
## cited in 'LICENSE.txt'.
## ===================================================================
## @doc: This script starts distributed tests.
## ===================================================================

## compile utility test files
erlc -I ./include test_distributed/*.erl

## get host ip, to set cookie
COOKIE=`hostname -f`

## prepare
cp rel/globalregistry/etc/app.config test_distributed/sys.config
cp -R rel/resources test_distributed/
cp -R src/dao/views rel/resources/
find test_distributed -name "TEST-report.xml" -exec rm -rf {} \;

## run tests
ct_run -pa ./ebin -pa ./deps/**/ebin -pa ./test_distributed -noshell -spec test_distributed/test.spec -name tester -setcookie $COOKIE

## cleanup
rm -f test_distributed/sys.config
rm -rf test_distributed/resources
find test_distributed -name "*.beam" -exec rm -rf {} \;