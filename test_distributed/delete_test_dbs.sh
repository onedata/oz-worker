#!/bin/bash

## ===================================================================
## @author Krzysztof Trzepla
## @copyright (C): 2014 ACK CYFRONET AGH
## This software is released under the MIT license
## cited in 'LICENSE.txt'.
## ===================================================================
## @doc: This script deletes test databases.
## ===================================================================

hostname=127.0.0.1
port=5984
username=admin
password=password

# Delete unprotected tests databases
DBS=`curl -X GET ${hostname}:${port}/_all_dbs | tr -d "["  | tr -d "]" | tr -d "\"" | tr , "\n"`
for DB in ${DBS}; do
	SUFFIX=`echo ${DB:${#DB} - 5}`
	if [ "$SUFFIX" == "_test" ];
	then
		echo -n "Deleting database: ${DB}"
		curl -X DELETE http://${hostname}:${port}/${DB}
	fi
done

# Delete protected tests databases
DBS=`curl -X GET ${hostname}:${port}/_all_dbs | tr -d "["  | tr -d "]" | tr -d "\"" | tr , "\n"`
for DB in ${DBS}; do
        SUFFIX=`echo ${DB:${#DB} - 5}`
        if [ "$SUFFIX" == "_test" ];
        then
		echo -n "Deleting database: ${DB}"
		curl -u ${username}:${password} -X DELETE http://${hostname}:${port}/${DB}
	fi
done
