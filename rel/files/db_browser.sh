#!/usr/bin/env bash

OZ_WORKER_PROCESS=`ps aux | grep oz_worker | grep run_erl`
if [ -z "${OZ_WORKER_PROCESS}" ]; then
    echo "ERROR: oz_worker application doesn't seem to be running (tried: 'ps aux | grep oz_worker | grep run_erl')."
    echo "Note that oz_worker must be started in detached mode for this to work (using 'start' rather than 'console')."
    exit 1
fi

TO_ERL_PROCESS=`pgrep to_erl`
if [ ! -z "${TO_ERL_PROCESS}" ]; then
    echo "ERROR: It seems that another process is already attached to oz_worker. You must first detach (Ctrl+D) or kill the mentioned process ('kill ${TO_ERL_PROCESS}')."
    exit 1
fi

RUN_ERL_PATH=`echo ${OZ_WORKER_PROCESS} | awk -v RS=' ' '/run_erl$/'`
TO_ERL_PATH=`echo ${RUN_ERL_PATH} | sed 's/run_erl/to_erl/'`
DAEMON=`echo ${OZ_WORKER_PROCESS} | sed -n -e 's/^.*-daemon //p' | cut -d' ' -f1`

OUTPUT_FILE=`mktemp`
LOCK_FILE=`mktemp --dry-run`

${TO_ERL_PATH} ${DAEMON} &>/dev/null <<- EOF
terminate_previous_input_with_a_dot.
db_browser:call_from_script("${OUTPUT_FILE}", "${@}").
% This lets the script know that the operation has finished
file:write_file("${LOCK_FILE}", <<"lock">>).
EOF

# The lock file will be created after the output is dumped to
# the output file. Wait for the lock file and then display
# the output to ensure everything is printed.
while [ ! -f ${LOCK_FILE} ]; do sleep 0.05; done

cat ${OUTPUT_FILE}
rm -f ${OUTPUT_FILE} ${LOCK_FILE}
