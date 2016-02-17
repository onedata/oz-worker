#!/bin/sh

if [ "$ONEPANEL_MULTICAST_ADDRESS" ]; then
	sed -i s/239.255.0.1/"$ONEPANEL_MULTICAST_ADDRESS"/g /opt/globalregistry/nodes/onepanel/etc/app.config;
fi

sed -i s/onepanel@.*/onepanel@`hostname -f`/g /opt/globalregistry/nodes/onepanel/etc/vm.args;
service onepanel start
bash
