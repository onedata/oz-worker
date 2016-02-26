#!/bin/sh

if [ "$ONEPANEL_MULTICAST_ADDRESS" ]; then
	sed -i s/239.255.0.1/"$ONEPANEL_MULTICAST_ADDRESS"/g /opt/oz_worker/nodes/onepanel/etc/app.config
fi

sed -i s/onepanel@.*/onepanel@`hostname -f`/g /opt/oz_worker/nodes/onepanel/etc/vm.args
sed -i -e s/"^-setcookie .*"/"-setcookie oz_worker"/g /opt/oz_worker/nodes/onepanel/etc/vm.args
service onepanel start

while true; do sleep 60; done
