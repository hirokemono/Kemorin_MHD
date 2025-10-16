#!/bin/sh
#
cd ~/git/Kemorin_MHD/
#
echo 'syncronize to CalypsoView'
rsync -avz --copy-links --delete SYNC_to_VIEWER/* ~/git/CalypsoView/src/
