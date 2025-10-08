#!/bin/sh
#
cd ~/git/Kemorin_MHD/
#
echo 'syncronize to Calypso'
rsync -avz --copy-links --delete SYNC_to_RELEASE/* ~/git/Calypso/src/
