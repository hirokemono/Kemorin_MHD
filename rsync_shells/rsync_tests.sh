#!/bin/sh
#
cd ~/git/Kemorin_MHD/
#
echo 'syncronize test data to Calypso'
rsync -avz --copy-links --delete tests/Calypso/*  ~/git/Calypso/tests/
echo 'syncronize example data to Calypso'
rsync -avz --copy-links --delete examples/Calypso/*  ~/git/Calypso/examples/
