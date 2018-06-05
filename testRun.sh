#!/bin/bash

for i in $(seq 1 11); 
do
    timeout 10 prolog -G0 -L0 -T0 -s testInstances.pl -t 'ex4:measure(_, '$i').'
    printf '\n exit code: %s' $?
    printf '\n\n-------------------------------------------\n\n'
done