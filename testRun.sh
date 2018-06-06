#!/bin/bash

for i in $(seq 1 11); 
do
    printf 'problem #%d\n' $i
    timeout 100m prolog -G10g -L10g -T10g -s testInstances.pl -t 'ex4:measure(_, '$i').'
    printf '\n exit code: %s' $?
    printf '\n\n-------------------------------------------\n\n'
done