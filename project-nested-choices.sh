#!/bin/bash
for i in {1..10}; 
do
    echo "Projecting sys-$i.qosgc";
    project-gc -D min -qos experiments/nested-choices/sys/sys-$i.qosgc > experiments/nested-choices/sys/sys-$i.qosfsa; 
done