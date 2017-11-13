#!/bin/sh
for (( i=0; i <= 3; i++ ))
do
    echo "executing main$i"
    stack exec -- CompareMethods $i +RTS -l -N4
    cp CompareMethods.eventlog "main$i.eventlog"
done

for (( i=0; i <= 3; i++ ))
do
    threadscope "main$i.eventlog" &
done

echo "OK"
