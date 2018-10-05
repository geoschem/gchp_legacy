#!/bin/bash

segments=$( ipcs -m | grep '^0x' | awk '{print $1}' )
owners=$( ipcs -m | grep '^0x' | awk '{print $3}' | uniq )
echo $owners

if [[ -z $segments ]]
then
    echo "No shared memory segments to remove."
else
    echo "Removing segments..."
    for seg in $segments
    do
       echo "Removing segment $seg"
       ipcrm -M $seg
       if [[ ! $? ]]
       then
          echo "Unable to remove segments owned by $owners"
       fi
    done
fi

