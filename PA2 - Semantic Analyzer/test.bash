#!/bin/bash

../cool --parse $1
parse=`dirname $1`/`basename $1 .cl`.cl-ast
my_result=`ocaml type.ml "$parse"`
cool_result=`../cool --type $1`
if [[ $my_result == *"ERROR:"* ]]; then
    echo $my_result
else
    printf "%s\n" "$my_result"
fi
if [[ $cool_result == *"ERROR:"* ]]; then
    echo $cool_result
else
    rm "`dirname $1`/`basename $1 .cl`.cl-type"
fi
rm "$parse"

