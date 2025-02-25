#!/bin/bash

../cool --parse $1
parse=`dirname $1`/`basename $1 .cl`.cl-ast
my_result=`ocaml main.ml "$parse"`
if [[ $my_result == *"ERROR:"* ]]; then
    echo $my_result
fi
cool_result=`../cool --class-map $1`
if [[ $cool_result == *"ERROR:"* ]]; then
    echo $cool_result
else
    rm "`dirname $1`/`basename $1 .cl`.cl-type"
fi
rm "$parse"

