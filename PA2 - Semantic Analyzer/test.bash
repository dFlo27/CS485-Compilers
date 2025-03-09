#!/bin/bash

../cool --parse $1 --out main
my_result=`ocaml main.ml main.cl-ast`
cool_result=`../cool --type $1 --out cool`
echo $my_result
echo $cool_result
if [[ $my_result != *"ERROR:"* ]]; then
    diff -s cool.cl-type main.cl-type
fi
