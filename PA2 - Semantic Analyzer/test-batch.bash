#!/bin/bash


cases=`ls "bad-cases/"`
for i in $cases; do
    printf "Pass %s\n" $i
    i='bad-cases/'$i
    ../cool --parse $i
    parse=$i-ast
    my_result=`ocaml main.ml "$parse"`
    cool_result=`../cool --class-map $i`
    if [[ $my_result == *"ERROR:"* ]]; then
        echo $my_result
    else
        rm $i"-type"
    fi
    if [[ $cool_result == *"ERROR:"* ]]; then
        echo $cool_result
    fi
    rm "$parse"
done
