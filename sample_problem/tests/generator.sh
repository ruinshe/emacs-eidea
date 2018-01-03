#! /usr/bin/env bash

g++ generator.cc -o generator.bin -O2 -std=gnu++0x
for i in `seq 1 5`; do
    ./generator.bin a-plus-b 0 $i > $i.in
done
for i in `seq 6 10`; do
    ./generator.bin a-plus-b 1 $i > $i.in
done
