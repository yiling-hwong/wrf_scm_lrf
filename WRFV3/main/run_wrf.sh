#!/bin/bash

cd $1

./run_me_first.csh
./wrf.exe &> run.log

