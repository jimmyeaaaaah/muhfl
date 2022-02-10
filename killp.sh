#!/bin/bash
u=$(whoami)

killall --user $u -9 main.exe
killall --user $u -9 muapprox_main.exe
killall --user $u -9 muapprox_main.e
killall --user $u -9 z3
killall --user $u -9 hoice
killall --user $u -9 hflmc2
killall --user $u -9 eld
killall --user $u -9 java
killall --user $u -9 hflmc3.sh
killall --user $u -9 para_aux.sh
killall --user $u -9 -r "fptprove_launch_script_para_aux*"
killall --user $u -9 -r "fptprove_launch_script*"
# killall --user $u -9 sh
killall --user $u -9 timeout
killall --user $u -9 horsat2
killall --user $u -9 less
killall --user $u -9 hflmc
killall --user $u -9 rcaml-smt
killall --user $u -9 mochi.exe
