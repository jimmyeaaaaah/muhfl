#!/bin/bash

# Options
# --try-weak-subtype: try to solve nuHFL(Z) formulas with the katsura solver with the weak subtyping rules by Burn et al. in parallel
# --remove-disjunctions: try to solve nuHFL(Z) formulas with the disjunction removal transformation in parallel
# --aggressive-simplification: simplify HFL(Z) formulas
# --timeout: timeout for each approximation refinement iteration

# --disable-add-arguments: disable adding extra integer arguments that represent the information of higher-order arguments
# --coe: manually specify coefficients for approximating least-fixpoint operators and existenatial quantifiers. Speficfy such as "1,8" (default is "1,1")
# --coe-arguments: manually specify coefficients for extra arguments. Speficfy such as "1,8" (default: "1,0")

PATH=/home/kentotanahashi/hoice/target/release:/home/kentotanahashi/eldarica:/home/katsura/bin:$PATH \
  fptprove=/home/kentotanahashi/fptprove \
  katsura_solver_path=/home/kentotanahashi/hflmc2-dev/_build/default/bin/main.exe \
  /home/kentotanahashi/muapprox-dev/_build/default/bin/muapprox_main.exe \
    --try-weak-subtype --remove-disjunctions --aggressive-simplification --timeout=300 \
    "$@"
