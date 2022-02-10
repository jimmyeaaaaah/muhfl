#!/bin/bash

# Options
#  --solver=solver_name (default: hoice)
#      Choose background CHC solver. Available: z3, hoice, fptprover
#  --mode-burn-et-al
#      Use weak subtying rules in Burn et al.
#  --remove-disjunctions
#      Solve a formula with the disjunction removal transformation
#  --show-refinement
#      Show refinement types.
#  --no-disprove
#      Disable disproving
#  --no-inlining
#      Disable inlining
#  --pcsat-config=STRING (default: solver/pcsat_tb.json)
#      path of config for pcsat (e.g., --pcsat-config=solver/pcsat_dt.json )
#  --solve-dual=STRING (absent=auto)
#      "auto" (default): automatically solve dual or non-dual chc, depends
#      on the sizes of the dual and non-dual chc (solve smaller one) / "dual": always solve dual chc
#      / "non-dual": always solve non-dual chc / "auto-conservative" : solve
#      non-dual chc, unless the non-dual chc is extended chc and the dual chc is not (in the experiment in the paper, the "auto-conservative" option is used)

PATH=/home/kentotanahashi/hoice/target/release:/home/kentotanahashi/eldarica:/home/katsura/bin:$PATH \
  fptprove=/home/kentotanahashi/fptprove \
  /home/kentotanahashi/hflmc2-dev/_build/default/bin/main.exe \
    --solve-dual=auto-conservative \
    "$@"
