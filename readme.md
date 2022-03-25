# MuHFL

A full-HFLz solver

## Installation

The solver can be built with docker.

```bash
cd docker

# build
# the ssh key is for kamocyc/hflmc2.git, kamocyc/muapprox.git, ketanahashi/fptprove.git repository
DOCKER_BUILDKIT=1 docker build --progress=plain --secret id=ssh,src=<path_to_ssh_private_key> .

# run
docker run -v <path_to_directory_containing_input_files>:/home/opam/inputs/ <image_id> muapprox_main /home/opam/inputs/<input_file_name>

# examples
docker run -v <path_to_repositoy>/benchmark/inputs:/home/opam/inputs/ <image_id> muapprox_main /home/opam/inputs/termination/sum.in                  # valid
docker run -v <path_to_repositoy>/benchmark/inputs:/home/opam/inputs/ <image_id> muapprox_main /home/opam/inputs/nontermination/fib_CPS_nonterm.in   # valid
docker run -v <path_to_repositoy>/benchmark/inputs:/home/opam/inputs/ <image_id> muapprox_main /home/opam/inputs/termination/notused/sum-invalid.in  # invalid
```

## Run benchmark

1. In the repository's root directory,
    run ``python3 memory_watchdog.py &``.
    This script kills the (backend) solver if it uses excessive memory.
2. ``cd benchmark``
3. ``python3 bench1.py --timeout <timeout for the solver> --benchmark <text file with a list of input files (in benchmark/file_list directory)> <script to run the solver (in benchmark directory )>``
    * e.g. ``python3 bench1.py --timeout 900 --benchmark all_paper_t muapprox_katsura``.

* Benchmark used in the paper
  * ``all_paper_t`` for Experiment 1, ``ho`` for Experiment 2 (``run_ho_test.sh`` script), and ``prog2`` for Experiment 3
  * ``muapprox_katsura`` script is used

## Scripts (in the repository's root directory)

* ``x``: shortcut to run the solver
* ``killp.sh``: Kill zombie processes (The script kills processes with the current user and with specific process names)
* ``clear.sh``: Remove temporary files created in the tool's working directory

## Show help

``docker run <image_id> muapprox_main --help | less``

## Installation of other backend nu-HFL(Z) solvers (optional)

### PaHFL (developed by Iwayama)

#### Installation

```bash
git clone https://github.com/Hogeyama/hflmc2.git
# Then, build with Dockerfile
#
```

Set the full path of the solver executable to the environment variable ``iwayama_solver_path``.

#### Older version

```bash
git clone git@github.com:kamocyc/hflmc2-1.git
cd hflmc2-1
git checkout -b old
# Then, build with Dockerfile
#
```

### First-order nu-HFL(Z) solver

#### Installation

```bash
git clone https://github.com/kamocyc/nu_only_mu_arithmetic_solver
# Then, build according to readme.md
#
```

Set the full path of the solver executable to the environment variable ``first_order_solver_path``.
