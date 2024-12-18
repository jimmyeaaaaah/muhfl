from pprint import pprint
import json
import os
import subprocess
import time
import re
import argparse
import glob
import sys
import copy

OUTPUT_FILE_NAME = "0bench_out_append.txt"

def append(text):
    with open(OUTPUT_FILE_NAME, 'a') as f:
        f.write(str(text))

def is_process_running(process_name):
    os.system("ps -ef | grep \"" + process_name + "\" | grep -v grep | awk '{ print $2 }' > __result2.tmp")
    try:
        with open('__result2.tmp', 'r') as f:
            text = f.read().strip()
    except:
        return is_process_running(process_name)
    
    return text != ''

def prepare():
    global timeout, lists_path, base_dir, exe_path, add_args
    
    if not is_process_running("python3 memory_watchdog.py"):
        print('########################')
        print('Error: memory_watchdog.py IS NOT RUNNING')
        print('########################')
        exit(1)

    # set path
    if os.path.basename(os.getcwd()) == 'benchmark':
        os.chdir('..')

    os.system('./clean.sh')
    print('building...')
    os.system('./x')
    
    os.chdir("benchmark")

    if not os.path.exists("output"):
        os.mkdir("output")

    os.chdir("output")

    with open(OUTPUT_FILE_NAME, 'w') as f:
        pass

    BACKEND_SOLVER_CANDIDATE = ['muapprox_first_order', 'muapprox_katsura', 'muapprox_iwayama', 'muapprox_suzuki', 'muapprox_katsura_replacer', 'muapprox_mochi', 'muapprox_katsura_no_options']

    parser = argparse.ArgumentParser(description='benchmarker.')
    parser.add_argument('backend_solver', metavar='backend_solver', type=str, 
                        choices=BACKEND_SOLVER_CANDIDATE,
                        help='backend solver name')
    parser.add_argument('--timeout', dest='timeout', action='store', type=int, required=True,
                        help='timeout')
    parser.add_argument('--benchmark', dest='benchmark', action='store', type=str, required=True,
                        help='benchmark set')
    parser.add_argument('--pass-args', dest='pass_args', action='store', type=str,
                        help='additional arguments to pass them to the Hflz solver')

    args = parser.parse_args()
    backend_solver_name = args.backend_solver
    timeout = float(args.timeout)
    benchmark = args.benchmark
    add_args = args.pass_args
    if add_args == None:
        add_args = []
    else:
        add_args = add_args.split(" ")

    benchmark_dir = os.path.dirname(os.getcwd())
    
    lists_path = os.path.join(benchmark_dir, 'file_list/' + benchmark + '.txt')    # lists_path
    base_dir = os.path.join(benchmark_dir, 'inputs')                               # base_dir
    exe_path = os.path.join(benchmark_dir, 'run_' + backend_solver_name + '.sh')   # exe_path
    
    print("START")
    pprint({
        "backend_solver_name": backend_solver_name,
        "timeout":    timeout,
        "lists_path": lists_path,
        "base_dir":   base_dir,
        "exe_path":   exe_path,
        "add_args":   add_args,
    })
    
    return benchmark
    
def extract_result(text):
    try:
        m = re.search(r'\n\[\[MAIN]] Verification Result:\n\s*(\w+)\n', text)
        if m == None:
            return ('other', '')
        status = m.group(1)
        
        m = re.search(r'\n(\(mode=.+\))\n', text)
        if m == None:
            return (status, '')
        info = m.group(1)
        return (status, info)
    except IndexError:
        return ('other', 'info')
    
def preexec_fn():
    os.chdir('./')
    os.setsid()

def readfile(path):
    with open(path, 'r', errors='ignore') as f:
        return f.read()
    
def run(cmd):
    time.sleep(3.0)
    print("CMD: ")
    print(cmd, flush=True)
    
    st = time.perf_counter()
    elapsed = timeout
    timed_out = False
    result = subprocess.run(["timeout", str(timeout)] + cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, preexec_fn=preexec_fn, encoding='utf8')
    if result.returncode == 124:
        timed_out = True
    else:
        ed = time.perf_counter()
        elapsed = ed - st

    stdout = readfile("/tmp/stdout_1.txt")
    stderr = readfile("/tmp/stderr_1.txt")
    
    os.system('../../killp.sh 2> /dev/null')
    
    return stdout, elapsed, stderr, timed_out

def get_data(file, result):
    def get_file_pattern(prefix, mode):
        return prefix + "_" + os.path.splitext(os.path.basename(file))[0] + "_" + mode + "_*.tmp"
    
    def load_jsons_data(filename):
        with open(filename, 'rb') as f:
            json_str = subprocess.Popen(["jq", "-cs", "."], stdout=subprocess.PIPE, stdin=subprocess.PIPE).communicate(f.read())[0]
            return json.loads(json_str.decode('utf-8'))
    
    
    def get(mode, pre_or_post):
        # try:
            files = glob.glob(get_file_pattern(pre_or_post, mode))
            if files == []:
                print("get (" + pre_or_post + "): not found (" + mode + ")")
                return []
            if len(files) > 1:
                print("WARN 1")
            
            print("file: " + files[0])
            
            data = load_jsons_data(files[0])
            os.remove(files[0])
            return data
        
    def get_post_merged(mode, d):
        # try:
            files = glob.glob(get_file_pattern("post_merged", mode))
            res = []
            for file2 in files:
                print("file: " + file2)
                data = load_jsons_data(file2)
                # if not (len(data) == 1):
                #     print("Warn: get_post_merged, len(data) <> 1")
                
                res.append(data[-1])
                os.remove(file2)
            
            res = sorted(res, key=lambda r: int(r["iter_count"]))
            
            if res == []:
                {}
            else:
                result2 = result['result']
                match = (result2 == 'invalid' and mode == 'disprover') or (result2 == 'valid' and mode == 'prover')
                cs = [r["iter_count"] for r in res]
                if max(cs) > d['iter_count'] or (match and max(cs) != d['iter_count']):
                    print(d['iter_count'])
                    print(max(cs))
                    print("WARN 2")
                    
                for i in range(max(cs)):
                    if cs.count(i+1) != 1:
                        print("WARN 3")
                
                return {
                    "iter_count": '|'.join([str(c["iter_count"]) for c in res]),
                    "t_count": '|'.join([str(c["t_count"]) for c in res]),
                    "s_count": '|'.join([str(c["s_count"]) for c in res]),
                    "solved_by": '|'.join([c["solved_by"] for c in res]),
                    "elapsed_all": sum([c["elapsed_all"] for c in res]),
                    "hflz_size": '|'.join([str(c["hflz_size"]) for c in res]),
                    "hflz_inlined_size": '|'.join([str(c["hflz_inlined_size"]) for c in res]),
                    "hflz_pred_num": '|'.join([str(c["hflz_pred_num"]) for c in res]),
                    "hflz_inlined_pred_num": '|'.join([str(c["hflz_inlined_pred_num"]) for c in res]),
                    "elapsed_all_string": '|'.join([str(c["elapsed_all"]) for c in res]),
                }
            
    data = {}
    data['pre_prover'] = get('prover', 'pre')
    data['pre_disprover'] = get('disprover', 'pre')
    
    data['post_prover'] = get('prover', 'post')
    data['post_disprover'] = get('disprover', 'post')
    
    data['is_nu_hflz'] = False
    
    if len(data['pre_prover']) == 0 and len(data['pre_disprover']) == 0:
        data['pre_prover'] = get('solver', 'pre')
        data['pre_disprover'] = copy.deepcopy(data['pre_prover'])
        
        data['post_prover'] = get('solver', 'post')
        data['post_disprover'] = copy.deepcopy(data['post_prover'])
        
        data['is_nu_hflz'] = True
    
    data['post_merged_prover'] = get_post_merged('prover', data['pre_prover'][-1])
    data['post_merged_disprover'] = get_post_merged('disprover', data['pre_disprover'][-1])
    
    return data

# stderr, stdout, result, info
def parse_stdout(full_stdout, stderr):
    result_data = {}
    
    if stderr is None:
        stderr = ''
    
    result_data['stderr'] = stderr
    result_data['result'], result_data['info'] = extract_result(full_stdout)
    
    if result_data['result'] == 'other':
        result_data['stdout'] = full_stdout
        
    return result_data
    
def gen_cmd(exe_path, file):
    cmd_template = [exe_path]  # <option> <filename>
    
    cmd_template.append(file)
    
    for _, add_arg in enumerate(add_args):
        cmd_template.append(add_arg)
    
    return cmd_template

def log_file(file):
    with open('current.txt', 'w') as f:
        f.write(    file)
    
results = []
def handle(exe_path, file):
    print("file: " + file, flush=True)
    append({'file': file})
    log_file(file)
    
    cmd = gen_cmd(exe_path, file)
    stdout, t, stderr, timed_out = run(cmd)
    if not timed_out:
        result = parse_stdout(stdout, stderr)
    else:
        if 1 == 0:  # trick for the type-checker
            stdout = 1
        result = {
            "result": "timeout",
            "info": '',
            "stdout": stdout,
            "stderr": stderr,
        }
    
    # print({'result': result})
    result['time'] = t
    result['data'] = get_data(file, result)
        
    append({'result': result})
    
    result['file'] = file
    results.append(result)

def to_table(data):
    lines = []
    for _, row in enumerate(data):
        lines.append(row['result'] + '\t' + str(row['time']) + '\n')
    
    return lines

def main_sub():
    with open(lists_path) as f:
        files = [line for line in [l.strip() for l in f.readlines()] if line != '' and line[0] != '#']
    
    for file in files:
        handle(exe_path, os.path.join(base_dir, file))
        
        with open('0bench_out_full.txt', 'w') as f:    
            f.write(json.dumps(results, indent=2))

        with open(OUTPUT_FILE_NAME + '_table.txt', 'w') as f:
            f.writelines(to_table(results))
    
def main(benchmark):
    try:
        main_sub()
        print("FINISHED")
    except KeyboardInterrupt:
        devnull = os.open(os.devnull, os.O_WRONLY)
        os.dup2(devnull, sys.stdout.fileno())
    except:
        print("Exception occured")
        
    os.system("""
        jq -r '[.[] | {
            file: .file,
            result: .result,
            time: .time,
            prove_iter_count: .data.pre_prover[-1].iter_count,
            disprove_iter_count: .data.pre_disprover[-1].iter_count,
            prover_t_count: .data.pre_prover[-1].t_count,
            prover_s_count: .data.pre_prover[-1].s_count,
            disprover_t_count: .data.pre_disprover[-1].t_count,
            disprover_s_count: .data.pre_disprover[-1].s_count,
            prover_elapsed_all: .data.post_merged_prover.elapsed_all,
            disprover_elapsed_all: .data.post_merged_disprover.elapsed_all,
            prover_elapsed_all_string: .data.post_merged_prover.elapsed_all_string,
            disprover_elapsed_all_string: .data.post_merged_disprover.elapsed_all_string,
            prover_will_try_weak_subtype: .data.post_prover[-1].will_try_weak_subtype,
            disprover_will_try_weak_subtype: .data.post_disprover[-1].will_try_weak_subtype,
            l_prover_hflz_size: .data.pre_prover[-1].hflz_size,
            l_prover_hflz_inlined_size: .data.pre_prover[-1].hflz_inlined_size,
            l_prover_hflz_pred_num: .data.pre_prover[-1].hflz_pred_num,
            l_prover_hflz_inlined_pred_num: .data.pre_prover[-1].hflz_inlined_pred_num,
            l_disprover_hflz_size: .data.pre_disprover[-1].hflz_size,
            l_disprover_hflz_inlined_size: .data.pre_disprover[-1].hflz_inlined_size,
            l_disprover_hflz_pred_num: .data.pre_disprover[-1].hflz_pred_num,
            l_disprover_hflz_inlined_pred_num: .data.pre_disprover[-1].hflz_inlined_pred_num,
            prover_solved_by: .data.post_merged_prover.solved_by,
            disprover_solved_by: .data.post_merged_disprover.solved_by,
            is_nu_hflz: .data.is_nu_hflz,
            prover_iter_count: .data.post_merged_prover.iter_count,
            prover_hflz_size: .data.post_merged_prover.hflz_size,
            prover_hflz_inlined_size: .data.post_merged_prover.hflz_inlined_size,
            prover_hflz_pred_num: .data.post_merged_prover.hflz_pred_num,
            prover_hflz_inlined_pred_num: .data.post_merged_prover.hflz_inlined_pred_num,
            disprover_iter_count: .data.post_merged_disprover.iter_count,
            disprover_hflz_size: .data.post_merged_disprover.hflz_size,
            disprover_hflz_inlined_size: .data.post_merged_disprover.hflz_inlined_size,
            disprover_hflz_pred_num: .data.post_merged_disprover.hflz_pred_num,
            disprover_hflz_inlined_pred_num: .data.post_merged_disprover.hflz_inlined_pred_num,
            m_prover_t_count: .data.post_merged_prover.t_count,
            m_prover_s_count: .data.post_merged_prover.s_count,
            m_disprover_t_count: .data.post_merged_disprover.t_count,
            m_disprover_s_count: .data.post_merged_disprover.s_count
            }]
            | .[] | "\\(.file)\t\\(.prove_iter_count)\t\\(.disprove_iter_count)\t\\(.prover_t_count)\t\\(.prover_s_count)\t\\(.disprover_t_count)\t\\(.disprover_s_count)\t\\(.prover_elapsed_all)\t\\(.disprover_elapsed_all)\t\\(.prover_will_try_weak_subtype)\t\\(.disprover_will_try_weak_subtype)\t\\(.is_nu_hflz)\t\\(.prover_elapsed_all_string)\t\\(.disprover_elapsed_all_string)\t\\(.l_prover_hflz_size)\t\\(.l_prover_hflz_inlined_size)\t\\(.l_prover_hflz_pred_num)\t\\(.l_prover_hflz_inlined_pred_num)\t\\(.l_disprover_hflz_size)\t\\(.l_disprover_hflz_inlined_size)\t\\(.l_disprover_hflz_pred_num)\t\\(.l_disprover_hflz_inlined_pred_num)\t\\(.prover_solved_by)\t\\(.disprover_solved_by)\t\\(.prover_iter_count)\t\\(.prover_hflz_size)\t\\(.prover_hflz_inlined_size)\t\\(.prover_hflz_pred_num)\t\\(.prover_hflz_inlined_pred_num)\t\\(.disprover_iter_count)\t\\(.disprover_hflz_size)\t\\(.disprover_hflz_inlined_size)\t\\(.disprover_hflz_pred_num)\t\\(.disprover_hflz_inlined_pred_num)\t\\(.m_prover_t_count)\t\\(.m_prover_s_count)\t\\(.m_disprover_t_count)\t\\(.m_disprover_s_count)"' 0bench_out_full.txt > """ + OUTPUT_FILE_NAME + "_iter_count.txt")
    
    os.system("paste " + OUTPUT_FILE_NAME + '_table.txt' + ' ' + OUTPUT_FILE_NAME + "_iter_count.txt > " + OUTPUT_FILE_NAME + "_summary.txt")
    
    # result,time,file,prove_iter_count,disprove_iter_count,prover_t_count,prover_s_count,disprover_t_count,disprover_s_count,prover_elapsed_all,disprover_elapsed_all,prover_will_try_weak_subtype,disprover_will_try_weak_subtype,is_nu_hflz,prover_elapsed_all_string,disprover_elapsed_all_string,l_prover_hflz_size,l_prover_hflz_inlined_size,l_prover_hflz_pred_num,l_prover_hflz_inlined_pred_num,l_disprover_hflz_size,l_disprover_hflz_inlined_size,l_disprover_hflz_pred_num,l_disprover_hflz_inlined_pred_num,prover_solved_by,disprover_solved_by,prover_iter_count,prover_hflz_size,prover_hflz_inlined_size,prover_hflz_pred_num,prover_hflz_inlined_pred_num,disprover_iter_count,disprover_hflz_size,disprover_hflz_inlined_size,disprover_hflz_pred_num,disprover_hflz_inlined_pred_num,m_prover_t_count,m_prover_s_count,m_disprover_t_count,m_disprover_s_count
    print("time: " + os.path.join(os.getcwd(), OUTPUT_FILE_NAME + "_summary.txt"))
    print("list: " + os.path.join(os.getcwd(), lists_path))
    print("full: " + os.path.join(os.getcwd(), "0bench_out_full.txt"))
    
    os.chdir("..")
    os.system("bash ho2.sh file_list/" + benchmark + ".txt 2> /dev/null")

benchmark = prepare()

main(benchmark)
