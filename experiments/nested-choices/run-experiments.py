import timeit
import subprocess
import sys
import os

script_dir = os.path.dirname(os.path.realpath(__file__))
results_dir = os.path.join(script_dir, "results")
os.makedirs(results_dir, exist_ok=True)

sample_size = 0
if len(sys.argv) == 1:
    sample_size = 100
elif len(sys.argv) == 2:
    sample_size = int(sys.argv[1])
else:
    print("Usage: python3 run-experiments.py [sample-size]")
    exit(1)

for filename in os.listdir(results_dir):
    os.remove(os.path.join(results_dir, filename))

for sys_height in range(1,11):

    print(f"Running experiments for nested choices = {sys_height}.")

    output_filename = f"{results_dir}/{sys_height}.output"
    if os.path.exists(output_filename):
        os.remove(output_filename)

    with open(output_filename, "w") as f:
        f.write("")

    for i in range(1,sample_size+1):

        output = f"Prop: {i}\n"
        with open(output_filename, "a") as f:
            f.write(output)

        k = 100

        with open(output_filename, "a") as f:
            t_0 = timeit.default_timer()
            subprocess.call(["mocheqos", "satisfiability", f"{script_dir}/sys/sys-{sys_height}.qosfsa", f'{script_dir}/prop/{sys_height}/prop-{i}.ql', str(k)], stdout=f, stderr=f)
            t_1 = timeit.default_timer()

        elapsed_time = round((t_1 - t_0), 3)

        output = f"Elapsed time: {elapsed_time} s\n\n"
        with open(output_filename, "a") as f:
            f.write(output)