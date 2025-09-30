import timeit
import subprocess
import os
import sys

script_dir = os.path.dirname(os.path.realpath(__file__))

def run_experiment(experiment_id: int, bound: int, experiment_type: str):

    sys_filename = f"{script_dir}/sys.qosfsa"
    phi_filename = f"{script_dir}/phi{experiment_id}.ql"

    # print("Result: ")
    t_0 = timeit.default_timer()
    subprocess.run(["mocheqos", experiment_type, sys_filename, phi_filename, str(bound)])
    # subprocess.run(["cabal", "run", "-v0", "mocheqos", "--", experiment_type, sys_filename, phi_filename, str(bound)])
    t_1 = timeit.default_timer()
    elapsed_time = round((t_1 - t_0), 3)

    print(f"Elapsed time: {elapsed_time} s")

def run_experiment_loop(experiment_ids, bounds, experiment_types):
    for experiment_id in experiment_ids:
        # print(f"==================================")
        print(f"========== Formula Phi{experiment_id} ==========")
        print(f"==================================")
        for k in bounds:
            for experiment_type in experiment_types:
                print("")
                print(f"[Bound k = {k}, Type: {experiment_type}]")
                run_experiment(experiment_id, k, experiment_type)
        print("")

if __name__ == "__main__":
    run_experiment_loop([1,2,3,4], [18, 32], ["satisfiability", "validity"])