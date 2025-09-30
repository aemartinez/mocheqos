import timeit
import subprocess
import os
import sys

script_dir = os.path.dirname(os.path.realpath(__file__))

def run_experiment(experiment_type: str, experiment_id: int, output_filename: str, last_unfolding: int = 10):
    
    if os.path.exists(output_filename):
        os.remove(output_filename)

    for i in range(0,last_unfolding+1):
        loop = "; C -> S: read; S -> C: size; C -> S: retr; S -> C: msg; C -> S: ack"
        gchor = "[C -> A : cred; A -> C: token; C -> S: token; S -> C: ok; C -> S: helo; S -> C: int {}]".format(loop * i)
        size = 6 + 5*i
        computed_states = 2*size + 3
        emailsRetrieved = i
        dataTransferredOut = 6 + i * 500
        executionTime = 5*3 + i * 4 * 3

        clientPriceIncomingEmails = f"{i * 0.1 : .1f}"
        
        executionTimeServer = 2*3 + i * 2 * 3

        phi = ""
        if experiment_type == "sat":
            if experiment_id == 1:
                phi = "True U {} True".format(gchor)
            elif experiment_id == 2:
                phi = f"True U {gchor} qos{{(and (= emailsRetrieved {emailsRetrieved}) (<= dataTransferredOut {dataTransferredOut}) (< executionTime {executionTime}) (= clientPriceIncomingEmails {clientPriceIncomingEmails}) (< executionTimeServer {executionTimeServer}))}}"
            elif experiment_id == 3:
                phi = f"qos{{(and (<= emailsRetrieved {emailsRetrieved}) (<= dataTransferredOut {dataTransferredOut}) (< executionTime {executionTime}) (<= clientPriceIncomingEmails {clientPriceIncomingEmails}) (< executionTimeServer {executionTimeServer}))}} U {gchor} qos{{(and (= emailsRetrieved {emailsRetrieved}) (<= dataTransferredOut {dataTransferredOut}) (< executionTime {executionTime}) (= clientPriceIncomingEmails {clientPriceIncomingEmails}) (< executionTimeServer {executionTimeServer}))}}"
        elif experiment_type == "unsat":
            if experiment_id == 1:
                phi = "True U {} (Not True)".format(gchor)
            elif experiment_id == 2:
                phi = f"True U {gchor} qos{{(or (distinct emailsRetrieved {emailsRetrieved}) (> dataTransferredOut {dataTransferredOut}) (>= executionTime {executionTime}) (distinct clientPriceIncomingEmails {clientPriceIncomingEmails}) (>= executionTimeServer {executionTimeServer}))}}"
            elif experiment_id == 3:
                phi = f"qos{{(and (<= emailsRetrieved {emailsRetrieved}) (<= dataTransferredOut {dataTransferredOut}) (< executionTime {executionTime}) (<= clientPriceIncomingEmails {clientPriceIncomingEmails}) (< executionTimeServer {executionTimeServer}))}} U {gchor} qos{{(or (distinct emailsRetrieved {emailsRetrieved}) (> dataTransferredOut {dataTransferredOut}) (>= executionTime {executionTime}) (distinct clientPriceIncomingEmails {clientPriceIncomingEmails}) (>= executionTimeServer {executionTimeServer}))}}"
        with open(f"{script_dir}/phi.ql", 'w') as f:
            f.write(phi)


        with open(output_filename, "a") as f:
            f.write(f"Unfoldings: {i}\n")
            f.write(f"Size: {size}\n")
            
        with open(output_filename, "a") as f:    
            k = (size + 2)*2
            t_0 = timeit.default_timer()
            subprocess.run(["mocheqos", "satisfiability", f"{script_dir}/sys.qosfsa", f"{script_dir}/phi.ql", str(k)], stdout=f, stderr=f)
            # subprocess.run(["cabal", "run", "-v0", "mocheqos", "--", "satisfiability", f"{script_dir}/sys.qosfsa", f"{script_dir}/phi.ql", str(k)], stdout=f, stderr=f)
            t_1 = timeit.default_timer()
            elapsed_time = round((t_1 - t_0), 3)

        with open(output_filename, "a") as f:
            f.write(f"Elapsed time: {elapsed_time} s\n\n")

        os.remove(f"{script_dir}/phi.ql")

def run_experiment_loop(experiment_types, experiment_ids):
    for experiment_type in experiment_types:
        for experiment_id in experiment_ids:
            print(f"Running experiment {experiment_type} {experiment_id}")
            dir = f"{script_dir}/results/{experiment_type}"
            os.makedirs(dir, exist_ok=True)
            output_filename = f"{dir}/experiment-{experiment_id}.out"
            
            if experiment_type == "sat-subset":
                run_experiment("sat", experiment_id, output_filename, 8)
            elif experiment_type == "unsat-subset":
                run_experiment("unsat", experiment_id, output_filename, 8)
            else: 
                run_experiment(experiment_type, experiment_id, output_filename)

if __name__ == "__main__":

    experiment_types = ["sat", "unsat", "sat-subset", "unsat-subset"]
    experiment_ids = [1, 2, 3]
    
    if len(sys.argv) == 1:
        run_experiment_loop(["sat", "unsat"], [1, 2, 3])

    elif len(sys.argv) == 2:
        experiment_type = sys.argv[1]
        if experiment_type not in experiment_types:
            print("Invalid experiment type")
            sys.exit(1)
        run_experiment_loop([experiment_type], [1, 2, 3])

    elif len(sys.argv) == 3:
        experiment_type = sys.argv[1]
        experiment_id = int(sys.argv[2])

        if experiment_type not in experiment_types:
            print("Invalid experiment type")
            sys.exit(1)
        if experiment_id not in experiment_ids:
            print("Invalid experiment id")
            sys.exit(1)

        run_experiment_loop([experiment_type], [experiment_id])

    else:
        print("Usage: python3 run-experiments.py [sat|unsat|sat-subset|unsat-subset] [1|2|3]")
        sys.exit(1)