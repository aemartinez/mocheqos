import sys
import numpy as np
import matplotlib.pyplot as plt
import os

script_dir = os.path.dirname(os.path.realpath(__file__))

if len(sys.argv) != 2:
    print('Usage: python3 plot-results.py [sat|unsat|sat-subset|unsat-subset]')
    exit(1)

type = sys.argv[1]
experiment_types = ["sat", "unsat", "sat-subset", "unsat-subset"]
if not (type in experiment_types):
    print('Invalid type')
    exit(1)
input_dir = f"{script_dir}/results/{type}"
output_dir = f"{script_dir}/results"

filename1 = f"{input_dir}/experiment-1.out"
filename2 = f"{input_dir}/experiment-2.out"
filename3 = f"{input_dir}/experiment-3.out"

def load_data(filename):
    # Read data from file
    with open(filename) as f:
        data = f.readlines()

    xdata = []
    ydata = []
    for line in data:
        if line.startswith('Unfoldings:'):
            xdata.append(float(line.split()[1]))
        elif line.startswith('Elapsed time:'):
            ydata.append(float(line.split()[2]))

    xdata = np.array(xdata)
    ydata = np.array(ydata)

    return xdata, ydata

xdata, ydata = load_data(filename1)
xdata2, ydata2 = load_data(filename2)
xdata3, ydata3 = load_data(filename3)

if type == 'sat' or type == 'sat-subset':
    labelA = "True U [G] True"
    labelB = "True U [G] qos{ ... }"
    labelC = "qos{ ... } U [G] qos{ ... }"
elif type == 'unsat' or type == 'unsat-subset':
    labelA = "True U [G] False"
    labelB = "True U [G] qos{ ... }"
    labelC = "qos{ ... } U [G] qos{ ... }"

# Plot the data 
plt.plot(xdata, ydata, 'o-', label=labelA)
plt.plot(xdata2, ydata2, '.-', label=labelB)
plt.plot(xdata3, ydata3, '*-', label=labelC)

# Add axis labels and legend
plt.xlabel('n = number of loop unfoldings in G', fontsize=13)
plt.ylabel('Time (s)', fontsize=13)

plt.xticks(fontsize=12)
plt.yticks(fontsize=12)

plt.legend(fontsize=12)

plt.grid(True)

# if type == 'sat':
#     plt.title('Performance over satisfiable formulas')
# elif type == 'unsat':
#     plt.title('Performance over unsatisfiable formulas')

# Save the plot
output_filename = f"{output_dir}/plot-{type}.pdf"
# if type == 'sat':
#     output_filename = f"{output_dir}/plot-sat.pdf"
# elif type == 'unsat':
#     output_filename = f"{output_dir}/plot-unsat.pdf"

plt.savefig(output_filename)