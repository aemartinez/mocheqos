import re
import numpy as np
import matplotlib.pyplot as plt
import os

script_dir = os.path.dirname(os.path.realpath(__file__))
results_dir = os.path.join(script_dir, "results")

# Regular expression pattern to extract the elapsed time from each file
pattern = r"Elapsed time: (\d+\.\d+) s"

# List to store the elapsed times from each file
elapsed_times = []

# Loop through each file and extract the elapsed time
for i in range(1, 11):
    filename = f"{results_dir}/{i}.output"
    with open(filename, "r") as f:
        data = f.read()
        times = re.findall(pattern, data)
        times = [float(time) for time in times]
        elapsed_times.append(times)

# Create the box plot
fig, ax = plt.subplots()
bp = ax.boxplot(elapsed_times)
ax.set_xticklabels([f"{i}" for i in range(1, 11)], fontsize=12)
ax.set_xlabel("n = number of nested choices", fontsize=13)
ax.set_ylabel("Time (s) - Logarithmic scale", fontsize=13)
# ax.set_title("Average running time of sat instance \n w.r.t. number of nested choices")

# Log scale
ax.set_yscale('log')

# # Add mean value annotations next to each box
# for i, box in enumerate(bp['boxes']):
#     # Get the mean value for the current box
#     mean = np.mean(elapsed_times[i])
#     # Add the mean value annotation
#     ax.text(i+1, mean*1.1, f"{mean:.3f}", ha='center', va='bottom')

plt.grid(True)

plt.savefig(f"{results_dir}/nested-choices.pdf")
