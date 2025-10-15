import matplotlib.pyplot as plt
import numpy as np

plt.rcParams["figure.figsize"] = [7.50, 3.50]
plt.rcParams["figure.autolayout"] = True

with open("plot_output") as f:

    plt.style.use('dark_background')

    fig, axs = plt.subplots(1, 1)

    num_samples = 441

    lines = f.readlines()[:num_samples]
    y = [float(i.strip()) for i in lines]
    ticks = range(len(lines))
    axs.plot(ticks, y)
    axs.set_xticks(np.arange(0, num_samples, num_samples//10), np.arange(0, 11, 1))
    axs.set_yticks([-1, -0.5, 0, 0.5, 1])

    plt.ylabel("Amplitude")
    plt.xlabel("Time (ms)")

    plt.show()
