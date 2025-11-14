import matplotlib.pyplot as plt

def generate_line_graphs_with_end_marker(benchmark_results, output_path,
                                         dice_backend=True, roulette_backend=True):
    """
    Generates the scaling plot with an extra marker at the last valid Slice+Roulette point.

    Args:
        benchmark_results: list of tuples (prog_size, ((slice_dice, total_dice),
                                                       (slice_roulette, total_roulette)), sppl_time)
        output_path: output .png file
        dice_backend: bool
        roulette_backend: bool
    """
    plt.figure(figsize=(10, 6))
    
    # Font and style setup (same as your original)
    plt.rcParams.update({
        'axes.labelsize': 18,
        'axes.titlesize': 18,
        'xtick.labelsize': 14,
        'ytick.labelsize': 14,
        'legend.fontsize': 14,
        'lines.markersize': 8
    })

    all_prog_sizes = [b[0] for b in benchmark_results]

    # --- SPPL ---
    sppl_prog_sizes = []
    sppl_times = []
    for b in benchmark_results:
        if b[2] is not None:
            sppl_prog_sizes.append(b[0])
            sppl_times.append(b[2])

    if sppl_times:
        plt.plot(sppl_prog_sizes, sppl_times,
                 label="SPPL", color="red", marker='o',
                 markevery=[-1], linewidth=3.0)

    # --- Slice (compiler time only) ---
    if dice_backend:
        slice_times = [b[1][0][0] for b in benchmark_results]
        plt.plot(all_prog_sizes, slice_times,
                 label="Slice", color="peru", marker='s',
                 markevery=[-1], linewidth=3.0)

    # --- Slice+Dice and Slice+Roulette ---
    if dice_backend and roulette_backend:
        dice_times = [b[1][0][1] for b in benchmark_results]
        roulette_times = [b[1][1][1] for b in benchmark_results]

        # Slice+Dice line
        plt.plot(all_prog_sizes, dice_times,
                 label="Slice+Dice", color="darkblue", marker='^',
                 markevery=[-1], linewidth=3.0)

        # Slice+Roulette line
        plt.plot(all_prog_sizes, roulette_times,
                 label="Slice+Roulette", color="forestgreen",
                 marker='D', markevery=[-1], linewidth=3.0)

        # Add a distinct larger marker at the last *valid* (non-None) Slice+Roulette point
        valid_points = [(x, y) for x, y in zip(all_prog_sizes, roulette_times) if y is not None]
        if valid_points:
            last_x, last_y = valid_points[-1]
            plt.plot(last_x, last_y, marker='D', markersize=10, color="forestgreen")

    elif dice_backend:
        dice_times = [b[1][0][1] for b in benchmark_results]
        plt.plot(all_prog_sizes, dice_times,
                 label="Slice+Dice", color="darkblue", marker='^',
                 markevery=[-1], linewidth=3.0)

    elif roulette_backend:
        roulette_times = [b[1][1][1] for b in benchmark_results]
        plt.plot(all_prog_sizes, roulette_times,
                 label="Slice+Roulette", color="forestgreen",
                 marker='D', markevery=[-1], linewidth=3.0)

        valid_points = [(x, y) for x, y in zip(all_prog_sizes, roulette_times) if y is not None]
        if valid_points:
            last_x, last_y = valid_points[-1]
            plt.plot(last_x, last_y, marker='D', color="forestgreen")

    # --- Labels, grid, legend ---
    plt.xlabel("Program Size")
    plt.ylabel("Execution Time (seconds)")

    plt.gca().spines['top'].set_visible(False)
    plt.gca().spines['right'].set_visible(False)
    plt.grid(True, alpha=0.3)
    plt.legend()

    plt.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.show()
    print(f"Saved scaling plot to {output_path}", flush=True)


benchmark_results = [
    # (program_size, ((slice_dice, total_dice), (slice_roulette, total_roulette)), sppl)
    (0,  ((0.0070, 0.1345), (0.0054, 0.0995)), 0.5530),
    (10, ((0.0090, 0.1966), (0.0082, 0.2826)), 1.7486),
    (15, ((0.0158, 0.3176), (0.0167, 0.8607)), 6.5694),
    (20, ((0.0197, 0.5986), (0.0244, 1.6242)), 14.5291),
    (25, ((0.0451, 0.9108), (0.0457, 7.3596)), 41.3206),
    (30, ((0.0592, 1.0333), (0.0526, 20.2882)), 99.1215),
    (35, ((0.0839, 1.3278), (None, None)), None),
    (40, ((0.0857, 1.6548), (None, None)), None),
    (45, ((0.1019, 1.6477), (None, None)), None),
    (50, ((0.1085, 2.1299), (None, None)), None),
]

generate_line_graphs_with_end_marker(benchmark_results, "test.png")
