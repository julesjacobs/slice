import subprocess
import matplotlib.pyplot as plt
import re

# Define time steps and file names
time_steps = [i for i in range(1, 21)]
hmm_file = "hmm.wppl"
hmm_discretized_file = "hmm-discrete.wppl"

# Initialize lists to store scores
hmm_scores = []
hmm_discretized_scores = []

# Function to run webppl command and extract score
def run_webppl(file, time_step):
    try:
        # Execute the webppl command
        result = subprocess.run(
            ["webppl", file, "--", f"--timeSteps", str(time_step)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        # Check for errors
        if result.returncode != 0:
            print(f"Error running {file} with timeSteps={time_step}")
            return None
        
        # Extract numeric score from output
        match = re.search(r"\b\d+(\.\d+)?\b", result.stdout)
        if match:
            return float(match.group())
        else:
            print(f"No numeric output found for {file} with timeSteps={time_step}")
            print(result.stdout)
            return None
    except Exception as e:
        print(f"Exception while running {file}: {e}")
        return None

# Run experiments for all time steps
for t in time_steps:
    print(f"Running with timeSteps={t}...")
    hmm_score = run_webppl(hmm_file, t)
    hmm_discretized_score = run_webppl(hmm_discretized_file, t)
    
    if hmm_score is not None and hmm_discretized_score is not None:
        hmm_scores.append(hmm_score)
        hmm_discretized_scores.append(hmm_discretized_score)
    else:
        print(f"Skipping timeSteps={t} due to errors.")

# Plot the results
if hmm_scores and hmm_discretized_scores:
    plt.figure(figsize=(10, 6))
    plt.plot(
        time_steps[:len(hmm_scores)], hmm_scores, 
        label="HMM", color="red", marker="^", linestyle="--"
    )  # Triangle up marker for HMM
    plt.plot(
        time_steps[:len(hmm_discretized_scores)], hmm_discretized_scores, 
        label="HMM Discretized", color="blue", marker="o", linestyle="-"
    )  # Circle marker for discretized HMM
    plt.xlabel("Time Steps")
    plt.ylabel("Average Scores")
    plt.title("HMM vs HMM Discretized Average Scores")
    plt.legend()
    plt.grid(True)
    save_path = "hmm_scores_plot.png"  # Save directly in current directory
    plt.savefig(save_path)
    plt.show()
    print(f"Plot saved to {save_path}")
else:
    print("No valid scores to plot.")
