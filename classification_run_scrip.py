import sys

# Create the output file and save all printed output
class OutputLogger:
    def __init__(self, filename):
        self.file = open(filename, 'w')
        self.stdout = sys.stdout

    def write(self, message):
        # Write the message to both the file and the standard output
        self.file.write(message)
        self.stdout.write(message)

    def flush(self):
        pass  # No need to flush the custom logger

# Specify the path and file name for the log file
log_path = "/path/to/logs"
log_file_name = "output_log"

# Create an instance of the OutputLogger class with the specified log file name
logger = OutputLogger(f'{log_path}/{log_file_name}.txt')

# Redirect the standard output to the custom logger
sys.stdout = logger

# ====================================== running script!! ====================================== #
import pandas as pd
import numpy as np
from collections import Counter

df_merged = pd.read_csv('data/df.csv')
df=df_merged.copy()
# Convert the 'date' column to Timestamp type
df['date']=pd.to_datetime(df['date'], format='mixed')

# Create an empty DataFrame for evaluation metrics
eval_df = pd.DataFrame(columns=['year', 'month', 'train_samples', 'validation_samples', 'test_samples', 'train_features_used', 'hospitalized_counts(0)', 'death_counts(1)', 'accuracy_non_val', 'best_param', 'confusion_matrix_TN', 'confusion_matrix_FP', 'confusion_matrix_FN', 'confusion_matrix_TP', 'accuracy', 'precision', 'recall', 'F1-Score', 'AUC_score'])
# Create an empty DataFrame to store feature importance values
feature_impt = pd.DataFrame([])





# At the end of your code execution, close the output file
logger.file.close()
