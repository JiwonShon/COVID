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

# Create an empty DataFrame to store evaluation metrics
eval_df = pd.DataFrame(columns=[
    'year', 'month', 'train_samples', 'validation_samples', 'test_samples', 'train_features_used',
    'hospitalized_counts(0)', 'death_counts(1)', 'accuracy_non_val', 'best_param',
    'confusion_matrix_TN', 'confusion_matrix_FP', 'confusion_matrix_FN', 'confusion_matrix_TP',
    'accuracy', 'precision', 'recall', 'F1-Score', 'AUC_score'
])

# Create an empty DataFrame to store feature importance values
feature_impt = pd.DataFrame([])

# Loop through years and months
for year in range(2020, 2023):
    if year == 2020:
        for month in range(3, 13):
            # Extract data for the specified year and month
            previous_data, following_data = extract_data_ver1(df, year, month)
            
            # Count the labels in the training data
            df_merged = pd.concat([previous_data, following_data], axis=0)
            value_counts = Counter(df_merged['survival'])
            result = dict(value_counts)
            hos_count = result[0]
            dec_count = result[1]
            
            # Print or perform operations on the extracted data
            print("*** ===== Start!", year, month, previous_data.shape, following_data.shape, end='\n')

            # Transform data frame into pivot style
            train_pivot, test_pivot, feature_train_count, feature_test_count = pivot_converter_ver1(previous_data, following_data) 
            train_shape = train_pivot.shape
            test_shape = test_pivot.shape

            # Train a Random Forest Classification model without validation
            accuracy_non_val = RFM_without_validation(train_pivot, test_pivot, year, month)

            # Train a Random Forest Classification model with GridSearchCV
            X_val, X_test, final_model, best_params, validation_accuracy, importance_df, confusion_matrix_list, accuracy, precision, recall, f1_score_result, roc_auc = RFM_grid_search(train_pivot, test_pivot, year, month)

            # Create a new row for the evaluation DataFrame
            new_row_eval = pd.DataFrame([{
                'year': year, 'month': month, 'train_samples': train_shape[0],
                'validation_samples': X_val.shape[0], 'test_samples': X_test.shape[0],
                'train_features_used': feature_train_count, 'hospitalized_counts(0)': hos_count,
                'death_counts(1)': dec_count, 'accuracy_non_val': accuracy_non_val,
                'best_param': best_params, 'confusion_matrix_TN': confusion_matrix_list[0],
                'confusion_matrix_FP': confusion_matrix_list[1],
                'confusion_matrix_FN': confusion_matrix_list[2],
                'confusion_matrix_TP': confusion_matrix_list[3], 'accuracy': accuracy,
                'precision': precision, 'recall': recall, 'F1-Score': f1_score_result,
                'AUC_score': roc_auc
            }])

            # Concatenate the new row to the evaluation DataFrame
            eval_df = pd.concat([eval_df, new_row_eval], ignore_index=True)

            # Store feature importance data
            if month == 3:
                feature_impt = importance_df.copy()
            else:
                feature_impt = pd.merge(feature_impt, importance_df, on='Feature', how='outer')

    elif year == 2021:
        for month in range(1, 13):
            # Extract data for the specified year and month
            previous_data, following_data = extract_data_ver1(df, year, month)
            
            # Count the labels in the training data
            df_merged = pd.concat([previous_data, following_data], axis=0)
            value_counts = Counter(df_merged['survival'])
            result = dict(value_counts)
            hos_count = result[0]
            dec_count = result[1]
            
            # Print or perform operations on the extracted data
            print("*** ===== Start!", year, month, previous_data.shape, following_data.shape, end='\n')

            # Transform data frame into pivot style
            train_pivot, test_pivot, feature_train_count, feature_test_count = pivot_converter_ver1(previous_data, following_data) 
            train_shape = train_pivot.shape
            test_shape = test_pivot.shape

            # Train a Random Forest Classification model without validation
            accuracy_non_val = RFM_without_validation(train_pivot, test_pivot, year, month)

            # Train a Random Forest Classification model with GridSearchCV
            X_val, X_test, final_model, best_params, validation_accuracy, importance_df, confusion_matrix_list, accuracy, precision, recall, f1_score_result, roc_auc = RFM_grid_search(train_pivot, test_pivot, year, month)

            # Create a new row for the evaluation DataFrame
            new_row_eval = pd.DataFrame([{
                'year': year, 'month': month, 'train_samples': train_shape[0],
                'validation_samples': X_val.shape[0], 'test_samples': X_test.shape[0],
                'train_features_used': feature_train_count, 'hospitalized_counts(0)': hos_count,
                'death_counts(1)': dec_count, 'accuracy_non_val': accuracy_non_val,
                'best_param': best_params, 'confusion_matrix_TN': confusion_matrix_list[0],
                'confusion_matrix_FP': confusion_matrix_list[1],
                'confusion_matrix_FN': confusion_matrix_list[2],
                'confusion_matrix_TP': confusion_matrix_list[3], 'accuracy': accuracy,
                'precision': precision, 'recall': recall, 'F1-Score': f1_score_result,
                'AUC_score': roc_auc
            }])

            # Concatenate the new row to the evaluation DataFrame
            eval_df = pd.concat([eval_df, new_row_eval], ignore_index=True)
            
            # Store feature importance data
            feature_impt = pd.merge(feature_impt, importance_df, on='Feature', how='outer')

    elif year == 2022:
        for month in range(1, 10):
            # Extract data for the specified year and month
            previous_data, following_data = extract_data_ver1(df, year, month)
            
            # Count the labels in the training data
            df_merged = pd.concat([previous_data, following_data], axis=0)
            value_counts = Counter(df_merged['survival'])
            result = dict(value_counts)
            hos_count = result[0]
            dec_count = result[1]
            
            # Print or perform operations on the extracted data
            print("*** ===== Start!", year, month, previous_data.shape, following_data.shape, end='\n')

            # Transform data frame into pivot style
            train_pivot, test_pivot, feature_train_count, feature_test_count = pivot_converter_ver1(previous_data, following_data) 
            train_shape = train_pivot.shape
            test_shape = test_pivot.shape

            # Train a Random Forest Classification model without validation
            accuracy_non_val = RFM_without_validation(train_pivot, test_pivot, year, month)

            # Train a Random Forest Classification model with GridSearchCV
            X_val, X_test, final_model, best_params, validation_accuracy, importance_df, confusion_matrix_list, accuracy, precision, recall, f1_score_result, roc_auc = RFM_grid_search(train_pivot, test_pivot, year, month)

            # Create a new row for the evaluation DataFrame
            new_row_eval = pd.DataFrame([{
                'year': year, 'month': month, 'train_samples': train_shape[0],
                'validation_samples': X_val.shape[0], 'test_samples': X_test.shape[0],
                'train_features_used': feature_train_count, 'hospitalized_counts(0)': hos_count,
                'death_counts(1)': dec_count, 'accuracy_non_val': accuracy_non_val,
                'best_param': best_params, 'confusion_matrix_TN': confusion_matrix_list[0],
                'confusion_matrix_FP': confusion_matrix_list[1],
                'confusion_matrix_FN': confusion_matrix_list[2],
                'confusion_matrix_TP': confusion_matrix_list[3], 'accuracy': accuracy,
                'precision': precision, 'recall': recall, 'F1-Score': f1_score_result,
                'AUC_score': roc_auc
            }])

            # Concatenate the new row to the evaluation DataFrame
            eval_df = pd.concat([eval_df, new_row_eval], ignore_index=True)
            
            # Store feature importance data
            feature_impt = pd.merge(feature_impt, importance_df, on='Feature', how='outer')

eval_df.loc[:,'motality']=eval_df['dec_counts']/(eval_df['hos_counts']+eval_df['dec_counts'])
eval_df.to_csv('/FM_window_evaluation_date.csv')
feature_impt.to_csv('/RFM_feature_impt_date.csv')


# At the end of your code execution, close the output file
logger.file.close()
