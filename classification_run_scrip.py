import sys
import pandas as pd
import numpy as np
from collections import Counter
from classification_functions import extract_data_ver1, pivot_converter_ver1, RFM_without_validation, RFM_grid_search, year_month_column


# Creat the output file and save all printed command 
class OutputLogger:
    def __init__(self, filename):
        self.file = open(filename, 'w')
        self.stdout = sys.stdout

    def write(self, message):
        self.file.write(message)
        self.stdout.write(message)

    def flush(self):
        pass
        # self.file.flush()
        # self.stdout.flush()

# Create an instance of the OutputLogger class with the specified log file name
logger = OutputLogger('/mnt/NAS4/home/jiwon/classification_model/result/20230915_RFM/20230915_log_normal.txt')

# Redirect the standard output to the custom logger
sys.stdout = logger

# ====================================== running script!! ====================================== #

df_merged = pd.read_csv('/mnt/NAS4/home/jiwon/classification_model/jupyter/data/df_all_230829.csv')
df=df_merged.copy()
df['survival'] = np.where(df['status'] == 'hospitalized', 0, 1)  # deceased 가 event가 일어난거니까 1임

print(df['status'].value_counts())
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
            train_pivot, test_pivot, feature_train_count, feature_test_count = pivot_converter_ver1(previous_data, following_data, df) 
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

    elif year in [2021,2022]:
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
            train_pivot, test_pivot, feature_train_count, feature_test_count = pivot_converter_ver1(previous_data, following_data,df) 
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

    elif year == 2023:
        for month in range(1, 8):
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
            train_pivot, test_pivot, feature_train_count, feature_test_count = pivot_converter_ver1(previous_data, following_data,df) 
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

eval_df = year_month_column(eval_df)
eval_df.to_csv('/mnt/NAS4/home/jiwon/classification_model/result/20230915_RFM/RFM_evaluation_normal.csv')
feature_impt.to_csv('/mnt/NAS4/home/jiwon/classification_model/result/20230915_RFM/RFM_feature_impt_normal.csv')


# At the end of your code execution, close the output file
logger.file.close()
