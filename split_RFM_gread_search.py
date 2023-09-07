"" this is generated by chat GTP3.5, to split the code named RFM_without_validation, because this is to complicated"""

import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import (
    confusion_matrix,
    accuracy_score,
    precision_score,
    recall_score,
    f1_score,
    roc_auc_score,
    roc_curve,
    classification_report,
)
import matplotlib.pyplot as plt

def perform_grid_search(X_train, y_train):
    '''
    Description: Perform GridSearchCV for hyperparameter tuning.
    Input:   X_train - Features of the training data.
             y_train - Labels of the training data.
    Output: best_model - The tuned final model.
            best_params - A dictionary of the best parameter values.
    '''
    # Define the parameter grid
    n_estimators = [200, 1100, 2000]
    max_depth = [10, 32, None]
    min_samples_split = [2, 5, 10]
    
    param_grid = {
        'n_estimators': n_estimators,
        'max_depth': max_depth,
        'min_samples_split': min_samples_split
    }

    grid_search = GridSearchCV(
        RandomForestClassifier(random_state=42),
        param_grid,
        cv=3,
        scoring='accuracy'
    )
    grid_search.fit(X_train, y_train)

    # Access the best parameters
    best_params = grid_search.best_params_
    print("Best Parameters:", best_params)

    # Get the best model
    best_model = grid_search.best_estimator_
    return best_model, best_params

def evaluate_model(model, X_val, X_test, y_val, y_test):
    '''
    Description: Evaluate the model on the validation and test data.
    Input:   model - The trained model.
             X_val - Features of the validation data.
             X_test - Features of the test data.
             y_val - Labels of the validation data.
             y_test - Labels of the test data.
    Output: val_accuracy - Accuracy score on the validation data.
            test_accuracy - Accuracy score on the test data.
            precision - Precision score on the test data.
            recall - Recall score on the test data.
            f1_score_result - F1-Score on the test data.
            roc_auc - AUC score on the test data.
    '''
    # Evaluate the best model on the validation data
    val_accuracy = model.score(X_val, y_val)
    print("Validation Accuracy:", val_accuracy)

    # Step 1: Make Predictions on Test Data
    y_pred = model.predict(X_test)

    # Step 2: Evaluate Model Performance on Test Data
    test_accuracy = accuracy_score(y_test, y_pred)
    print("Test Accuracy:", test_accuracy)

    # Calculate precision: TP/(TP+FP)
    precision = precision_score(y_test, y_pred)

    # Calculate recall: TP/(TP+FN)
    recall = recall_score(y_test, y_pred)

    # Calculate F1-score: 2 * (precision * recall) / (precision + recall)
    f1_score_result = f1_score(y_test, y_pred)

    # Get predicted probabilities
    probas = model.predict_proba(X_test)

    # Generate ROC curve and store FP, TP, and thresholds
    fpr, tpr, thresholds = roc_curve(y_test, probas[:, 1])

    # Calculate AUC score
    roc_auc = roc_auc_score(y_test, probas[:, 1])

    return val_accuracy, test_accuracy, precision, recall, f1_score_result, roc_auc

def save_plot_and_report(roc_curve_data, year, month):
    '''
    Description: Save ROC curve plot and print classification report.
    Input:   roc_curve_data - Data for ROC curve (fpr, tpr, thresholds).
             year - Year information for file naming.
             month - Month information for file naming.
    '''
    fpr, tpr, thresholds = roc_curve_data

    # Create and customize the ROC plot
    plt.style.use('fivethirtyeight')
    fig, ax = plt.subplots(figsize=(4.8, 5))
    ax.step(fpr, tpr, 'gray')
    ax.fill_between(fpr, tpr, 0, color='skyblue', alpha=0.8)
    ax.set_xlabel('False Positive Rate')
    ax.set_ylabel('True Positive Rate')
    title_name = f"AUC-ROC Curve_{year}_{month}"
    ax.set_title(title_name)
    ax.set_facecolor('xkcd:white')

    # Save the ROC plot as a PNG file
    plot_name = f"/AUC_plot_{year}_{month}.png"
    plt.savefig(plot_name, dpi=300, bbox_inches='tight', transparent=True)
    print('Plot saved successfully.')

    # Print the classification report (optional)
    print("Classification Report:\n", class_report)
    
    # Display the ROC plot
    plt.show()