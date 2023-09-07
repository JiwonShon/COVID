## == Functions
def extract_data_ver1(df, year, month):
    '''
    Description: This function is designed to split a data frame into two different periods for the application of a classification model.
    Input:   A data frame that includes a 'Date' column for the classification model.
    Output:  previous_three_months: The training data set, consisting of data from the previous three months.
             following_three_months: The test data set, containing data from following three months.
    '''
    import pandas as pd
    # set the start- and end-month
    start_date = pd.to_datetime(f"{year}-{month:02d}", format="%Y-%m")
    end_date = start_date + pd.DateOffset(months=1)
    print("start_date: ", start_date, '\n', "end_date", end_date)

    # Filter the data for the previous three months and the following three months
    previous_three_months = df[(df['date'] >= (start_date - pd.DateOffset(months=3))) & (df['date'] < start_date)]
    print(previous_three_months['date'].value_counts())
    following_three_months = df[(df['date'] >= start_date) & (df['date'] < (start_date + pd.DateOffset(months=3)))]
    print(following_three_months['date'].value_counts())

    print('================ End of Extract data sets (2sets) for pivot table =================')
    return previous_three_months, following_three_months

def extract_data_ver2(df, year, month):
    '''
    Description: This function is designed to split a data frame into three different periods for the application of a classification model.
    Input:   A data frame that includes a 'Date' column for the classification model.
    Output:  previous_three_months: The training data set, consisting of data from the previous three months.
             following_first_month: The validation data set, comprising data from the first following month.
             after_two_months: The test data set, containing data from two months ahead.
    '''
    import pandas as pd
    # set the start- and end-month
    start_date = pd.to_datetime(f"{year}-{month:02d}", format="%Y-%m")
    end_date = start_date + pd.DateOffset(months=1)
    print("start_date: ", start_date, '\n', "end_date", end_date)

    # Filter the data for the previous three months and the following three months
        #1) the previous three months: for training set
    previous_three_months = df[(df['date'] >= (start_date - pd.DateOffset(months=3))) & (df['date'] < start_date)]
    print(previous_three_months['date'].value_counts())
        #2) following_first_month: for validation set
    following_first_month = df[(df['date'] >= start_date) & (df['date'] < (start_date + pd.DateOffset(months=1)))]
    print(following_first_month['date'].value_counts())
        #3) after_two_month: for test set
    after_two_month = df[(df['date'] >= (start_date + pd.DateOffset(months=1))) & (df['date'] < (start_date + pd.DateOffset(months=3)))]
    print(after_two_month['date'].value_counts())
    print('the size of data sets', 'training:', previous_three_months.shape,'validation:', following_first_month.shape,'test:', after_two_month.shape)

    print('================ End of Extract data sets (3sets) for pivot table =================')
    return previous_three_months, following_first_month, after_two_month

def pivot_converter_ver1(previous_three_months, following_three_months, df):
    '''
    Description: This function is designed to pivot a data frame for immediate use in a model.
                you can apply this train_pivot to training model, and test_pivot to separate into validation and test.
    Input:   The outputs from three data frames are obtained from the extract_data function.
    Output:  train_pivot: Feature-values with labels (survival) for training the model.
             test_pivot: Feature-values with labels (survival) for testing the model.
             feature_train_count: Count of features, including train_pivot.
             feature_test_count: Count of features, including test_pivot.
    '''
    import pandas as pd
    # Assign a 'purpose' column to indicate the dataset purpose
    previous_three_months.loc[:,'purpose'] = 'training'
    following_three_months.loc[:,'purpose'] = 'test'
    
    # Concatenate three data sets into a single data frame vertically.
    df_merged = pd.concat([previous_three_months, following_three_months], axis=0)
    
    # Check for duplicate 'sampleID' entries and retain the first occurrence.
    df_merged_res = df_merged.drop_duplicates(subset='sampleID', keep='first')
    print("Merged data: All together (after removing duplicates):", df_merged_res.shape)

    
    # Get unique 'aachange' features for the training data
    features_train = df_merged_res.loc[df_merged_res['purpose']=='training', 'aachange'].unique().tolist()
    feature_train_count =  len(features_train)
    print('Training Features:', feature_train_count)
 
    # Get unique 'aachange' features for the test data
    features_test = df_merged_res.loc[df_merged_res['purpose']=='test', 'aachange'].unique().tolist()
    feature_test_count =  len(features_test)
    print('Test Features:',  feature_test_count)
    

    # Separate the data frame into train, validation, and test sets based on the 'purpose' column and make list 'sampleID'
    sampleID_train = df_merged_res.loc[df_merged_res['purpose']=='training', 'sampleID'].unique().tolist()
    sampleID_test = df_merged_res.loc[df_merged_res['purpose']=='test', 'sampleID'].unique().tolist()
    print("Total number of samples:", "Train:", len(sampleID_train), '\t', "Test:", len(sampleID_test))

    
    # Create a new 'feature1' column by combining 'pos' and 'aachange' columns.
    df_merged_res.loc[:,'feature1'] = df_merged_res['pos'].astype(str) + '_' + df_merged_res['aachange']
    
    
    # Transform the data frame into a pivot-style format
    pivot_df = df_merged_res.pivot(index='sampleID', columns='aachange', values='impact_transform').fillna(0)
    print("Pivot shape:", pivot_df.shape)
    print("First two rows:", pivot_df.head(n=2))
    # Convert pivot_df to a regular DataFrame
    df1 = pd.DataFrame(pivot_df.to_records())
    
    # Add status information to the pivot data frame
    status_df = df[['sampleID','survival']].drop_duplicates()
    df1 = pd.merge(df1, status_df, on='sampleID', how='left')
    df1 = df1.set_index('sampleID')
    print("Pivot output after adding status information:", df1.head(n=2))

    # Select the columns corresponding to training features and 'survival'
    strings = [str(num) for num in features_train]
    strings.append('survival')
    selected_pivot_df = df1[strings]
    print('check only including training features:', df_merged_res.shape, selected_pivot_df.shape)

    # Separate the pivot data into training, validation, and test sets
    train_pivot = selected_pivot_df[selected_pivot_df.index.isin(sampleID_train)]
    test_pivot = selected_pivot_df[selected_pivot_df.index.isin(sampleID_test)]
    print("Training pivot shape:", train_pivot.shape, "Test pivot shape:", test_pivot.shape)
    
    print('================ End of Pivot data (2sets) =================') 
    return train_pivot, test_pivot, feature_train_count, feature_test_count

def pivot_converter_ver2(previous_three_months, following_first_month, after_two_month):
    '''
    Description: This function is designed to pivot a data frame for immediate use in a model.
    Input:   The outputs from three data frames obtained from the extract_data function.
    Output:  train_pivot: Feature-values with labels (survival) for training the model.
             val_pivot: Feature-values with labels (survival) for validating the model
             test_pivot: Feature-values with labels (survival) for testing the model.
             feature_train_count: Count of features, including train_pivot.
             features_val_count: Count of features, including val_pivot.
             feature_test_count: Count of features, including test_pivot.
    '''
    import pandas as pd
    # Assign a 'purpose' column to indicate the dataset purpose
    previous_three_months.loc[:,'purpose'] = 'training'
    following_first_month.loc[:,'purpose'] = 'validation'
    after_two_month.loc[:,'purpose'] = 'test'
    
    # Concatenate three data sets into a single data frame vertically.
    df_merged = pd.concat([previous_three_months, following_first_month, after_two_month], axis=0)
    
    # Check for duplicate 'sampleID' entries and retain the first occurrence.
    df_merged_res = df_merged.drop_duplicates(subset='sampleID', keep='first')
    print("Merged data: All together (after removing duplicates):", df_merged_res.shape)

    
    # Get unique 'aachange' features for the training data
    features_train = df_merged_res.loc[df_merged_res['purpose']=='training', 'aachange'].unique().tolist()
    feature_train_count =  len(features_train)
    print('Training Features:', feature_train_count)
    # Get unique 'aachange' features for the validation data
    features_val = df_merged_res.loc[df_merged_res['purpose']=='validation', 'aachange'].unique().tolist()
    features_val_count =  len(features_val)
    print('Validation Features:',  features_val_count)
    # Get unique 'aachange' features for the test data
    features_test = df_merged_res.loc[df_merged_res['purpose']=='test', 'aachange'].unique().tolist()
    feature_test_count =  len(features_test)
    print('Test Features:',  feature_test_count)
    

    # Separate the data frame into train, validation, and test sets based on the 'purpose' column and make list 'sampleID'
    sampleID_train = df_merged_res.loc[df_merged_res['purpose']=='training', 'sampleID'].unique().tolist()
    sampleID_val = df_merged_res.loc[df_merged_res['purpose']=='validation', 'sampleID'].unique().tolist()
    sampleID_test = df_merged_res.loc[df_merged_res['purpose']=='test', 'sampleID'].unique().tolist()
    print("Total number of samples:", "Train:", len(sampleID_train), '\t', "Validation:", len(sampleID_val), '\t', "Test:", len(sampleID_test))

    
    # Create a new 'feature1' column by combining 'pos' and 'aachange' columns.
    df_merged_res.loc[:,'feature1'] = df_merged_res['pos'].astype(str) + '_' + df_merged_res['aachange']
    
    
    # Transform the data frame into a pivot-style format
    pivot_df = df_merged_res.pivot(index='sampleID', columns='aachange', values='impact_transform').fillna(0)
    print("Pivot shape:", pivot_df.shape)
    print("First two rows:", pivot_df.head(n=2))
    # Convert pivot_df to a regular DataFrame
    df1 = pd.DataFrame(pivot_df.to_records())
    
    # Add status information to the pivot data frame
    status_df = df[['sampleID','survival']].drop_duplicates()
    df1 = pd.merge(df1, status_df, on='sampleID', how='left')
    df1 = df1.set_index('sampleID')
    print("Pivot output after adding status information:", df1.head(n=2))

    # Select the columns corresponding to training features and 'survival'
    strings = [str(num) for num in features_train]
    strings.append('survival')
    selected_pivot_df = df1[strings]
    print('check only including training features:', df_merged_res.shape, selected_pivot_df.shape)

    # Separate the pivot data into training, validation, and test sets
    train_pivot = selected_pivot_df[selected_pivot_df.index.isin(sampleID_train)]
    val_pivot = selected_pivot_df[selected_pivot_df.index.isin(sampleID_val)]
    test_pivot = selected_pivot_df[selected_pivot_df.index.isin(sampleID_test)]
    print("Training pivot shape:", train_pivot.shape, "Validation pivot shape:", val_pivot.shape, "Test pivot shape:", test_pivot.shape)

    print('================ End of Pivot data (3sets) =================')  
    return train_pivot, val_pivot, test_pivot, feature_train_count, features_val_count, feature_test_count

def pivot_converter_ver3(previous_three_months, following_three_months, df):
    '''
    Description: This function is designed to pivot a data frame for immediate use in a model.
                you can apply this train_pivot to training model, and test_pivot to separate into validation and test.
                << + ver 3 updated: include patient meta data such as age, sex, region>>
    Input:   The outputs from three data frames obtained from the extract_data function.
    Output:  train_pivot: Feature-values with labels (survival) for training the model.
             test_pivot: Feature-values with labels (survival) for testing the model.
             feature_train_count: Count of features, including train_pivot.
             feature_test_count: Count of features, including test_pivot.
    '''
    import pandas as pd
    # Assign a 'purpose' column to indicate the dataset purpose
    previous_three_months.loc[:,'purpose'] = 'train'
    following_three_months.loc[:,'purpose'] = 'test'

    # Concatenate three data sets into a single data frame vertically.
    df_merged = pd.concat([previous_three_months, following_three_months], axis=0)
    
    # Check for duplicate 'sampleID' entries and retain the first occurrence.
    df_merged_res = df_merged.drop_duplicates(subset='sampleID', keep='first')
    print("Merged data: All together (after removing duplicates):", df_merged_res.shape)

    # Create a new 'feature1' column by combining 'pos' and 'aachange' columns.
    df_merged_res.loc[:,'feature1'] = df_merged_res['pos'].astype(str) + '_' + df_merged_res['aachange']
    
    # Get features from train data 
    features_train = df_merged_res.loc[df_merged_res['purpose']=='train', 'feature1'].unique().tolist()+['age_range', 'sex', 'region']
    feature_train_count =  len(features_train)
    print('Training Features:', feature_train_count)
    # Get features from test data
    features_test = df_merged_res.loc[df_merged_res['purpose']=='test', 'feature1'].unique().tolist()+['age_range', 'sex', 'region']
    feature_test_count =  len(features_test)
    print('Test Features:',  feature_test_count)

    # Separate the data frame into train, validation, and test sets based on the 'purpose' column and make list 'sampleID'
    sampleID_train = df_merged_res.loc[df_merged_res['purpose']=='train', 'sampleID'].unique().tolist()
    sampleID_test = df_merged_res.loc[df_merged_res['purpose']=='test', 'sampleID'].unique().tolist()
    print("Total number of samples:", "Train:", len(sampleID_train), '\t', "Test:", len(sampleID_test))

    # Transform the data frame into a pivot-style format
    pivot_df = df_merged_res.pivot(index='sampleID', columns='feature1', values='impact_transform').fillna(0)
    print("Pivot shape: ", pivot_df.shape)
    print("First two rows: ", pivot_df.head(n=2))
    # Transform the data frame into a pivot-style format
    df1 = pd.DataFrame(pivot_df.to_records())
    
    # Add status information in pivot data frame
    status_df = df[['sampleID', 'age_range', 'sex', 'region', 'survival']].drop_duplicates()
    df1 = pd.merge(df1, status_df, on='sampleID', how='left')
    df1 = df1.set_index('sampleID')
    print("Pivot output after adding patients metadata & status information:", df1.head(n=2))

    # Select the columns corresponding to training features and 'survival'
    strings = [str(num) for num in features_train]
    strings.append('survival')
    selected_pivot_df = df1[strings]
    print('check only including training features:', df_merged_res.shape, selected_pivot_df.shape)

    # Separate the pivot data into training, validation, and test sets
    train_pivot=selected_pivot_df[selected_pivot_df.index.isin(sampleID_train)]
    test_pivot=selected_pivot_df[selected_pivot_df.index.isin(sampleID_test)]
    print("Training pivot shape:", train_pivot.shape, "Test pivot shape:", test_pivot.shape)

    return train_data, test_data, train_pivot, test_pivot, feature_train_count, feature_test_count


def RFM_without_validation(df1, df2, year, month):
    '''
    Description: This function implements a Random Forest Classification model without a validation test. It trains on 'df1' and tests on 'df2' and 'df3'.
    Input:   Three pivot data frames are obtained from the 'pivot_converter_ver1' function.
    Output:  accuracy_test_n: The accuracy score of the model on the test data.
    '''
    from sklearn.ensemble import RandomForestClassifier
    from sklearn.metrics import accuracy_score
    import pandas as pd

    # Step 1: Prepare the training data
    X_train_n = df1.drop(['survival'], axis=1)
    y_train_n = df1['survival']
    print('X_train shape:', X_train_n.shape, '\t', 'y_train shape:', y_train_n.shape)

    # Prepare the test data
    X_test_n = df2.drop(['survival'], axis=1)
    y_test_n = df2['survival']
    print('X_test shape:', X_test_n.shape, '\t', 'y_test shape:', y_test_n.shape)

    # Step 2: Train a Random Forest Classifier
    model = RandomForestClassifier(n_estimators=100, random_state=42)
    model.fit(X_train_n, y_train_n)

    # Step 3: Test the model on the validation data set
    y_pred_n = model.predict(X_test_n)
    accuracy_test_n = accuracy_score(y_test_n, y_pred_n)
    print(accuracy_test_n)
    
    return accuracy_test_n


def RFM_grid_search(df1, df2, year, month):
    '''
    Description: Train a Random Forest Classification model with GridSearchCV for hyperparameter tuning. It trains on 'df1', validates on 'df2', and tests on 'df3'.
    Input:   Three pivot data frames obtained from the 'pivot_converter_ver1' function. 
                However, merged df2 with df3 and then split the data set into validation(0.3) and test data set(0.7)
    Output: final_model: The tuned final model.
            best_params: A dictionary of the best parameter values for 'n_estimators', 'max_depth', and 'min_samples_split'.
            validation_accuracy: Accuracy score of the final model on the validation data.
    '''
    import numpy as np
    import pandas as pd
    import matplotlib.pyplot as plt
    from sklearn.model_selection import train_test_split
    from sklearn.model_selection import GridSearchCV
    from sklearn.ensemble import RandomForestClassifier
    from sklearn.metrics import confusion_matrix, accuracy_score, precision_score, recall_score, f1_score, roc_auc_score, roc_curve, classification_report


    # Define the parameter grid
    n_estimators = [int(x) for x in np.linspace(start = 200, stop = 2000, num = 3)]
    max_depth = [int(x) for x in np.linspace(10, 100, num = 5)]
    max_depth.append(None)
    min_samples_split = [2, 5, 10]

    param_grid = {
    'n_estimators': n_estimators,
    'max_depth': max_depth,
    'min_samples_split': min_samples_split
    }
    print("Parameter Grid:", param_grid)

    # Prepare the training 
    X_train = df1.drop(['survival'], axis=1)
    y_train = df1['survival']
    # Prepare the validation data and test data ===< editable >
    X = df2.drop(['survival'], axis=1)
    y = df2['survival']
    # Split the test data set into a validation set and a test set
    X_val, X_test, y_val, y_test = train_test_split(X, y, test_size=0.7, random_state=42)

    # Perform GridSearchCV for hyperparameter tuning
    grid_search = GridSearchCV(RandomForestClassifier(random_state=42), param_grid, cv=3, scoring='accuracy')
    grid_search.fit(X_train, y_train)

    # Access the best parameters
    best_params = grid_search.best_params_
    print("Best Parameters:", best_params)

    # Evaluate the best model on the validation data
    best_model = grid_search.best_estimator_
    validation_accuracy = best_model.score(X_val, y_val)
    print("Validation Accuracy:", validation_accuracy)

    # Finalize the model
    final_model = best_model
    print('================ End of Hyperparameter Tuning =================')
    
    print('Apply to final best model with test data')
    # Step 1: Make Predictions on Test Data
    y_pred = final_model.predict(X_test)
    # Step 2: Evaluate Model Performance on Test Data
    test_accuracy = accuracy_score(y_test, y_pred)
    print("Test Accuracy:", test_accuracy)
    # Step 3: Generate Classification Report (Optional)
    class_report = classification_report(y_test, y_pred)
    print("Classification Report:\n", class_report)
    print('================         End of Test         =================')

    print('>> Extract feature importances from the trained model')
    feature_importances = final_model.feature_importances_
    feature_names = X_train.columns  # Get the names of the features
    importance_df = pd.DataFrame({'Feature': feature_names, 'Importance': feature_importances})      # Create a DataFrame to store feature importances
    importance_df = importance_df.sort_values(by='Importance', ascending=False)    # Sort the DataFrame by importance in descending order
    col_name = f"{year}_{month}"    
    importance_df.rename(columns={'Importance': col_name}, inplace=True)    # Rename the 'Importance' column to the specified column name

    print('>> Create a confusion matrix and all of the scores such as accuracy, precision, recall, f1score, auc score')
    # Create a confusion matrix from labels and prediction results
    confusion_matrix_result = confusion_matrix(y_test, y_pred)
    # Convert the matrix to a flattened list
    confusion_matrix_list = confusion_matrix_result.flatten().tolist()
    print(confusion_matrix_result)
    
    # Calculate accuracy using labels and predicted results: (TP+TN)/(TP+TN+FP+FN)
    accuracy = test_accuracy
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
    # Display the ROC plot
    plt.show()
    
    # Calculate AUC score
    roc_auc = roc_auc_score(y_test, probas[:, 1]) 
    print('Accuracy Score: ', accuracy, 'Precision Score: ', precision, "Recall Score: ", recall, 'F1-Score: ', f1_score_result, "AUC Score: ", roc_auc)

    return X_val, X_test, final_model, best_params, validation_accuracy, importance_df, confusion_matrix_list, accuracy, precision, recall, f1_score_result, roc_auc

