import pandas as pd
import numpy as np
from collections import Counter

# Set the maximum number of displayed columns and rows
pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

deceased_info = pd.read_csv('/Deceased_info.csv', low_memory=False)
hospitalized_info = pd.read_csv('/Hospitalized_info.csv', low_memory=False)
print(deceased_info.shape, hospitalized_info.shape)

annotmut_d = pd.read_csv('/annotmut_d.csv', low_memory=False)
annotmut_h = pd.read_csv('/annotmut_h.csv', low_memory=False)
print(annotmut_d.shape, annotmut_h.shape)

merged_df_d = pd.merge(deceased_info[['gisaid_epi_isl', 'pangolin_lineage','partiallyAliased','clade_who','date','age','region','sex']], annotmut_d, left_on='gisaid_epi_isl', right_on='sampleID', how='right')
merged_df_d.head()
merged_df_h = pd.merge(hospitalized_info[['gisaid_epi_isl', 'pangolin_lineage','partiallyAliased','clade_who','date','age','region','sex']], annotmut_h, left_on='gisaid_epi_isl', right_on='sampleID', how='right')
merged_df_h.head()

def ready_dataset(patient_info, annotated_data, status='deceased'):
  merged_df = pd.merge(patient_info[['gisaid_epi_isl', 'pangolin_lineage','partiallyAliased','clade_who','date','age','region','sex']], annotated_data, left_on='gisaid_epi_isl', right_on='sampleID', how='right')
  deceased_mut = merged_df.copy()
  deceased_mut['status'] = status    ## change into deceased/or hospitalized
  
  # set the new column as values with 'Synonymous':1, 'Missense': 1, 'Nonsense':1, 'Stop_loss':1, 'no-SNP': 0'
  deceased_mut.loc[:, 'impact_transform'] = 0  # Initialize the new column with default value
  deceased_mut.loc[deceased_mut['impact'].isin(['Synonymous', 'Missense', 'Nonsense', 'Stop_loss']), 'impact_transform'] = 1
  
  # check duplication: this is strange outcome because how could it possible the gene is different despite of the same position 
  duplicates = deceased_mut.duplicated(subset=['pos', 'sampleID'], keep=False)
  duplicate_rows = deceased_mut[duplicates]
  
  # print(duplicate_rows)
  
  # Remove duplicate entries based on 'pos' and 'sampleID': drop the rows including position number of '13474' and '27191'
  deceased_mut = deceased_mut.drop_duplicates(subset=['pos', 'sampleID'], keep=False)
  deceased_mut['date']= pd.to_datetime(deceased_mut['date']).dt.to_period('M')
