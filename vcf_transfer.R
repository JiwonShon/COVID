# mut_mod.R
#' ============================================================================
# project: sars-cov-2 virus population: convert single vcf
# author: Jiwon Shon
# created: 2022-12-20
# last edited: 2023-09-20
#' ============================================================================

# load R packages ==============================================================
require(readxl)
library(data.table)
library(magrittr)
install.packages("tidyverse")
library(tidyverse)
library(plyr);library(dplyr)
library(skimr)
library('dndscv')
library(reshape2)
#library(rvg);library(officer);library(devEMF)
library(covid19.analytics)


#' ============================================================================
#' set the working directory
getwd()

Deceased <- read.table('../../../../GISAID/latest_data_set_20230918/merged_dec_ex.tsv', sep='\t', header=TRUE)%>% 
  subset(qc.overallStatus == 'good') %>% 
  mutate(status='Deceased')
dim(Deceased) # [1] 676  41

Hospitalized <- read.table('../../../../GISAID/latest_data_set_20230918/merged_hos_ex.tsv', sep='\t', header=TRUE)%>% 
  subset(qc.overallStatus == 'good') %>% 
  mutate(status='Hospitalized')
dim(Hospitalized) # [1] 11113    41


#' start modification============================================================================


# obtain covid19's genomic data
X.covid19.genomic.data(graphics.ON = TRUE)
covid19.gen.seq <- covid19.genomic.data()

# display the actual RNA seq
reference <- covid19.gen.seq$NC_045512.2
covref <- paste(unlist(reference),collapse="")%>% toupper()



# Hospitalized ====
## Hospitalized for substitutions ====
Hospitalized_mod <- Hospitalized %>% .[,c('gisaid_epi_isl', 'substitutions','date','clade_who','status')] %>% filter(!is.na(substitutions)) %>% unique()
stopifnot(all(!duplicated(Hospitalized_mod$gisaid_epi_isl)))


Hospitalized_mod <-ddply(Hospitalized_mod,'gisaid_epi_isl',function(X) {
  #cat('\n',unique(X$gisaid_epi_isl))
  stopifnot(nrow(X)==1)
  substitutions_str <-unlist(strsplit(X$substitutions,split=','))
  d <-data.frame(substitution=substitutions_str,
                 stringsAsFactors = F) %>%
    mutate(position=gsub("([ATGC]+)([0-9]+)([A-Z]+)","\\2",substitution),
           ref_base=gsub("([ATGC]+)([0-9]+)([A-Z]+)","\\1",substitution),
           alt_base=gsub("([ATGC]+)([0-9]+)([A-Z]+)","\\3",substitution)) %>%
    mutate(chk=substitution==paste0(ref_base,position,alt_base))
  stopifnot(all(d$chk))
  d <-d %>% select(-chk)
  stopifnot(all(!duplicated(d$position)))
  X <-X[rep(1,nrow(d)),]
  X <-bind_cols(X,d) %>% select(-substitutions)
  return(X)
}) %>% as_tibble()

# [1] "gisaid_epi_isl" "date"           "clade_who"      "status"         "substitution"  
# [6] "position"       "ref_base"       "alt_base"  
# colnames(Hospitalized_mod)[which(names(Hospitalized_mod) == "gisaid_epi_isl")] <- 'sampleID'
# colnames(Hospitalized_mod)[which(names(Hospitalized_mod) == "pos")] <- 'position'
# colnames(Hospitalized_mod)[which(names(Hospitalized_mod) == "ref")] <- 'ref_base'
# colnames(Hospitalized_mod)[which(names(Hospitalized_mod) == "mut")] <- 'alt_base'
# 
# colnames(Hospitalized_mod)[which(names(Hospitalized_mod) == "sampleID")] <- 'gisaid_epi_isl'


## Hospitalized for deletion ====
Hospitalized_del_mod <- Hospitalized %>% .[,c('gisaid_epi_isl', 'deletions','date','clade_who','status')] %>% filter(!is.na(deletions)) %>% unique()

# separate deleted positions
stopifnot(all(!duplicated(Hospitalized_del_mod$gisaid_epi_isl)))
Hospitalized_del_mod <-ddply(Hospitalized_del_mod,'gisaid_epi_isl',function(X) {
  stopifnot(nrow(X)==1)
  deletions_str <-unlist(strsplit(X$deletions,split=','))
  reference <- covid19.gen.seq$NC_045512.2
  d <-data.frame(deletion=deletions_str,
                 stringsAsFactors = F) %>%
    mutate(startpos=gsub("([0-9]+)([-])([0-9]+)","\\1",deletion)%>% as.integer(),
           endpos=gsub("([0-9]+)([-])([0-9]+)","\\3",deletion) %>% as.integer()) %>%
    mutate(chk=deletion==paste0(startpos,"-",endpos))
  stopifnot(all(!duplicated(d$startpos)))
  X <-X[rep(1,nrow(d)),]
  X <-bind_cols(X,d) %>% select(-deletions)
  return(X)
}) %>% as_tibble()

## add reference sequence
Hospitalized_del_mod <- Hospitalized_del_mod %>% 
  mutate(ref = str_sub(covref, start = startpos, end = endpos))%>% 
  mutate(mut = '-')
# colnames
# [1] "gisaid_epi_isl" "date"           "clade_who"      "status"         "deletion"      
# [6] "pos"            "endpos"         "chk"            "ref"            "mut" 

# colnames(a)[which(names(a) == "startpos")] <- 'pos'
colnames(Hospitalized_del_mod)[which(names(Hospitalized_del_mod) == "gisaid_epi_isl")] <- 'sampleID'




## Hospitalized for insertion ====
Hospitalized_ins_mod <- Hospitalized %>% .[,c('gisaid_epi_isl', 'insertions','date','clade_who','status')] %>% filter(!is.na(insertions)) %>% unique()

# separate insertion positions
stopifnot(all(!duplicated(Hospitalized_ins_mod$gisaid_epi_isl)))
Hospitalized_ins_mod <-ddply(Hospitalized_ins_mod,'gisaid_epi_isl',function(X) {
  stopifnot(nrow(X)==1)
  insertion_str <-unlist(strsplit(X$insertions,split=','))
  d <-data.frame(insertion=insertion_str,
                 stringsAsFactors = F) %>%
    mutate(pos=gsub("([0-9]+)([:])([A-Z]+)","\\1",insertion) %>% as.integer(),
           mut=gsub("([0-9]+)([:])([A-Z]+)","\\3",insertion)) %>%
    mutate(chk=insertion==paste0(pos,":",mut))%>% 
    mutate(ref='-')
  stopifnot(all(!duplicated(d$pos)))
  X <-X[rep(1,nrow(d)),]
  X <-bind_cols(X,d) %>% select(-insertions)
  return(X)
}) %>% as_tibble()

# [1] "gisaid_epi_isl" "date"           "clade_who"      "status"         "insertion"     
# [6] "pos"            "mut"            "chk"            "ref"  
colnames(Hospitalized_ins_mod)[which(names(Hospitalized_ins_mod) == "gisaid_epi_isl")] <- 'sampleID'

sub <- Hospitalized_mod %>% select('sampleID', 'pos', 'ref', 'mut')
#del <- Hospitalized_del_mod %>% select('sampleID', 'pos', 'ref', 'mut')
#ins <- Hospitalized_ins_mod %>% select('sampleID', 'pos', 'ref', 'mut')
#vcf_hospiatlized <- rbind(sub, del, ins) %>% unique()



# save RDS
saveRDS(Hospitalized_mod, '../../../../GISAID/latest_data_set_20230918/output/Hospitalized_mod_sub.rds')
# saveRDS(Hospitalized_del_mod, './Hospitalized_mod_del.rds')
# saveRDS(Hospitalized_ins_mod, './Hospitalized_mod_ins.rds')
# saveRDS(vcf_hospiatlized, '../../../../GISAID/latest_data_set_20230918/output/vcf_hospiatlized.rds')
# Specify the file path where you want to save the CSV
file_path <- "../../../../GISAID/latest_data_set_20230918/output/Hospitalized_mod_sub.csv"
# Save data as a CSV file
write.csv(Hospitalized_mod, file = file_path, row.names = FALSE)


# Deceased ====
## Deceased for substitutions ====
Deceased_mod <- Deceased %>% .[,c('gisaid_epi_isl', 'substitutions','date','clade_who','status')] %>% filter(!is.na(substitutions)) %>% unique()
stopifnot(all(!duplicated(Deceased_mod$gisaid_epi_isl)))
Deceased_mod <-ddply(Deceased_mod,'gisaid_epi_isl',function(X) {
  #cat('\n',unique(X$gisaid_epi_isl))
  stopifnot(nrow(X)==1)
  substitutions_str <-unlist(strsplit(X$substitutions,split=','))
  d <-data.frame(substitution=substitutions_str,
                 stringsAsFactors = F) %>%
    mutate(position=gsub("([ATGC]+)([0-9]+)([A-Z]+)","\\2",substitution),
           ref_base=gsub("([ATGC]+)([0-9]+)([A-Z]+)","\\1",substitution),
           alt_base=gsub("([ATGC]+)([0-9]+)([A-Z]+)","\\3",substitution)) %>%
    mutate(chk=substitution==paste0(ref_base,position,alt_base))
  stopifnot(all(d$chk))
  d <-d %>% select(-chk)
  stopifnot(all(!duplicated(d$position)))
  X <-X[rep(1,nrow(d)),]
  X <-bind_cols(X,d) %>% select(-substitutions)
  return(X)
}) %>% as_tibble()

# [1] "gisaid_epi_isl" "date"           "clade_who"      "status"         "substitution"  
# [6] "position"       "ref_base"       "alt_base"  
# colnames(Deceased_mod)[which(names(Deceased_mod) == "gisaid_epi_isl")] <- 'sampleID'
# colnames(Deceased_mod)[which(names(Deceased_mod) == "pos")] <- 'position'
# colnames(Deceased_mod)[which(names(Deceased_mod) == "ref")] <- 'ref_base'
# colnames(Deceased_mod)[which(names(Deceased_mod) == "mut")] <- 'alt_base'
# colnames(Deceased_mod)[which(names(Deceased_mod) == "sampleID")] <- 'gisaid_epi_isl'



## Deceased for deletion ====
Deceased_del_mod <- Deceased %>% .[,c('gisaid_epi_isl', 'deletions','date','clade_who','status')] %>% filter(!is.na(deletions)) %>% unique()

# separate deleted positions
stopifnot(all(!duplicated(Deceased_del_mod$gisaid_epi_isl)))
Deceased_del_mod <-ddply(Deceased_del_mod,'gisaid_epi_isl',function(X) {
  stopifnot(nrow(X)==1)
  deletions_str <-unlist(strsplit(X$deletions,split=','))
  reference <- covid19.gen.seq$NC_045512.2
  d <-data.frame(deletion=deletions_str,
                 stringsAsFactors = F) %>%
    mutate(startpos=gsub("([0-9]+)([-])([0-9]+)","\\1",deletion)%>% as.integer(),
           endpos=gsub("([0-9]+)([-])([0-9]+)","\\3",deletion) %>% as.integer()) %>%
    mutate(chk=deletion==paste0(startpos,"-",endpos))
  stopifnot(all(!duplicated(d$startpos)))
  X <-X[rep(1,nrow(d)),]
  X <-bind_cols(X,d) %>% select(-deletions)
  return(X)
}) %>% as_tibble()

## add reference sequence
Deceased_del_mod <- Deceased_del_mod %>% 
  mutate(ref = str_sub(covref, start = startpos, end = endpos))%>% 
  mutate(mut = '-')
# colnames
# [1] "gisaid_epi_isl" "date"           "clade_who"      "status"         "deletion"      
# [6] "pos"            "endpos"         "chk"            "ref"            "mut" 

# colnames(a)[which(names(a) == "startpos")] <- 'pos'
colnames(Deceased_del_mod)[which(names(Deceased_del_mod) == "gisaid_epi_isl")] <- 'sampleID'




## Deceased for insertion ====
Deceased_ins_mod <- Deceased %>% .[,c('gisaid_epi_isl', 'insertions','date','clade_who','status')] %>% filter(!is.na(insertions)) %>% unique()

# separate insertion positions
stopifnot(all(!duplicated(Deceased_ins_mod$gisaid_epi_isl)))
Deceased_ins_mod <-ddply(Deceased_ins_mod,'gisaid_epi_isl',function(X) {
  stopifnot(nrow(X)==1)
  insertion_str <-unlist(strsplit(X$insertions,split=','))
  d <-data.frame(insertion=insertion_str,
                 stringsAsFactors = F) %>%
    mutate(pos=gsub("([0-9]+)([:])([A-Z]+)","\\1",insertion) %>% as.integer(),
           mut=gsub("([0-9]+)([:])([A-Z]+)","\\3",insertion)) %>%
    mutate(chk=insertion==paste0(pos,":",mut))%>% 
    mutate(ref='-')
  stopifnot(all(!duplicated(d$pos)))
  X <-X[rep(1,nrow(d)),]
  X <-bind_cols(X,d) %>% select(-insertions)
  return(X)
}) %>% as_tibble()

# [1] "gisaid_epi_isl" "date"           "clade_who"      "status"         "insertion"     
# [6] "pos"            "mut"            "chk"            "ref"  
colnames(Deceased_ins_mod)[which(names(Deceased_ins_mod) == "gisaid_epi_isl")] <- 'sampleID'

sub_d <- Deceased_mod %>% select('sampleID', 'pos', 'ref', 'mut')
del_d <- Deceased_del_mod %>% select('sampleID', 'pos', 'ref', 'mut')
ins_d <- Deceased_ins_mod %>% select('sampleID', 'pos', 'ref', 'mut')
vcf_hospiatlized <- rbind(sub_d, del_d, ins_d) %>% unique()



# save RDS
saveRDS(Deceased_mod, '../../../../GISAID/latest_data_set_20230918/output/Deceased_mod_sub.rds')
# saveRDS(Deceased_del_mod, './Deceased_mod_del.rds')
# saveRDS(Deceased_ins_mod, './Deceased_mod_ins.rds')
# saveRDS(vcf_deceased, '../../../../GISAID/latest_data_set_20230918/output/vcf_deceased.rds')
# Specify the file path where you want to save the CSV
file_path <- "../../../../GISAID/latest_data_set_20230918/output/Deceased_mod_sub.csv"
# Save data as a CSV file
write.csv(Deceased_mod, file = file_path, row.names = FALSE)


# 4. dN/dS  =======================================================================
#' source:https://github.com/im3sanger/dndscv
#' tutorial: https://htmlpreview.github.io/?http://github.com/im3sanger/dndscv/blob/master/vignettes/dNdScv.html
#' ============================================================================
# Using dNdScv in SARS-Cov-2
load('RefCDS_MN908947.3.peptides.rda'); print(RefCDS[[6]])

## a) Make input file list for dN/dS ======
# Deceased, set the data set into input data for dnds
Deceased_dnds_input <- Deceased_mod %>% 
  select(gisaid_epi_isl, position, ref_base, alt_base, date) %>% 
  mutate(chr='MN908947.3') %>% unique()
Deceased_dnds_input <- Deceased_dnds_input %>% select(gisaid_epi_isl, chr, position, ref_base, alt_base, date)
colnames(Deceased_dnds_input) <- c('sampleID', 'chr', 'pos', 'ref', 'mut', 'date')

# Hospitalized, set the data set into input data for dnds
Hospitalized_dnds_input <- Hospitalized_mod %>% 
  select(gisaid_epi_isl, position, ref_base, alt_base, date) %>% 
  mutate(chr='MN908947.3') %>% unique()
Hospitalized_dnds_input <- Hospitalized_dnds_input %>% select(gisaid_epi_isl, chr, position, ref_base, alt_base, date)
colnames(Hospitalized_dnds_input) <- c('sampleID', 'chr', 'pos', 'ref', 'mut', 'date')

# all of both status
blind_dnds_input <- rbind(Deceased_dnds_input, Hospitalized_dnds_input)

## b) Do dN/dS =========
# Deceased
Deceased_dndsout <- dndscv(Deceased_dnds_input, refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
d_anotmuts <- Deceased_dndsout$annotmuts

# Hospitalized
Hospitalized_dndsout <- dndscv(Hospitalized_dnds_input, refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
h_anotmuts <- Hospitalized_dndsout$annotmuts


# Total
blind_dndsout <- dndscv(blind_dnds_input, refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
b_anotmuts <- blind_dndsout$annotmuts


saveRDS(Deceased_dndsout, '../../../../GISAID/latest_data_set_20230918/Deceased_dndsout.rds')
saveRDS(Hospitalized_dndsout, '../../../../GISAID/latest_data_set_20230918/Hospitalized_dndsout.rds')
saveRDS(blind_dndsout, '../../../../GISAID/latest_data_set_20230918/blind_dndsout.rds')


