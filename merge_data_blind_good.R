#' ============================================================================
# project: sars-cov-2 virus population: Blind, qc=good
# author: Jiwon Shon
# created: 2022-11-10
# last edited: 2022-12-07
#' ============================================================================

# load R packages ==============================================================
require(readxl);library(data.table);library(magrittr);library(tidyverse);library(tableone)
library(dplyr);library(plyr); library(skimr);library(scales)
library(writexl);library(openxlsx)
library('dndscv')
library(reshape2)
library(ggplot2);library(patchwork);library(ggpubr);library(RColorBrewer)
library(rvg);library(officer);library(devEMF)
library(maftools)


# set the working directory ===================================================
getwd()

blind_all <- readRDS('../../data/blind/allstatus.rds') %>% 
  subset(qc.overallStatus == 'good') # 47272

Deceased <- blind_all %>% subset(status=="Deceased") # 7251
Hospitalized <- blind_all %>% subset(status=="Hospitalized") # 47272



# 1. Blind_info =======

### create workbook
d1 <- Deceased$date %>% table() %>% as.data.frame()
d2 <- Hospitalized$date %>% table() %>% as.data.frame()
d3 <- blind_all$date %>% table() %>% as.data.frame()


s1 <- Deceased$sex %>% table(exclude = NULL) %>% as.data.frame()
s2 <- Hospitalized$sex %>% table(exclude = NULL) %>% as.data.frame()
s3 <- blind_all$sex %>% table(exclude = NULL) %>% as.data.frame()

r1 <- Deceased$region %>% table(exclude = NULL) %>% as.data.frame()
r2 <- Hospitalized$region %>% table(exclude = NULL) %>% as.data.frame()
r3 <- blind_all$region %>% table(exclude = NULL) %>% as.data.frame()

# 1: <18, 2: 18-64, 3: >65, 4 = NA
blind_all$age <- blind_all$age %>% as.numeric()
blind_all <- blind_all %>% mutate(age_range = ifelse(age < 18,  1, 
                                       ifelse(age >= 18 & age < 65, 2,
                                       ifelse(age >= 65, 3, 4))))

d <- blind_all %>% subset(status=='Deceased') 
a1 <- d$age_range %>% table(exclude=NULL) %>% as.data.frame()
a <- blind_all %>% subset(status=='Hospitalized') 
a2 <- a$age_range %>% table(exclude=NULL) %>% as.data.frame()

a3 <- blind_all$age_range %>% table(exclude=NULL) %>% as.data.frame()

v1 <- Deceased$clade_who %>% table(exclude=NULL) %>% as.data.frame()
v2 <- Hospitalized$clade_who %>% table() %>% as.data.frame()
v3 <- blind_all$clade_who %>% table() %>% as.data.frame()

tablelist <- list('D_date'=d1, 'H_date'=d2, 'all_date'=d3, 
                  'D_region'=r1, 'H_region'=r2, 'all_region'=r3,
                  'D_sex'=s1, 'H_sex'=s2, 'all_sex'=s3,
                  'D_age'=a1, 'H_age'=a2, 'all_age'=a3,
                  'D_voc'=v1, 'H_voc'=v2, 'all_voc'=v3)

write.xlsx(tablelist, file = "../../data/blind/blind_info.xlsx", rowNames=T)

# count sample number of VOC during date time
a <- blind_all %>% subset(status== 'Deceased' & clade_who=="Omicron")
a$date %>% table(exclude = NULL) %>% as.data.frame()

'%!in%' <- function(x,y)!('%in%'(x,y))
b<- blind_all %>% subset(status== 'Hospitalized' & clade_who %!in% c('Alpha', 'Beta', 'Delta', 'Gamma', "Omicron"))
b$date %>% table(exclude = NULL) %>% as.data.frame()

# count sample number of Omicron during date time: 21K 21l 22A 22B 22C 22D 22E 22F
a <- blind_all %>% subset(status== 'Deceased' & clade_nextstrain=="22F")
a$date %>% table(exclude = NULL) %>% as.data.frame()


### plot for date
ggplot(blind_all) + 
  geom_bar(aes(x = date, fill = status))+
  # geom_line(aes(x=date),color = 'black') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))+
  scale_color_manual('status', values = c('Deceased' = "red", 'Hospitalized'= "blue"))+
  labs(y="Number of genomes", x='Date',
       subtitle = 'Number of genomes: Deceased and Hospitalized')





# 2. Temporal dynamics =========================================================

## a) mutation event + date (plot) =====
pd <- position_dodge(0.1) # move them .05 to the left and right

pall_sub <- ggplot(blind_all,aes(x=date, y=totalSubstitutions))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))+
  labs(y="Mutation events per genome",
       subtitle = 'Temporal dynamics of SARS-CoV-2: Average Events of Substitutions')+
  ylim(c(0,100))
pall_del <- ggplot(blind_all, aes(x=date, y=totalDeletions))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))+
  labs(y="Mutation events per genome",
       subtitle = 'Temporal dynamics of SARS-CoV-2: Average Events of Deletions')+
  ylim(c(0,100))
pall_ins <- ggplot(blind_all, aes(x=date, y=totalInsertions))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))+
  labs(y="Mutation events per genome",
       subtitle = 'Temporal dynamics of SARS-CoV-2: Average Events of Insertions')+
  ylim(c(0,100))
plot3 <- pall_sub/pall_del/pall_ins


## b) mutation event + date + status (plot) =====
# substitution
psub_ex <- ggplot(blind_all, aes(x=date, y=totalSubstitutions, fill=status))+
  geom_boxplot(aes(color=status),outlier.shape = NA)+
  scale_color_manual('status', values = c('Deceased' = "red", 'Hospitalized'= "blue"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))+
  labs(y="Mutation events per genome",
       subtitle = 'Temporal dynamics of SARS-CoV-2: Average Events of Substitutions')+
  ylim(c(0,100))

# deletion
pdel_ex <- ggplot(blind_all, aes(x=date, y=totalDeletions, fill=status))+
  geom_boxplot(aes(color=status),outlier.shape = NA)+
  scale_color_manual('status',values = c('Deceased' = "red", 'Hospitalized'= "blue"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))+
  labs(y="Mutation events per genome",
       subtitle = 'Temporal dynamics of SARS-CoV-2 mutations: Average Events of Deletions')+
  ylim(c(0,100))

# insertion
pins_ex <- ggplot(blind_all,aes(x=date, y=totalInsertions, fill=status))+
  geom_boxplot(aes(color=status),outlier.shape = NA)+
  scale_color_manual('status',values = c('Deceased' = "red", 'Hospitalized'= "blue"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))+
  labs(y="Mutation events per genome",
       subtitle = 'Temporal dynamics of SARS-CoV-2 mutations: Average Events of Insertions')+
  ylim(c(0,100))

p5 <- psub_ex/pdel_ex/pins_ex


## c) mutation event + position + status (plot) =====
# deceased: sample size
d1 <- blind_all %>% subset(status=='Deceased') %>%  
  ggplot(aes(status)) + geom_bar(fill='red', width = 1) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y=element_blank(),
        panel.grid.minor.y =element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x='Deceased', y='Number of genome') + 
  scale_y_continuous(limits=c(0,45000))+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5)
# deceased: mutation event on position
d2 <- top_d %>% 
  ggplot(aes(x=pos, 
             y= norm_by_sample, colour=domain, fill = domain)) + 
  geom_bar(stat='identity', width = 0.5) +
  scale_x_continuous(limits=c(0,30000))+
  theme_minimal()+ 
  scale_color_manual('domain', 
                     values = c('ORF1ab'='IndianRed','S'='#40E0D0','ORF3a'='LightCoral','E'='#FFBF00','M'='#9FE2BF','ORF6'='Salmon','ORF7a'='DarkSalmon','ORF8'='LightSalmon','N'='#6495ED','ORF10'='#CCCCFF'))+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        legend.position="none",
        plot.subtitle=element_text(size=14, face="italic", color="black"))+
  labs(x="Position", 
       y="Mutation events per genome",
       subtitle='Deceased')+
  ylim(c(0,1))

# Hospitalized: sample size
a1 <- blind_all %>% subset(status=='Hospitalized') %>%  #, Hospitalized
  ggplot(aes(status)) + geom_bar(width = 1, fill='blue') + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y=element_blank(),
        panel.grid.minor.y =element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x='Hospitalized', y='Number of genome') + 
  scale_y_continuous(limits=c(0,45000))+
  geom_text(aes(label = ..count..), stat = "count",vjust = -0.5)
# Hospitalized: mutation event on position
a2 <- top_i %>% 
  ggplot(aes(x=pos, 
             y= norm_by_sample, colour=domain, fill = domain)) + 
  geom_bar(stat='identity', width = 0.5) +
  scale_x_continuous(limits=c(0,30000))+
  theme_minimal()+ 
  scale_color_manual('domain', 
                     values = c('ORF1ab'='IndianRed','S'='#40E0D0','ORF3a'='LightCoral','E'='#FFBF00','M'='#9FE2BF','ORF6'='Salmon','ORF7a'='DarkSalmon','ORF8'='LightSalmon','N'='#6495ED','ORF10'='#CCCCFF'))+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        legend.position="none",
        plot.subtitle=element_text(size=14, face="italic", color="black"))+
  labs(x="Position", 
       y="Mutation events per genome",subtitle='Hospitalized') +
  ylim(c(0,1))

# domain plot
gene_split <- data.frame(do.call('rbind',strsplit(df$gene,split=':', fixed=TRUE)))
df <- df %>% mutate(gene= gene_split$X1) %>% arrange(CDS_start)
df$event <- 1

map_domain <- ggplot(dff, aes(xmin = CDS_start, xmax = CDS_end, ymin=0, ymax=event, fill=gene)) +
  geom_rect(stat='identity')+
  theme_minimal()+
  scale_x_continuous(limits=c(0,30000))+
  theme(axis.text.x = element_blank(),
        axis.title.x= element_blank(),
        panel.grid = element_blank(),
        legend.position="none")+
  scale_fill_manual('gene', 
                     values = c('ORF1ab'='#CD5C5C','S'='#40E0D0','ORF3a'='LightCoral','E'='#FFBF00','M'='#9FE2BF','ORF6'='Salmon','ORF7a'='DarkSalmon','ORF8'='LightSalmon','N'='#6495ED','ORF10'='#CCCCFF'))
  

map_domain2 <- ggplot(dff, aes(xmin = CDS_start, xmax = CDS_end, ymin=0, ymax=event, fill=gene)) +
  geom_rect(stat='identity')+
  theme_minimal()+
  scale_x_continuous(limits=c(0,30000))+
  theme(axis.text.x = element_blank(),
        axis.title.x= element_blank(),
        panel.grid = element_blank())+
  scale_fill_manual('gene', 
                    values = c('ORF1ab'='#CD5C5C','S'='#40E0D0','ORF3a'='LightCoral','E'='#FFBF00','M'='#9FE2BF','ORF6'='Salmon','ORF7a'='DarkSalmon','ORF8'='LightSalmon','N'='#6495ED','ORF10'='#CCCCFF'))


# domain_list <- c('ORF1ab','S','ORF3a','E','M','ORF6','ORF7a','ORF8','N','ORF10')
# map_domain <- ggplot(data=cov_domain, aes(x=seq, y=1, fill=domain)) +
#   geom_tile(stat='identity')+
#   scale_fill_manual('domain', 
#                      values = c('ORF1ab'='IndianRed','S'='#40E0D0','ORF3a'='LightCoral','E'='#FFBF00','M'='#9FE2BF','ORF6'='Salmon','ORF7a'='DarkSalmon','ORF8'='LightSalmon','N'='#6495ED','ORF10'='#CCCCFF'))+
#   theme_minimal()+
#   scale_fill_discrete(breaks=domain_list)+
#   scale_x_continuous(limits=c(0,30000))+
#   theme(axis.text.x = element_blank(),
#         axis.title.x= element_blank(),
#         panel.grid = element_blank(),
#         legend.position="none")



map_domain + geom_text(y=1, aes(label=domain_list))








plot1 <- d1+d2+map_domain + plot_layout(design = layout)
plot2 <- a1+a2+map_domain + plot_layout(design = layout)

grph <- d1+d2+map_domain+a1+a2+map_domain + plot_layout(design = layout)

layout <- "
ABBBBBBBBBBBBB
ABBBBBBBBBBBBB
ABBBBBBBBBBBBB
ABBBBBBBBBBBBB
ABBBBBBBBBBBBB
#CCCCCCCCCCCCC
"
# DEEEEEEEEEEEE
# DEEEEEEEEEEEE
# DEEEEEEEEEEEE
# DEEEEEEEEEEEE
# #FFFFFFFFFFFF
emf("grph.emf", width = 15, height = 9, emfPlus = F)
print(grph)
dev.off()

dff <- read.xlsx('blind/genesize.xlsx')



# 3. Extract mutation ========================================================
## a) Extract substitution mutation ==========================================
# Deceased
Deceased_mod <- Deceased %>% .[,c('gisaid_epi_isl', 'substitutions','date','clade_who','status')] %>% filter(!is.na(substitutions))
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

# Hospitalized
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


# save RDS
saveRDS(Deceased_mod, '../../data/blind/Deceased_mod_sub.rds')
saveRDS(Hospitalized_mod, '../../data/blind/Hospitalized_mod_sub.rds')


## b) Extract deletion mutation ===============================================





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



# do dnds separate 19 bins (match date to mod and collect
bin1_d_out <- dndscv(Deceased_dnds_input%>% subset(date < '2021-04') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin1_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date < '2021-04') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin1_all_out <- dndscv(blind_dnds_input%>% subset(date < '2021-04') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin2_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2021-04') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin2_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2021-04') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin2_all_out <- dndscv(blind_dnds_input%>% subset(date == '2021-04') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin3_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2021-05') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin3_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2021-05') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin3_all_out <- dndscv(blind_dnds_input%>% subset(date == '2021-05') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin4_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2021-06') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin4_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2021-06') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin4_all_out <- dndscv(blind_dnds_input%>% subset(date == '2021-06') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin5_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2021-07') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin5_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2021-07') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin5_all_out <- dndscv(blind_dnds_input%>% subset(date == '2021-07') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin6_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2021-08') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin6_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2021-08') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin6_all_out <- dndscv(blind_dnds_input%>% subset(date == '2021-08') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin7_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2021-09') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin7_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2021-09') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin7_all_out <- dndscv(blind_dnds_input%>% subset(date == '2021-09') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin8_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2021-10') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin8_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2021-10') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin8_all_out <- dndscv(blind_dnds_input%>% subset(date == '2021-10') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin9_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2021-11') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin9_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2021-11') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin9_all_out <- dndscv(blind_dnds_input%>% subset(date == '2021-11') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin10_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2021-12') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin10_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2021-12') %>% select(-date),
                     refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin10_all_out <- dndscv(blind_dnds_input%>% subset(date == '2021-12') %>% select(-date),
                       refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin11_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2022-01') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin11_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2022-01') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin11_all_out <- dndscv(blind_dnds_input%>% subset(date == '2022-01') %>% select(-date),
                        refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin12_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2022-02') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin12_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2022-02') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin12_all_out <- dndscv(blind_dnds_input%>% subset(date == '2022-02') %>% select(-date),
                        refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin13_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2022-03') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin13_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2022-03') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin13_all_out <- dndscv(blind_dnds_input%>% subset(date == '2022-03') %>% select(-date),
                        refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin14_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2022-04') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin14_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2022-04') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin14_all_out <- dndscv(blind_dnds_input%>% subset(date == '2022-04') %>% select(-date),
                        refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin15_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2022-05') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin15_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2022-05') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin15_all_out <- dndscv(blind_dnds_input%>% subset(date == '2022-05') %>% select(-date),
                        refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin16_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2022-06') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin16_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2022-06') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin16_all_out <- dndscv(blind_dnds_input%>% subset(date == '2022-06') %>% select(-date),
                        refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin17_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2022-07') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin17_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2022-07') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin17_all_out <- dndscv(blind_dnds_input%>% subset(date == '2022-07') %>% select(-date),
                        refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin18_d_out <- dndscv(Deceased_dnds_input%>% subset(date == '2022-08') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin18_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date == '2022-08') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin18_all_out <- dndscv(blind_dnds_input%>% subset(date == '2022-08') %>% select(-date),
                        refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)

bin19_d_out <- dndscv(Deceased_dnds_input%>% subset(date > '2022-08') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin19_h_out <- dndscv(Hospitalized_dnds_input%>% subset(date > '2022-08') %>% select(-date),
                      refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)
bin19_all_out <- dndscv(blind_dnds_input%>% subset(date > '2022-08') %>% select(-date),
                        refdb= 'RefCDS_MN908947.3.peptides.rda', cv=NULL, max_muts_per_gene_per_sample=NULL, max_coding_muts_per_sample=NULL)




saveRDS(bin1_d_out, '../../data/blind/bin1_d_out.rds');saveRDS(bin1_h_out, '../../data/blind/bin1_h_out.rds');saveRDS(bin1_all_out, '../../data/blind/bin1_all_out.rds')
saveRDS(bin2_d_out, '../../data/blind/bin2_d_out.rds');saveRDS(bin2_h_out, '../../data/blind/bin2_h_out.rds');saveRDS(bin2_all_out, '../../data/blind/bin2_all_out.rds')
saveRDS(bin3_d_out, '../../data/blind/bin3_d_out.rds');saveRDS(bin3_h_out, '../../data/blind/bin3_h_out.rds');saveRDS(bin3_all_out, '../../data/blind/bin3_all_out.rds')
saveRDS(bin4_d_out, '../../data/blind/bin4_d_out.rds');saveRDS(bin4_h_out, '../../data/blind/bin4_h_out.rds');saveRDS(bin4_all_out, '../../data/blind/bin4_all_out.rds')
saveRDS(bin5_d_out, '../../data/blind/bin5_d_out.rds');saveRDS(bin5_h_out, '../../data/blind/bin5_h_out.rds');saveRDS(bin5_all_out, '../../data/blind/bin5_all_out.rds')
saveRDS(bin6_d_out, '../../data/blind/bin6_d_out.rds');saveRDS(bin6_h_out, '../../data/blind/bin6_h_out.rds');saveRDS(bin6_all_out, '../../data/blind/bin6_all_out.rds')
saveRDS(bin7_d_out, '../../data/blind/bin7_d_out.rds');saveRDS(bin7_h_out, '../../data/blind/bin7_h_out.rds');saveRDS(bin7_all_out, '../../data/blind/bin7_all_out.rds')
saveRDS(bin8_d_out, '../../data/blind/bin8_d_out.rds');saveRDS(bin8_h_out, '../../data/blind/bin8_h_out.rds');saveRDS(bin8_all_out, '../../data/blind/bin8_all_out.rds')
saveRDS(bin9_d_out, '../../data/blind/bin9_d_out.rds');saveRDS(bin9_h_out, '../../data/blind/bin9_h_out.rds');saveRDS(bin9_all_out, '../../data/blind/bin9_all_out.rds')
saveRDS(bin10_d_out, '../../data/blind/bin10_d_out.rds');saveRDS(bin10_h_out, '../../data/blind/bin10_h_out.rds');saveRDS(bin10_all_out, '../../data/blind/bin10_all_out.rds')
saveRDS(bin11_d_out, '../../data/blind/bin11_d_out.rds');saveRDS(bin11_h_out, '../../data/blind/bin11_h_out.rds');saveRDS(bin11_all_out, '../../data/blind/bin11_all_out.rds')
saveRDS(bin12_d_out, '../../data/blind/bin12_d_out.rds');saveRDS(bin12_h_out, '../../data/blind/bin12_h_out.rds');saveRDS(bin12_all_out, '../../data/blind/bin12_all_out.rds')
saveRDS(bin13_d_out, '../../data/blind/bin13_d_out.rds');saveRDS(bin13_h_out, '../../data/blind/bin13_h_out.rds');saveRDS(bin13_all_out, '../../data/blind/bin13_all_out.rds')
saveRDS(bin14_d_out, '../../data/blind/bin14_d_out.rds');saveRDS(bin14_h_out, '../../data/blind/bin14_h_out.rds');saveRDS(bin14_all_out, '../../data/blind/bin14_all_out.rds')
saveRDS(bin15_d_out, '../../data/blind/bin15_d_out.rds');saveRDS(bin15_h_out, '../../data/blind/bin15_h_out.rds');saveRDS(bin15_all_out, '../../data/blind/bin15_all_out.rds')
saveRDS(bin16_d_out, '../../data/blind/bin16_d_out.rds');saveRDS(bin16_h_out, '../../data/blind/bin16_h_out.rds');saveRDS(bin16_all_out, '../../data/blind/bin16_all_out.rds')
saveRDS(bin17_d_out, '../../data/blind/bin17_d_out.rds');saveRDS(bin17_h_out, '../../data/blind/bin17_h_out.rds');saveRDS(bin17_all_out, '../../data/blind/bin17_all_out.rds')
saveRDS(bin18_d_out, '../../data/blind/bin18_d_out.rds');saveRDS(bin18_h_out, '../../data/blind/bin18_h_out.rds');saveRDS(bin18_all_out, '../../data/blind/bin18_all_out.rds')
saveRDS(bin19_d_out, '../../data/blind/bin19_d_out.rds');saveRDS(bin19_h_out, '../../data/blind/bin19_h_out.rds');saveRDS(bin19_all_out, '../../data/blind/bin19_all_out.rds')





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


saveRDS(Deceased_dndsout, '../../data/blind/Deceased_dndsout.rds')
saveRDS(Hospitalized_dndsout, '../../data/blind/Hospitalized_dndsout.rds')
saveRDS(blind_dndsout, '../../data/blind/blind_dndsout.rds')

# Date (bin size: 19)

## c) Count mutation event through whole position =========
# read gene size 
df <- readRDS('../../data/blind/genesize.rds')
# Deceased
pos_d <- Deceased_dndsout$annotmuts %>% select(sampleID, chr, pos, ref, mut, gene) %>% unique() 
top_d <- count(pos_d$pos)
colnames(top_d) <- c('pos', 'N')
top_d$gene <- pos_d$gene[match(top_d$pos, pos_d$pos)]   # add gene name 

top_d$norm_by_gene <- top_d$N / df$CDS_lenght[match(top_d$gene,df$domain)]
top_d$norm_by_sample <- top_d$N / 7251
top_d$norm_by_s_by_g <- top_d$norm_by_gene/7251
top_d <- top_d %>% mutate(status='Deceased')

gene_split <- data.frame(do.call('rbind',strsplit(top_d$gene,split=':', fixed=TRUE)))
top_d <- top_d %>% mutate(domain= gene_split$X1) %>% mutate(seg=gene_split$X2)

# Hospitalized
pos_i <- Hospitalized_dndsout$annotmuts %>% select(sampleID, chr, pos, ref, mut, gene)%>% unique() 
top_i <- count(pos_i$pos)
colnames(top_i) <- c('pos', 'N')
top_i$gene <- pos_i$gene[match(top_i$pos, pos_i$pos)]   # add gene name 

top_i$norm_by_gene <- top_i$N / df$CDS_lenght[match(top_i$gene,df$domain)]
top_i$norm_by_sample <- top_i$N / 40021
top_i$norm_by_s_by_g <- top_i$norm_by_gene/40021
top_i <- top_i %>% mutate(status='Hospitalized')

gene_spliti <- data.frame(do.call('rbind',strsplit(top_i$gene,split=':', fixed=TRUE)))
top_i <- top_i %>% mutate(domain= gene_spliti$X1) %>% mutate(seg=gene_spliti$X2)

saveRDS(top_d, '../../data/blind/top_d.rds')
saveRDS(top_i, '../../data/blind/top_i.rds')
top_d <- readRDS('../../data/blind/top_d.rds')
top_i <- readRDS('../../data/blind/top_i.rds')

# mutation count: extract .xlsx
table_count <- list('Hospitalized' = top_i, 'Deceased' = top_d)
write.xlsx(table_count, file = "../../data/blind/count_postion.xlsx", rowNames=T)

write.xlsx(Deceased_dndsout, file='../../data/blind/Deceased_dndsout.xlsx')
write.xlsx(Hospitalized_dndsout, file='../../data/blind/Hospitalized_dndsout.xlsx')
write.xlsx(blind_dndsout, file='../../data/blind/blind_dndsout.xlsx')

## d) plots =======================================================================
### 1) Global dN/dS: Deceased vs Hospitalized =========
order1 <- c("ORF1ab:nsp2", "ORF1ab:nsp3", "ORF1ab:nsp4", "ORF1ab:nsp6","ORF1ab:nsp7",
            "ORF1ab:nsp8","ORF1ab:nsp9" ,"ORF1ab:nsp10","ORF1ab:nsp11" ,
            "ORF1ab:RNA-dependent-RNA-polymerase","ORF1ab:helicase" ,"ORF1ab:3'-to-5'-exonuclease",
            "ORF1ab:endoRNAse","ORF1ab:3C-like-proteinase","ORF1ab:leader-protein" ,
            "ORF1ab:2'-O-ribose-methyltransferase","S", "ORF3a","E","M","ORF6", "ORF7a" ,"ORF8","N", "ORF10" )  
order <- c('Non-synonymous','Missense','Nonsense','Truncating')

# global dnds (confidential interval)
glod <- Deceased_dndsout$globaldnds %>% na.omit() %>% mutate(status=as.factor('Deceased'))
gloa <- Hospitalized_dndsout$globaldnds %>% na.omit() %>% mutate(status=as.factor('Hospitalized'))
glot <- blind_dndsout$globaldnds %>% na.omit() %>% mutate(status=as.factor('Total'))
glo <- rbind(glod,gloa,glot)
colnames(glo) <- c('Mutation', 'Global_dNdS', 'cilow', 'cihigh', 'status')
glo['Mutation'][glo['Mutation']=='wmis'] <- 'Missense'
glo['Mutation'][glo['Mutation']=='wnon'] <- 'Nonsense'
glo['Mutation'][glo['Mutation']=='wtru'] <- 'Truncating'
glo['Mutation'][glo['Mutation']=='wall'] <- 'Non-synonymous'

# global H vs D with baseline 
pd <- position_dodge(0.5) # move them .05 to the left and right

baseline_glo <- blind_dndsout$globaldnds
# Global dN/dS: Deceased vs Hospitalized
globladNdS_all <- ggplot(glo, aes(x=factor(Mutation, levels=order), y=Global_dNdS, colour=status, group=1)) +
  geom_errorbar(aes(ymin=cilow, ymax=cihigh), width=.1, position=pd) +
  scale_color_manual('status',values = c('Total'='black','Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position=pd)+
  theme_light() +
  theme(plot.subtitle=element_text(size=14, face="italic", color="black"))+
  labs(x='Mutation', y="Global dN/dS",
       subtitle = 'Global dN/dS: Deceased vs Hospitalized')+
  theme(legend.background = element_rect(color = 'black',size = 0.2))
# add baseline
globladNdS_all <- globladNdS_all + 
  geom_hline(yintercept=1.0, size=0.2)

# table
aaa <- glo %>% acast(status ~ Mutation, value.var='Global_dNdS') %>% as.data.frame()
aaa %>% select('Non-synonymous','Missense','Nonsense','Truncating')


### 2) Global dN/dS by gene: Missense/Nonsense ===========================================
loc_d <- Deceased_dndsout$sel_loc %>% select(gene_name, wmis_loc, wnon_loc, pmis_loc) %>% mutate(status="Deceased")
colnames(loc_d) <- c('gene', 'missense', 'nonsense','p_value', "status")
loc_h <- Hospitalized_dndsout$sel_loc %>% select(gene_name, wmis_loc, wnon_loc,pmis_loc) %>% mutate(status="Hospitalized")
colnames(loc_h) <- c('gene', 'missense', 'nonsense','p_value', "status")
loc_a <- blind_dndsout$sel_loc %>% select(gene_name, wmis_loc, wnon_loc, pmis_loc) %>% mutate(status="Total")
colnames(loc_a) <- c('gene', 'missense', 'nonsense','p_value', "status")
global_loc <- rbind(loc_d,loc_h, loc_a)

pd <- position_dodge(0.1)
# H vs D: Missense
global_loc_mis <- global_loc %>%
  # subset(p_value<0.05) %>%
  subset(status %in% c('Deceased','Hospitalized')) %>% 
  ggplot(aes(x=factor(gene,levels=order1), y=missense, colour=status, group=status))+
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position=pd,aes(color=status))+
  geom_hline(yintercept = 1.0,  size=0.2)+ 
  theme_light()+
  labs(y="Global dN/dS",
       subtitle = 'Global dN/dS by gene (Missense): Hospitalized vs Deceased')+
  theme(legend.background = element_rect(color = 'black',size = 0.2),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))
# total: Missense
global_loc_mis_total <- global_loc %>%
  # subset(p_value<0.05) %>%
  subset(status == 'Total') %>% 
  ggplot(aes(x=factor(gene,levels=order1), y=missense, colour=status, group=status))+
  scale_color_manual('Status',values = c('Total' = "black"))+
  geom_point(position=pd,aes(color=status))+
  geom_hline(yintercept = 1.0, size=0.2)+ 
  theme_light()+
  labs(y="Global dN/dS",
       subtitle = 'Global dN/dS by gene(Missense): Total')+
  theme(legend.background = element_rect(color = 'black',size = 0.2),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))

# H vs D: Nonsense
global_loc_non <- global_loc %>%
  # subset(p_value<0.05) %>%
  subset(status %in% c('Deceased','Hospitalized')) %>% 
  ggplot(aes(x=factor(gene,levels=order1), y=nonsense, colour=status, group=status))+
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position=pd,aes(color=status))+
  geom_hline(yintercept = 1.0, size=0.2)+ 
  theme_light()+
  labs(y="Global dN/dS",
       subtitle = 'Global dN/dS by gene(Nonsense): Hospitalized vs Deceased')+
  theme(legend.background = element_rect(color = 'black',size = 0.2),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))
# total: Nonsense
global_loc_non_total <- global_loc %>%
  # subset(p_value<0.05) %>%
  subset(status == 'Total') %>% 
  ggplot(aes(x=factor(gene,levels=order1), y=nonsense, colour=status, group=status))+
  scale_color_manual('Status',values = c('Total' = "black"))+
  geom_point(position=pd,aes(color=status))+
  geom_hline(yintercept = 1.0, size=0.2)+ 
  theme_light()+
  labs(y="dN/dS",
       subtitle = 'Global dN/dS by gene(Nonsense): Total')+
  theme(legend.background = element_rect(color = 'black',size = 0.2),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"))  


### 3) Global H vs D + date (bin 19) =====
# H vs D: Non-synonymous
globladNdS_bin_HD <- ggplot(glo_bin %>% subset(Mutation=='Non-synonymous' & status %in% c('Deceased','Hospitalized')), aes(x=date, y=Global_dNdS, colour=status, group=1)) +
  geom_errorbar(aes(ymin=cilow, ymax=cihigh), width=.1) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="Global dN/dS",
       subtitle = 'Global dN/dS: Non-synonymous during date')

# total: Non-synonymous
globladNdS_bin_t <- ggplot(glo_bin %>% subset(Mutation=='Non-synonymous' & status %in% c('Total')), aes(x=date, y=Global_dNdS, colour=status, group=1)) +
  geom_errorbar(aes(ymin=cilow, ymax=cihigh), width=.1) +
  scale_color_manual('Status',values = c('Total' = "black"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="Global dN/dS",
       subtitle = 'Global dN/dS: Non-synonymous during date')
globladNdS_bin_t+ geom_line()

### 4) dNdS domain + date (bin 19) =====
both <- glo_gene_bin %>% subset(status %in% c('Deceased','Hospitalized'))
su <- glo_gene_bin %>% subset(status == 'Total')

# ORF1ab:helicase
dNdS_1ab_heli <- ggplot(both %>% 
                              subset(gene=='ORF1ab:helicase' & missense<10000 ), 
                              aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = 'dN/dS by gene(Missense): ORF1ab:helicase')

dNdS_1ab_heli_t <- ggplot(su %>% 
                          subset(gene=='ORF1ab:helicase' & missense<10000), 
                        aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = 'dN/dS by gene(Missense): ORF1ab:helicase')+
  ylim(c(0.5,5))+
  geom_line()

dom1 <- dNdS_1ab_heli/dNdS_1ab_heli_t
# ORF1ab:ORF1ab:3'-to-5'-exonuclease
dNdS_1ab_exo <- ggplot(both %>% 
                              subset(gene=="ORF1ab:3'-to-5'-exonuclease" & missense<10000 ), 
                          aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF1ab:3'-to-5'-exonuclease")

dNdS_1ab_exo_t <- ggplot(su %>% 
                         subset(gene=="ORF1ab:3'-to-5'-exonuclease" & missense<10000 ), 
                       aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF1ab:3'-to-5'-exonuclease")+
  geom_line()
dom2 <- dNdS_1ab_exo / dNdS_1ab_exo_t
# ORF1ab:leader-protein
dNdS_1ab_lea <- ggplot(both %>% 
                         subset(gene=="ORF1ab:leader-protein" & missense<10000 ), 
                       aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF1ab:leader-protein")
dNdS_1ab_lea_t <- ggplot(su %>% 
                         subset(gene=="ORF1ab:leader-protein" & missense<10000 ), 
                       aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF1ab:leader-protein")+
  ylim(c(0,20))+
  geom_line()
dom3 <- dNdS_1ab_lea/dNdS_1ab_lea_t
# S
dNdS_s <- ggplot(both %>% 
                         subset(gene=="S" & missense<10000), 
                       aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): S")
dNdS_s_t <- ggplot(su %>% 
                   subset(gene=="S" & missense<10000), 
                 aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): S")+
  ylim(c(0,8))+
  geom_line()
dom4 <- dNdS_s/dNdS_s_t
# ORF3a
dNdS_3a <- ggplot(both %>% 
                         subset(gene=="ORF3a" & missense<10000), 
                       aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF3a")
dNdS_3a_t <- ggplot(su %>% 
                    subset(gene=="ORF3a" & missense<10000), 
                  aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF3a")+
  ylim(c(0,10))+
  geom_line()
dom5 <- dNdS_3a/dNdS_3a_t
# E : 2022-04, 2022-07 synonymous=0 이라서 값이 안나옴 
dNdS_e <- ggplot(both %>% 
                    subset(gene=="E" & missense != 10000 ), 
                  aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): E")
dNdS_e_t <- ggplot(su %>% 
                   subset(gene=="E" & missense != 10000 ), 
                 aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): E")+
  ylim(c(0,124))+
  geom_line()
dom6 <- dNdS_e /dNdS_e_t
# M
dNdS_m <- ggplot(both %>% 
                    subset(gene=="M" & missense<10000), 
                  aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): M")+
  ylim(c(0,15))
dNdS_m_t <- ggplot(su %>% 
                   subset(gene=="M" & missense<10000 ), 
                 aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): M")+
  ylim(c(0,15))+
  geom_line()
dom7 <- dNdS_m/dNdS_m_t
# ORF7a
dNdS_orf7a <- ggplot(both %>% 
                    subset(gene=="ORF7a" & missense<10000), 
                  aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF7a")+
  ylim(c(0,17.5))
dNdS_orf7a_t <- ggplot(su %>% 
                       subset(gene=="ORF7a" & missense<10000), 
                     aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF7a")+
  ylim(c(0,17.5))+
  geom_line()
dom8 <- dNdS_orf7a/dNdS_orf7a_t
# ORF8
dNdS_orf8 <- ggplot(both %>% 
                    subset(gene=="ORF8" & missense<10000), 
                  aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF8")+
  ylim(c(0,7.5))
dNdS_orf8_t <- ggplot(su %>% 
                      subset(gene=="ORF8" & missense<10000), 
                    aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF8")+
  ylim(c(0,7.5))+
  geom_line()
dom9 <- dNdS_orf8/dNdS_orf8_t
# N
dNdS_N <- ggplot(both %>% 
                    subset(gene=="N" & missense<10000), 
                  aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): N")+
  ylim(c(0,10))
dNdS_N_t <- ggplot(su %>% 
                   subset(gene=="N" & missense<10000), 
                 aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): N")+
  ylim(c(0,10))+
  geom_line()
dom10 <- dNdS_N/dNdS_N_t
# ORF10
dNdS_orf10 <- ggplot(both %>% 
                    subset(gene=="ORF10" & missense<10000), 
                  aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF10")
dNdS_orf10_t <- ggplot(su %>% 
                       subset(gene=="ORF10" & missense<10000), 
                     aes(x=date, y=missense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Missense): ORF10")+
  geom_line()


dom11 <- dNdS_orf10/dNdS_orf10_t

p_total <- su %>% subset(p_value > 0.05) %>% as.data.frame()
p_both <- both %>% subset(p_value > 0.05) %>% as.data.frame()
write.xlsx(p_total, file = "../../data/blind_good/p_value_both.xlsx", rowNames=T)



# ORF8: Nonsense
pp <- both %>% subset( p_value > 0.05 & missense<10000)
dNdS_orf8_non <- ggplot(both %>% 
                      subset(gene=="ORF8"&  p_value<0.05 ), 
                    aes(x=date, y=nonsense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Deceased' = "red", 'Hospitalized' = "blue"))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Nonsense): ORF8")+
  ylim(c(0,30))

dNdS_orf8_non_p <- dNdS_orf8_non+geom_label(pp, mapping = aes(label= p_value), alpha=0.5, size=2)

pv <- dNdS_orf8_non/dNdS_orf8_non_p

dNdS_orf8_t_non <- ggplot(su %>% 
                        subset(gene=="ORF8"& missense<10000), 
                      aes(x=date, y=nonsense, colour=status, group=1)) +
  scale_color_manual('Status',values = c('Total'='black'))+
  geom_point(position = position_dodge(width = 0.5))+
  geom_hline(yintercept=1.0, size=0.2)+
  theme_light() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x='Date', y="dN/dS",
       subtitle = "dN/dS by gene(Nonsense): ORF8")+
  ylim(c(0,30))+
  geom_line()
dom12 <- dNdS_orf8_non/dNdS_orf8_t_non


### 5) Normalization ===========================================
all_top <- rbind(top_d,top_i)
pd <- position_dodge(width=0.5)

# top_by_s
top_by_s <- ggplot(all_top,aes(x=factor(gene,levels=order1), y=norm_by_sample, fill=status))+
  geom_point(position = position_dodge(width = 0.5), aes(color=status))+
  scale_color_manual('status', values = c('Deceased' = "red3", 'Hospitalized' = "blue3"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        legend.position = 'top',
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x="Gene", 
       y="Mutation events per genome",
       subtitle = 'Substitution events: Normalized by genome')

# top_by_gene
top_by_gene <- ggplot(all_top,aes(x=factor(gene,levels=order1), y=norm_by_gene, fill=status))+
  geom_point(position = position_dodge(width = 0.5), aes(color=status))+
  scale_color_manual('status', values = c('Deceased' = "red3", 'Hospitalized' = "blue3"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        legend.position = c(0.1,0.8),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x="Gene", 
       y="Mutation events per gene size",
       subtitle = 'Substitution events: Normalized by gene size')

# top_by_s_by_g
top_by_s_by_g <- ggplot(all_top,aes(x=factor(gene,levels=order1), y=norm_by_s_by_g, fill=status))+
  geom_point(position = position_dodge(width = 0.5), aes(color=status))+
  scale_color_manual('status', values = c('Deceased' = "red3", 'Hospitalized' = "blue3"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1.0),
        legend.position = c(0.1,0.8),
        axis.title.x=element_blank(),
        plot.subtitle=element_text(size=14, face="italic", color="black"),
        legend.background = element_rect(color = 'black',size = 0.2))+
  labs(x="Gene", 
       y="Mutation events per gene size per genome",
       subtitle = 'Substitution events: Normalized by gene size and genome')






# 5. TiTv =====
#' https://www.bioconductor.org/packages/devel/bioc/vignettes/maftools/inst/doc/maftools.html
#' TiTv part 
## a) copy TiTv, PlotTiTv function from maftools, Tumer -> SampleID ===== 
# titv
titv = function(maf, useSyn = FALSE, plot = TRUE, file = NULL){
  
  mafDat = subsetMaf(maf = maf, 
                     # query = "Variant_Type == 'SNP'", 
                     mafObj = FALSE, includeSyn = useSyn)
  # fields = "Variant_Type")
  
  if(nrow(mafDat) == 0){
    stop('No more single nucleotide variants left after filtering for SNP in Variant_Type field.')
  }
  
  #mafDat = mafDat[,.(Hugo_Symbol, Start_Position, End_Position, Reference_Allele, Tumor_Seq_Allele2, sampleID)]
  mafDat$con = paste(mafDat[,ref], mafDat[,mut], sep = '>')
  
  conv = c("T>C", "T>C", "C>T", "C>T", "T>A", "T>A", "T>G", "T>G", "C>A", "C>A", "C>G", "C>G")
  names(conv) = c('A>G', 'T>C', 'C>T', 'G>A', 'A>T', 'T>A', 'A>C', 'T>G', 'C>A', 'G>T', 'C>G', 'G>C')
  conv.class = c('Ti', 'Ti', 'Tv', 'Tv', 'Tv', 'Tv')
  names(conv.class) = c("T>C", "C>T", "T>A", "T>G", "C>A", "C>G")
  
  if(nrow(mafDat[!con %in% names(conv)]) > 0){
    warning(paste0("Non standard Ti/Tv class: ", nrow(mafDat[!con %in% names(conv)]), immediate. = TRUE))
    mafDat = mafDat[con %in% names(conv)]
  }
  
  maf.con.summary = mafDat[,.N, by = .(sampleID, con)]
  maf.con.summary$con.class = conv[as.character(maf.con.summary$con)]
  # maf.con.summary$con.class = suppressWarnings(as.character(factor(maf.con.summary$con, levels = c("A-G", "T-C", "C-T", "G-A", "A-T", "T-A", "A-C", "T-G", "C-A", "G-T", "C-G", "G-C"),
  #                                                                  labels = c("T-C", "T-C", "C-T", "C-T", "T-A", "T-A", "T-G", "T-G", "C-A", "C-A", "C-G", "C-G"))))
  
  maf.con.class.summary = maf.con.summary[,sum(N), by = .(sampleID, con.class)]
  colnames(maf.con.class.summary)[ncol(maf.con.class.summary)] = 'nVars'
  maf.con.class.summary[,fract := (nVars/sum(nVars))*100, by = .(sampleID)]
  
  maf.con.class.summary$TiTv = conv.class[maf.con.class.summary$con.class]
  maf.con.class.summary$con.class = factor(x = maf.con.class.summary$con.class,
                                           levels = c("C>A", "C>G", "C>T", "T>C", "T>A", "T>G"))
  maf.con.class.summary$TiTv = factor(x = maf.con.class.summary$TiTv, levels = c('Ti', 'Tv'))
  # maf.con.class.summary$TiTv = suppressWarnings(as.character(factor(x = maf.con.class.summary$con.class,
  #                                                                   levels = c("T>C", "C>T", "T>A", "T>G", "C>A", "C>G"), labels = c('Ti', 'Ti', 'Tv', 'Tv', 'Tv', 'Tv'))))
  
  fract.classes = data.table::dcast(data = maf.con.class.summary, formula = sampleID ~ con.class, value.var = 'fract', fill = 0, drop = FALSE)
  raw.classes = data.table::dcast(data = maf.con.class.summary, formula = sampleID ~ con.class, value.var = 'nVars', fill = 0, drop = FALSE)
  titv.summary = maf.con.class.summary[,sum(fract), by = .(sampleID, TiTv)]
  titv.summary = data.table::dcast(data = titv.summary, sampleID ~ TiTv, value.var = 'V1', fill = 0, drop = FALSE)
  
  titv.res = list(fraction.contribution = fract.classes, raw.counts = raw.classes, TiTv.fractions = titv.summary)
  
  if(plot){
    plotTiTv(res = titv.res)
  }
  
  if(!is.null(file)){
    write.table(x = fract.classes,file = paste(file, '_fraction_contribution.txt', sep = ''), quote = FALSE, row.names = FALSE, sep = '\t')
    write.table(x = raw.classes,file = paste(file, '_fraction_counts.txt', sep = ''), quote = FALSE, row.names = FALSE, sep = '\t')
    write.table(x = titv.summary,file = paste(file, '_TiTv_fractions.txt', sep = ''), quote = FALSE, row.names = FALSE, sep = '\t')
  }
  
  return(titv.res)
}
# make titv columns
get_titvCol = function(alpha = 1){
  col = c("#F44336", "#3F51B5", "#2196F3", "#4CAF50", "#FFC107", "#FF9800")
  #col = c('coral4', 'lightcyan4', 'cornflowerblue', 'lightsalmon1', 'forestgreen', 'deeppink3')
  col = grDevices::adjustcolor(col = col, alpha.f = alpha)
  names(col) = c('C>T', 'C>G', 'C>A', 'T>A', 'T>C', 'T>G')
  col
}
# plot TiTv function
plotTiTv = function(res = NULL, plotType = 'both', sampleOrder = NULL,
                    color = NULL,
                    showBarcodes = FALSE, textSize = 0.8, baseFontSize = 1,
                    axisTextSize = c(1, 1), plotNotch = FALSE){
  
  if(is.null(color)){
    col = get_titvCol(alpha = 0.8)
  }else{
    col = color
  }
  
  titv.frac = res$fraction.contribution
  titv.frac.melt = data.table::melt(data = titv.frac, id = 'sampleID')
  conv.class = c('Ti', 'Ti', 'Tv', 'Tv', 'Tv', 'Tv')
  names(conv.class) = c("T>C", "C>T", "T>A", "T>G", "C>A", "C>G")
  titv.frac.melt$TiTv = conv.class[as.character(titv.frac.melt$variable)]
  
  data.table::setDT(x = res$TiTv.fractions)
  titv.contrib = suppressMessages(data.table::melt(res$TiTv.fractions, id = 'sampleID'))
  titv.frac.melt$variable = factor(x = titv.frac.melt$variable,
                                   levels = c("T>C", "C>T", "T>A", "T>G", "C>A", "C>G"))
  
  titv.order = titv.frac.melt[,mean(value), by = .(variable)]
  titv.order = titv.order[order(V1, decreasing = TRUE)]
  orderlvl = as.character(titv.order$variable)
  titv.frac.melt$variable = factor(x = titv.frac.melt$variable, levels = orderlvl)
  
  tf = res$TiTv.fractions
  data.table::setDF(x = tf)
  rownames(tf) = tf$sampleID
  tf = tf[,-1]
  
  
  if(plotType == 'bar'){
    
    titv.frac.melt = data.table::dcast(data = titv.frac.melt, variable ~ sampleID)
    data.table::setDF(x = titv.frac.melt)
    rownames(titv.frac.melt) = titv.frac.melt$variable
    titv.frac.melt = as.matrix(titv.frac.melt[,-1])
    
    if(length(which(colSums(titv.frac.melt) == 0)) > 0){
      titv.frac.melt = titv.frac.melt[,-which(colSums(titv.frac.melt) == 0), drop = FALSE]
    }
    
    if(showBarcodes){
      par(mar = c(6, 4, 3, 3))
    }else{
      par(mar = c(2, 4, 3, 3))
    }
    
    if(!is.null(sampleOrder)){
      sampleOrder = sampleOrder[sampleOrder %in% colnames(titv.frac.melt)]
      if(length(sampleOrder) == 0){
        stop("Sample names do not match")
      }
      titv.frac.melt = titv.frac.melt[,sampleOrder]
    }
    
    b = barplot(titv.frac.melt, col = col[rownames(x = titv.frac.melt)],
                names.arg = rep("", ncol(titv.frac.melt)),
                axes = FALSE, space = 0.2, border = NA, lwd = 1.2)
    if(showBarcodes){
      axis(side = 1, at = b, labels = colnames(titv.frac.melt), tick = FALSE, font = 1, line = -1, las = 2, cex.axis = textSize)
    }
    axis(side = 2, at = seq(0, 100, 25), las = 2, font = 1, lwd = 1.2, cex.axis = axisTextSize[2])
    mtext(side = 2, text = "% Mutations", font = 1, cex = baseFontSize, line = 2.5)
    
    add_legend(x = "topright", legend = names(col), col = col, bty = "n", pch = 15, y.intersp = 0.7, text.font = 1)
    
  } else if(plotType == 'box'){
    graphics::layout(matrix(data = c(1, 2), nrow = 1), widths = c(4, 2))
    par(mar = c(2, 4, 2, 2))
    b = boxplot(value ~ variable, data = titv.frac.melt, axes = FALSE, xlab = "", ylab = "", col = col[levels(titv.frac.melt[,variable])],
                names=NA, lty = 1, staplewex = 0, pch = 16, xaxt="n", notch = plotNotch,
                outcex = 0.6, outcol = "gray70", ylim = c(0, 100), lwd = 0.6)
    axis(side = 1, at = 1:length(levels(titv.frac.melt[,variable])), labels = levels(titv.frac.melt[,variable]),
         tick = FALSE, font = 1, line = -1, cex.axis = axisTextSize[1])
    axis(side = 2, at = seq(0, 100, 25), las = 2, font = 1, lwd = 1.2, cex.axis = axisTextSize[2])
    mtext(side = 2, text = "% Mutations", font = 1, cex = baseFontSize, line = 2.5)
    
    par(mar = c(2, 1.5, 2, 2))
    b = boxplot(tf, axes = FALSE, xlab = "", ylab = "", col = 'gray70',
                names=NA, lty = 1, staplewex = 0, pch = 16, xaxt="n", notch = plotNotch,
                outcex = 0.6, outcol = "gray70", ylim = c(0, 100), lwd = 0.6)
    axis(side = 1, at = 1:2, labels = names(tf), tick = FALSE, font = 1, line = -1, cex.axis = axisTextSize[1])
    axis(side = 2, at = seq(0, 100, 25), las = 2, font = 1, lwd = 1.2, cex.axis = axisTextSize[2])
    
  } else if(plotType == 'both'){
    
    graphics::layout(mat = matrix(data = c(1, 2, 3, 3), byrow = TRUE, nrow = 2), widths = c(4, 2), heights = c(5, 4))
    par(mar = c(2, 4, 2, 1))
    plot(NA, axes = FALSE, xlim = c(0.25, 6.25), ylim = c(0, 100), xlab = NA, ylab = NA)
    abline(h = seq(0, 100, 25), v = 1:6, col = grDevices::adjustcolor(col = "gray70", alpha.f = 0.5), lty = 2, lwd = 0.6)
    b = boxplot(value ~ variable, data = titv.frac.melt, axes = FALSE, xlab = "", ylab = "", col = col[levels(titv.frac.melt[,variable])],
                names=NA, lty = 1, staplewex = 0, pch = 16, xaxt="n", notch = plotNotch,
                outcex = 0.6, outcol = "gray70", ylim = c(0, 100), lwd = 0.6, add = TRUE)
    axis(side = 1, at = 1:length(levels(titv.frac.melt[,variable])), labels = levels(titv.frac.melt[,variable]), tick = FALSE, font = 1, line = -1, cex.axis = axisTextSize[1])
    axis(side = 2, at = seq(0, 100, 25), las = 2, font = 1, lwd = 1.2, cex.axis = axisTextSize[2], line = 0.4)
    mtext(side = 2, text = "% Mutations", font = 1, cex = baseFontSize, line = 2.5)
    
    par(mar = c(2, 1.5, 2, 2))
    plot(NA, axes = FALSE, xlim = c(0, 3), ylim = c(0, 100), xlab = NA, ylab = NA)
    abline(h = seq(0, 100, 25), v = 1:2,
           col = grDevices::adjustcolor(col = "gray70", alpha.f = 0.5), lty = 2, lwd = 0.6)
    b = boxplot(tf, axes = FALSE, xlab = "", ylab = "", col = 'gray70',
                names=NA, lty = 1, staplewex = 0, pch = 16, xaxt="n", notch = plotNotch,
                outcex = 0.6, outcol = "gray70", ylim = c(0, 100), lwd = 0.6, add = TRUE, at = 1:2)
    axis(side = 1, at = 1:2, labels = names(tf), tick = FALSE, font = 1, line = -1, cex.axis = axisTextSize[1])
    axis(side = 2, at = seq(0, 100, 25), las = 2, font = 1, lwd = 1.2, cex.axis = axisTextSize[2])
    
    titv.frac.melt = data.table::dcast(data = titv.frac.melt, variable ~ sampleID)
    data.table::setDF(x = titv.frac.melt)
    rownames(titv.frac.melt) = titv.frac.melt$variable
    titv.frac.melt = as.matrix(titv.frac.melt[,-1])
    
    if(length(which(colSums(titv.frac.melt) == 0)) > 0){
      titv.frac.melt = titv.frac.melt[,-which(colSums(titv.frac.melt) == 0), drop = FALSE]
    }
    
    if(showBarcodes){
      par(mar = c(6, 4, 1, 1))
    }else{
      par(mar = c(2, 4, 1, 1))
    }
    
    if(!is.null(sampleOrder)){
      sampleOrder = sampleOrder[sampleOrder %in% colnames(titv.frac.melt)]
      if(length(sampleOrder) == 0){
        stop("Sample names do not match")
      }
      titv.frac.melt = titv.frac.melt[,sampleOrder]
    }
    
    b = barplot(titv.frac.melt, col = col[rownames(x = titv.frac.melt)], names.arg = rep("", ncol(titv.frac.melt)),
                axes = FALSE, space = 0.2, border = NA)
    if(showBarcodes){
      axis(side = 1, at = b, labels = colnames(titv.frac.melt), tick = FALSE, font = 1, line = -1, las = 2, cex.axis = textSize)
    }
    axis(side = 2, at = seq(0, 100, 25), las = 2, font = 1, lwd = 1.2, cex.axis = axisTextSize[2])
    mtext(side = 2, text = "% Mutations", font = 1, cex = baseFontSize, line = 2.5)
    
  }else{
    stop('plotType can only be bar, box or both')
  }
}

## b) set titv input data ====
# Deceased
pos_d$con <- paste(pos_d$ref, pos_d$mut, sep = '>')
setDT(pos_d) # need to change form as data.table

conv = c("T>C", "T>C", "C>T", "C>T", "T>A", "T>A", "T>G", "T>G", "C>A", "C>A", "C>G", "C>G")
names(conv) = c('A>G', 'T>C', 'C>T', 'G>A', 'A>T', 'T>A', 'A>C', 'T>G', 'C>A', 'G>T', 'C>G', 'G>C')
conv.class = c('Ti', 'Ti', 'Tv', 'Tv', 'Tv', 'Tv')
names(conv.class) = c("T>C", "C>T", "T>A", "T>G", "C>A", "C>G")


if(nrow(pos_d[!con %in% names(conv)]) > 0){
  warning(paste0("Non standard Ti/Tv class: ", nrow(pos_d[!con %in% names(conv)]), immediate. = TRUE))
  pos_d = pos_d[con %in% names(conv)]
}

setDT(pos_d, keep.rownames=TRUE, key=NULL, check.names=FALSE)
pos_d.con.summary = pos_d[,.N, by = .(sampleID, con)]
pos_d.con.summary$con.class = conv[as.character(pos_d.con.summary$con)]

pos_d.con.class.summary = pos_d.con.summary[,sum(N), by = .(sampleID, con.class)]
colnames(pos_d.con.class.summary)[ncol(pos_d.con.class.summary)] = 'nVars'
pos_d.con.class.summary[,fract := (nVars/sum(nVars))*100, by = .(sampleID)]

pos_d.con.class.summary$TiTv = conv.class[pos_d.con.class.summary$con.class]
pos_d.con.class.summary$con.class = factor(x = pos_d.con.class.summary$con.class,
                                           levels = c("C>A", "C>G", "C>T", "T>C", "T>A", "T>G"))
pos_d.con.class.summary$TiTv = factor(x = pos_d.con.class.summary$TiTv, levels = c('Ti', 'Tv'))

fract.classes = data.table::dcast(data = pos_d.con.class.summary, formula = sampleID ~ con.class, value.var = 'fract', fill = 0, drop = FALSE)
raw.classes = data.table::dcast(data = pos_d.con.class.summary, formula = sampleID ~ con.class, value.var = 'nVars', fill = 0, drop = FALSE)
titv.summary = pos_d.con.class.summary[,sum(fract), by = .(sampleID, TiTv)]
titv.summary = data.table::dcast(data = titv.summary, sampleID ~ TiTv, value.var = 'V1', fill = 0, drop = FALSE)

titv.res = list(fraction.contribution = fract.classes, raw.counts = raw.classes, TiTv.fractions = titv.summary)
# result of Deceased input form
res <- titv.res

# Hospitalized
pos_i$con <- paste(pos_i$ref, pos_i$mut, sep = '>')

conv = c("T>C", "T>C", "C>T", "C>T", "T>A", "T>A", "T>G", "T>G", "C>A", "C>A", "C>G", "C>G")
names(conv) = c('A>G', 'T>C', 'C>T', 'G>A', 'A>T', 'T>A', 'A>C', 'T>G', 'C>A', 'G>T', 'C>G', 'G>C')
conv.class = c('Ti', 'Ti', 'Tv', 'Tv', 'Tv', 'Tv')
names(conv.class) = c("T>C", "C>T", "T>A", "T>G", "C>A", "C>G")
setDT(pos_i) # need to change form as data.table

if(nrow(pos_i[!con %in% names(conv)]) > 0){
  warning(paste0("Non standard Ti/Tv class: ", nrow(pos_i[!con %in% names(conv)]), immediate. = TRUE))
  pos_i = pos_i[con %in% names(conv)]
}

setDT(pos_i, keep.rownames=TRUE, key=NULL, check.names=FALSE)
pos_i.con.summary = pos_i[,.N, by = .(sampleID, con)]
pos_i.con.summary$con.class = conv[as.character(pos_i.con.summary$con)]

pos_i.con.class.summary = pos_i.con.summary[,sum(N), by = .(sampleID, con.class)]
colnames(pos_i.con.class.summary)[ncol(pos_i.con.class.summary)] = 'nVars'
pos_i.con.class.summary[,fract := (nVars/sum(nVars))*100, by = .(sampleID)]

pos_i.con.class.summary$TiTv = conv.class[pos_i.con.class.summary$con.class]
pos_i.con.class.summary$con.class = factor(x = pos_i.con.class.summary$con.class,
                                           levels = c("C>A", "C>G", "C>T", "T>C", "T>A", "T>G"))
pos_i.con.class.summary$TiTv = factor(x = pos_i.con.class.summary$TiTv, levels = c('Ti', 'Tv'))

fract.classes = data.table::dcast(data = pos_i.con.class.summary, formula = sampleID ~ con.class, value.var = 'fract', fill = 0, drop = FALSE)
raw.classes = data.table::dcast(data = pos_i.con.class.summary, formula = sampleID ~ con.class, value.var = 'nVars', fill = 0, drop = FALSE)
titv.summary = pos_i.con.class.summary[,sum(fract), by = .(sampleID, TiTv)]
titv.summary = data.table::dcast(data = titv.summary, sampleID ~ TiTv, value.var = 'V1', fill = 0, drop = FALSE)

# result of Hospitalized input form
alive.titv.res = list(fraction.contribution = fract.classes, raw.counts = raw.classes, TiTv.fractions = titv.summary)

## c) output of titv: H and D -> extract as png files ====
# Deceased
titvplot_d <- plotTiTv(res = titv.res)
# Hospitalized
titvplot_a <- plotTiTv(res = alive.titv.res)

# 6. pptx =======================================================================
## a) blind_landscape.pptx =====
plot_file <- read_pptx() %>%
  # add_slide() %>% ph_with(dml(ggobj = pall_sub), location=ph_location_type(type="body"))%>%
  # add_slide() %>% ph_with(dml(ggobj = pall_del), location=ph_location_type(type="body")) %>% 
  # add_slide() %>% ph_with(dml(ggobj = pall_ins), location=ph_location_type(type="body")) %>%
  # add_slide() %>% ph_with(dml(ggobj = psub_ex), location=ph_location_type(type="body")) %>%
  # add_slide() %>% ph_with(dml(ggobj = pdel_ex), location=ph_location_type(type="body")) %>%
  # add_slide() %>% ph_with(dml(ggobj = pins_ex), location=ph_location_type(type="body")) %>% 
  # add_slide() %>% ph_with(dml(ggobj = plot3), location=ph_location_type(type="body")) %>%
  # add_slide() %>% ph_with(dml(ggobj = p5), location=ph_location_type(type="body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = plot1), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = plot2), location=ph_location_type(type="body")) 
print(plot_file, target = "../../data/blind_good/plot_blind_landscape_position.pptx")  

## b) dNdS.pptx =====
plot_file <- read_pptx() %>%
  add_slide() %>% ph_with(dml(ggobj = globladNdS_all), location=ph_location_type(type="body"))%>%
  add_slide() %>% ph_with(dml(ggobj = global_loc_mis), location=ph_location_type(type="body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = global_loc_mis_total), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = global_loc_non), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = global_loc_non_total), location=ph_location_type(type="body")) 
print(plot_file, target = "../../data/blind_good/plot_blind_dnds.pptx")  

## c) Normalize.pptx =====
plot_file <- read_pptx() %>%
  add_slide() %>% ph_with(dml(ggobj = top_by_s), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = top_by_gene), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = top_by_s_by_g), location=ph_location_type(type="body"))
print(plot_file, target = "../../data/blind_good/plot_blind_normalization.pptx")  

## d) dNdS_domain.pptx ====
plot_file <- read_pptx() %>%
  add_slide() %>% ph_with(dml(ggobj = globladNdS_bin_HD), location=ph_location_type(type="body"))%>%
  add_slide() %>% ph_with(dml(ggobj = globladNdS_bin_t), location=ph_location_type(type="body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = dNdS_1ab_heli_t), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = dNdS_1ab_exo_t), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = dNdS_1ab_lea_t), location=ph_location_type(type="body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = dNdS_s_t), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = dNdS_3a_t), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = dNdS_e_t), location=ph_location_type(type="body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = dNdS_m_t), location=ph_location_type(type="body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = dNdS_orf7a_t), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = dNdS_orf8_t), location=ph_location_type(type="body")) %>%
  add_slide() %>% ph_with(dml(ggobj = dNdS_N_t), location=ph_location_type(type="body")) %>% 
  add_slide() %>% ph_with(dml(ggobj = dNdS_orf10_t), location=ph_location_type(type="body"))%>% 
  add_slide() %>% ph_with(dml(ggobj = dNdS_orf8_t_non), location=ph_location_type(type="body"))
print(plot_file, target = "../../data/blind_good/plot_blind_dnds_domain_ver3.pptx")  

plot_file <- read_pptx() %>%
  add_slide() %>% ph_with(dml(ggobj = map_domain2), location=ph_location_type(type="body"))
print(plot_file, target = "../../data/blind_good/domain.pptx")  

