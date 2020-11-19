##########################################################################################################
# Summarizes E. coli origina/duplicate pair data in DEQ E. coli database
# Jason Williams
# Last Update: 11-18-2020
#########################################################################################################

# required libraries
library(odbc)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(stringr)

# connect to EDAS SQL database--------------------------------------------------------------------------

EDAS_con <-dbConnect(odbc::odbc(), "EDAS2_SQLPROD", encoding = "latin1")

# NOTE: This script reads data from EDAS directly into R. 
# connceting to EDAS SQL db directly from R requires 1) 'read' permission from db administratior, and
# 2) setting up an ODBC connection to EDAS on your computer (see IT for assistance)

# Alternatively, the same data could be read into R by 1) opening IDASA2, Ecoli > EDAS Ecoli View > 
# selecting the relevant view, 3) export view data to csv, 4) read csv file into R. Required 
# views in IDASA 2 are listed below.

# get E. coli data from EDAS----------------------------------------------------------------------------

# all E. coli field duplicate pairs & RPD (same as IDASA2 EDAS Ecoli View #0713)
dups <-dbGetQuery(EDAS_con, "select * from dbo.View_Ecoli_regular_replicate_difference")

str(dups)


# format------------------------------------------------------------------------------------------------

dups_formatted <-
  dups %>%
  
  # calculate RPD
  mutate(RPD =  ( (`Sample-Routine result` -  `Quality Control Sample-Field Replicate result`) /
                    ( (`Sample-Routine result` +  `Quality Control Sample-Field Replicate result`)/ 2) * 100)) %>%
  
  # assign result range category to original result 
  mutate(`Result Range` = ifelse(`Sample-Routine result` <= 250, "0-250", "> 250")) %>%
  mutate(below_126 = ifelse(`Sample-Routine result` <= 126, "0-126", ">126"))

str(dups_formatted)

# Exploratory data plots---------------------------------------------------------------------------------

# plot original result vs RPD-
RPD_plot <-
  dups_formatted %>%
  ggplot(aes(x = `Sample-Routine result`, y = RPD)) +
  geom_point() +
  labs(x = " Original Sample Result (mpn/100 mL)", y = "RPD (%)") +
  theme_bw()
RPD_plot

# plot original result vs absolute difference
diff_plot <-
  dups_formatted %>%
  ggplot(aes(x = `Sample-Routine result`, y = `Absolute Difference (Sample-Routine result - Quality Control Sample-Field Replicate result)`)) +
  geom_point() +
  labs(x = "Original Sample Result (mpn/100 mL)", y = "|original - duplicate| (mpn/100 ml)") +
  theme_bw()
diff_plot


# summarize absolute diff, RPD based on whether original was > 126 mpn/100 ml-------------------------------

dups_formatted %>%
  group_by(`below_126`) %>%
  summarize(n = n(),
            min_abs_diff = min(`Absolute Difference (Sample-Routine result - Quality Control Sample-Field Replicate result)`),
            max_abs_diff = max(`Absolute Difference (Sample-Routine result - Quality Control Sample-Field Replicate result)`),
            mean_abs_diff = mean(`Absolute Difference (Sample-Routine result - Quality Control Sample-Field Replicate result)`),
            med_abs_diff = median(`Absolute Difference (Sample-Routine result - Quality Control Sample-Field Replicate result)`),
            min_RPD = min(RPD),
            max_RPD = max(RPD),
            mean_RPD = mean(RPD))
