#1. Load and install required packages
library(tidyverse)
library(janitor)
library(readxl)
library(skimr)
library(stringr)
library(scales)
library(ggplot2)
library(writexl)

#2. Import Data
setwd('/Users/elizabethbaidoo/Documents/R programming course/ Elizabeth_Baidoo_Capstone/Datasets/Elizabeth_Baidoo_CapstoneCleaned_Dataset.xlsx')
setwd('/Users/elizabethbaidoo/Documents/R programming course/ Elizabeth_Baidoo_Capstone/Datasets')
AMR_data<-read_excel('Elizabeth_Baidoo_CapstoneCleaned_Dataset.xlsx')

#3. Data Exploration
dim(AMR_data)  #rows and columns
nrow(AMR_data) #number of rows
ncol(AMR_data) #number of columns
length(AMR_data) #length of data
names(AMR_data) #column names
glimpse(AMR_data) #viewing the structure
skim(AMR_data)
str(AMR_data)

#4. Data cleaning and PreProcessing 
names(AMR_data)
colSums(is.na(AMR_data))
colSums(AMR_data == "", na.rm = TRUE)
lapply(AMR_data[8:14], unique) #looking at the content of the antibiotic columns
lapply(AMR_data[2:7], unique) #looking at the content of the other columns

#a. Standardizing CULTURE column
AMR_data <- AMR_data %>%
  mutate(CULTURE = str_to_lower(CULTURE))
table(AMR_data$CULTURE, useNA = "ifany") 

AMR_data <- AMR_data %>%
  mutate(SPECIMEN = str_to_lower(SPECIMEN))
table(AMR_data$SPECIMEN, useNA = "ifany") 

#b. Removing empty column
AMR_clean <- AMR_data %>%
  select(-`...6`)
names(AMR_clean)
view(AMR_clean)

#c. Standardizing column names
AMR_cleanr<-AMR_clean %>% 
  clean_names()
names(AMR_cleanr)

#d. Check for duplicates
duplicated(AMR_cleanr)
sum(duplicated(AMR_cleanr))
sum(duplicated(AMR_cleanr$s_n))

AMR_cleanr %>% 
  filter(duplicated(.))

AMR_cleanr %>%
  count(s_n) %>% 
  filter(n > 1)

#e. Clearing duplicates
AMR_cleanr <-AMR_cleanr %>% 
  distinct()

sum(duplicated(AMR_cleanr))
sum(duplicated(AMR_cleanr$s_n))
dim(AMR_data)  #rows and columns
view(AMR_cleanr)

#5. PRELIMINARY ANALYSIS
#a. Distribution of microorganisms
culture_summary<-AMR_cleanr %>% 
  count(culture) %>% 
  mutate(
    percentage = n/sum(n)*100
  )
culture_summary

#Visualize
ggplot(culture_summary,aes(x = culture, y = n))+
  geom_col()+ 
  geom_text(aes(label = paste0(n, "(", round(n/sum(n)*100,1), "%)")),
            vjust = -0.3
  )+
  labs(
    title = 'Culture distribution',
    x = 'Culture result',
    y = 'Number of specimen'
  )+
  theme(panel.grid = element_blank()) 

#b. Creating the positive-culture dataset

positive_cultures<-AMR_cleanr %>% 
  filter(culture == "positive")
positive_cultures
nrow(positive_cultures)

#c. Describing positive-culture dataset
#i. Distribution of microorganisms present
microbe_summary<-positive_cultures %>% 
  count(organism, sort = TRUE) %>% 
  mutate(
    percentage = n/sum(n)*100
  )
microbe_summary

#Visualize using a horizontal bar chart

ggplot(microbe_summary,aes(x = reorder(organism,n), y = n))+
  geom_col()+ 
  coord_flip(clip = 'off')+
  geom_text(aes(label = paste0(n, "(", round(n/sum(n)*100,1), "%)")),
            hjust = -0.3
  )+
  labs(
    title = 'Culture distribution',
    x = 'Culture result',
    y = 'Number of specimen'
  )+
  theme_minimal()

ggplot(microbe_summary, aes(x = reorder(organism, n), y = n)) +
  geom_col() +
  coord_flip(clip = "off") +
  geom_text(
    aes(label = paste0(n, " (", round(n/sum(n)*100, 1), "%)")),
    hjust = -0.3
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Culture distribution",
    x = "Culture result",
    y = "Number of specimen"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(5.5, 40, 5.5, 5.5)
  )

#ii. Distribution of microorganisms by sex
sex_microbe<-positive_cultures %>% 
  count(organism,sex) %>% 
  group_by(organism) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  ungroup()
sex_microbe
write_xlsx(sex_microbe, 'Gender_Distribution_capstone.xlsx')

#visualize using bar chart
ggplot(sex_microbe,aes(x = organism, fill = sex, y = n))+
  geom_col(position = 'dodge')+ 
  geom_text(aes(label = paste0(n, "(",round(percent), "%)")),
            position = position_dodge(width = 0.9),
            hjust = -0.1
  )+
  coord_flip()+
  labs(
    title = 'Distribution of microorganisms by sex',
    x = 'Microbial Isolates',
    y = 'Number of isolates',
    fill = 'sex'
  )+
  theme_minimal()


#iii. Distribution of organisms by ward
ward_microbes <-positive_cultures %>% 
  count(organism, ward_clinic) %>% 
  group_by(organism) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  ungroup()
ward_microbes
write_xlsx(ward_microbes, 'Ward_distribution_capstone.xlsx')




#6. AST Analysis
#a. Separate organisms into groups as Gram-negatives and Gram-positives
#i. Group gram-negatives
gram_negative<-positive_cultures %>% 
  filter(
    organism %in% c(
      'E.coli',
      'Klebsiella sp',
      'Proteus spp',
      'Pseudomonas sp'
    )
  )
nrow(gram_negative) 
view(gram_negative)

#ii. Group gram-positives
gram_positive<-positive_cultures %>% 
  filter(
    organism %in% c(
      'Staphylococcus aureus',
      'Staphylococcus epidermidis',
      'Staphylococcus saprophyticus',
      'Enterococcus'
    )
  )
nrow(gram_positive)
view(gram_positive)

#iii. Group Candida
candida<-positive_cultures %>% 
  filter(organism == 'Candida spp')
nrow(candida)  
view(candida)
names(AMR_cleanr)  

#b. Checking the compeleteness of the AST data
#i. Group antibiotics
antibiotics<-c(
  'amoxycillin_clavulanate',
  'ceftazidime',
  'ciprofloxacin',
  'meropenem',
  'nitrofurantoin',
  'pip_tazobactam',
  'ceftriaxone'
)
view(antibiotics) 

#ii. Establising the values for each antibiotic  
lapply(AMR_cleanr[antibiotics], unique) #looking at the content of the antibiotic columns

#iii. Calculating the AST data availability of each antibiotic
ast_availability<- gram_negative %>% 
  summarise(
    across(
      all_of(antibiotics),
      ~sum(!is.na(.))
    )
  )
view(ast_availability) 

####b. Creating a six-panel antibiotic because ceftriaxone has a different count
antibiotics_6<- c('amoxycillin_clavulanate',
                  'ceftazidime',
                  'ciprofloxacin',
                  'meropenem',
                  'nitrofurantoin',
                  'pip_tazobactam'
)
view(antibiotics_6)

#Creating a three-panel of gram_negatives
gram_negative_r <- positive_cultures %>% 
  filter(
    organism %in% c(
      "Klebsiella sp",
      "Proteus spp",
      "Pseudomonas sp"
    )
  )
view(gram_negative_r)

##c. Creating a panel for only ceftriaxone
ceftriaxone_data<-positive_cultures %>% 
  filter(!is.na(ceftriaxone)) %>% 
  count(organism, sort = TRUE)
ceftriaxone_data

#d. Conversion of six-antibiotic data from wide to long format

AST_longer <- gram_negative_r %>% 
  pivot_longer(
    cols = all_of(antibiotics_6),
    names_to = 'antibiotic',
    values_to = 'result')
view(AST_longer)

#e.Factor the susceptibility results
AST_longer <- AST_longer %>%
  mutate(
    result = factor(
      result,
      levels = c("S", "I", "R"),
      labels = c("Susceptible", "Intermediate", "Resistant")
    )
  )
table(AST_longer$result, useNA = "ifany")

#f. AST analysis
AST_summary<-AST_longer %>% 
  count(antibiotic,result) %>% 
  group_by(antibiotic) %>% 
  mutate(percentage = n/sum(n)*100) %>% 
  ungroup()
table(AST_summary)
write_xlsx(AST_summary, 'AST_Summary_capstone.xlsx')

#i. For table
AST_summary_wide <- AST_summary %>%
  select(antibiotic, result, n, percentage) %>%
  pivot_wider(
    names_from = result,
    values_from = c(n, percentage),
    names_glue = "{result}_{.value}"
  )
AST_summary_wide
write_xlsx(AST_summary_wide, 'AST_Summary_Wide_capstone.xlsx')

#ii. Calculate resistance percentages
resistance_summary <- AST_longer %>%
  group_by(antibiotic) %>%
  summarise(
    tested = n(),
    susceptible = sum(result == "Susceptible"),
    intermediate = sum(result == "Intermediate"),
    resistant = sum(result == "Resistant"),
    susceptibility_percent = round(susceptible / tested * 100,1),
    intermediate_percent = round(intermediate / tested * 100,1),
    resistance_percent = round(resistant / tested * 100,1),
    .groups = "drop"
  )
view(resistance_summary)
write_xlsx(resistance_summary, 'Resistance_Summary_capstone.xlsx')

#iii. Standardize antibiotic names
resistance_summary <- resistance_summary %>%
  mutate(
    antibiotic = recode(
      antibiotic,
      amoxycillin_clavulanate = "Amoxycillin-clavulanate",
      ceftazidime = "Ceftazidime",
      ciprofloxacin = "Ciprofloxacin",
      meropenem = "Meropenem",
      nitrofurantoin = "Nitrofurantoin",
      pip_tazobactam = "Piperacillin-tazobactam"
    )
  )

#iv. Creating clean table
resistance_table <- resistance_summary %>%
  mutate(
    `Susceptible n (%)` = paste0(
      susceptible, " (", round(susceptibility_percent, 1), "%)"
    ),
    `Intermediate n (%)` = paste0(
      intermediate, " (", round(intermediate_percent, 1), "%)"
    ),
    `Resistant n (%)` = paste0(
      resistant, " (", round(resistance_percent, 1), "%)"
    )
  ) %>%
  select(
    antibiotic,
    tested,
    `Susceptible n (%)`,
    `Intermediate n (%)`,
    `Resistant n (%)`
  )

View(resistance_table)
write_xlsx(resistance_table, 'resistance_table_capstone.xlsx')

#Visualize resistance percentage
library(ggplot2)

ggplot(resistance_summary,
       aes( x = reorder(antibiotic, resistance_percent), y = resistance_percent )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(resistance_percent, 1), "%")),
    hjust = -0.1
  ) +
  coord_flip() +
  labs(
    title = "Antimicrobial Resistance Among Gram-negative Isolates",
    x = "Antibiotic",
    y = "Resistance (%)"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )

#Visualize the susceptibilities of the isolates
#Creating the plot
AST_plot <- resistance_summary %>%
  select(
    antibiotic,
    susceptibility_percent,
    intermediate_percent,
    resistance_percent
  ) %>%
  pivot_longer(
    cols = c(
      susceptibility_percent,
      intermediate_percent,
      resistance_percent
    ),
    names_to = "result",
    values_to = "percent"
  ) %>%
  mutate(
    result = recode(
      result,
      susceptibility_percent = "Susceptible",
      intermediate_percent = "Intermediate",
      resistance_percent = "Resistant"
    )
  )

#Inserting the plot in. ggplot
ggplot(
  AST_plot,
  aes(
    x = antibiotic,
    y = percent,
    fill = result
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  labs(
    title = "Antimicrobial Susceptibility Profile",
    x = "Antibiotic",
    y = "Percentage of isolates",
    fill = "AST result"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )

# Calculate percentage resistance for each organism-antibiotic combination
resistance_by_microbe <- gram_negative_r %>%
  pivot_longer(
    cols = c(
      amoxycillin_clavulanate,
      pip_tazobactam,
      nitrofurantoin,
      ceftazidime,
      ciprofloxacin,
      meropenem
    ),
    names_to = "antibiotic",
    values_to = "result"
  ) %>%
  filter(!is.na(result)) %>%
  group_by(organism, antibiotic) %>%
  summarise(
    tested = n(),
    resistant = sum(result == "R"),
    resistance_percent = round((resistant / tested) * 100, 1),
    .groups = "drop"
  )


# Plot
ggplot(
  resistance_by_microbe,
  aes(
    x = antibiotic,
    y = resistance_percent,
    fill = organism
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(resistance_percent, "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Percentage resistance among microbes",
    x = "Antibiotic",
    y = "Resistance (%)",
    fill = "Microorganism"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


#v. Overall resistance across all the six antibiotics
overall_AST <- AST_longer %>%
  summarise(
    total_tests = n(),
    susceptible = sum(result == "Susceptible"),
    intermediate = sum(result == "Intermediate"),
    resistant = sum(result == "Resistant")
  ) %>%
  mutate(
    susceptible_percent = susceptible / total_tests * 100,
    intermediate_percent = intermediate / total_tests * 100,
    resistant_percent = resistant / total_tests * 100
  )

view(overall_AST)
write_xlsx(overall_AST, 'Overall_resistance_Summary_capstone.xlsx')


#vi. Analysis of ceftriaxone
Ecoli_ceftriaxone <- positive_cultures %>%
  filter(organism == "E.coli")

nrow(Ecoli_ceftriaxone)

ceftriaxone_summary <- Ecoli_ceftriaxone %>%
  count(ceftriaxone) %>%
  mutate(
    percentage = n / sum(n) * 100
  )
ceftriaxone_summary

#Ceftriaxone resistance
ceftriaxone_resistance <- Ecoli_ceftriaxone %>%
  summarise(
    tested = sum(!is.na(ceftriaxone)),
    susceptible = sum(ceftriaxone == "S", na.rm = TRUE),
    intermediate = sum(ceftriaxone == "I", na.rm = TRUE),
    resistant = sum(ceftriaxone == "R", na.rm = TRUE)
  ) %>%
  mutate(
    susceptible_percent = round(susceptible / tested * 100,1),
    intermediate_percent = round(intermediate / tested * 100,1),
    resistant_percent = round(resistant / tested * 100,1),
  )
ceftriaxone_resistance
write_xlsx(ceftriaxone_resistance, 'Ceftriaxone_resistance_capstone.xlsx')


#Ceftriaxone resistance summary table
ceftriaxone_table <- ceftriaxone_resistance %>%
  transmute(
    Antibiotic = "Ceftriaxone",
    Tested = tested,
    `Susceptible n (%)` = paste0(
      susceptible, " (", susceptible_percent, "%)"
    ),
    `Intermediate n (%)` = paste0(
      intermediate, " (", intermediate_percent, "%)"
    ),
    `Resistant n (%)` = paste0(
      resistant, " (", resistant_percent, "%)"
    )
  )

View(ceftriaxone_table)
write_xlsx(ceftriaxone_table, 'Ceftriaxone_resistance_table_capstone.xlsx')


#Graph of ceftriaxone resistance
ggplot(ceftriaxone_summary,
       aes( x = ceftriaxone, y = percentage)
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    vjust = -0.3
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Ceftriaxone susceptibility among E. coli isolates",
    x = "Ceftriaxone result",
    y = "Percentage of E. coli isolates"
  ) +
  theme_minimal()

#7. Flagging MDR(Multidrug resistant isolates)
#a. Multidrug Resistance (MDR) Classification
# Criteria: Magiorakos et al. (2012) - Non-susceptible (I or R) to >= 3 drug classes
mdr_processed_data <- gram_negative %>%
  mutate(
    # Class 1: Penicillins / Beta-lactamase inhibitors
    class_penicillins = if_else(pip_tazobactam %in% c("R", "I")|amoxycillin_clavulanate %in% c("R", "I"), 1, 0, missing = 0),
    
    # Class 2: Extended-Spectrum Cephalosporins
    class_cephalosporins = if_else(ceftriaxone %in% c("R", "I") | ceftazidime %in% c("R", "I"), 1, 0, missing = 0),
    
    # Class 3: Fluoroquinolones
    class_fluoroquinolones = if_else(ciprofloxacin %in% c("R", "I"), 1, 0, missing = 0),
    
    # Class 4: Carbapenems
    class_carbapenems = if_else(meropenem %in% c("R", "I"), 1, 0, missing = 0),
  ) %>%
  # Calculate total resistant classes and flag binary MDR status
  mutate(resistant_class_count = class_penicillins + class_cephalosporins + 
           class_fluoroquinolones + class_carbapenems,
         mdr_status = if_else(resistant_class_count >= 3, "MDR", "Non-MDR"),
         mdr_status = factor(mdr_status, levels = c("Non-MDR", "MDR"))
  )
view(mdr_processed_data)  

#b. MDR summary
mdr_summary <- mdr_processed_data %>%
  count(mdr_status) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    result = paste0(n, " (", percentage, "%)")
  ) %>%
  select(mdr_status, result)
mdr_summary
write_xlsx(mdr_summary, 'MDR_summary_capstone.xlsx')

#c. Summairze MDR by antimicrobial class
resistance_class_summary <- mdr_processed_data %>%
  summarise(
    Penicillins = sum(class_penicillins),
    Cephalosporins = sum(class_cephalosporins),
    Fluoroquinolones = sum(class_fluoroquinolones),
    Carbapenems = sum(class_carbapenems)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "antibiotic_class",
    values_to = "n"
  ) %>%
  mutate(
    percentage = round(n / nrow(mdr_processed_data) * 100, 1)
  )

resistance_class_summary
write_xlsx(resistance_class_summary, 'MDR_resistance_class_capstone.xlsx')

#d. Distribution of the number of resistant classes
class_count_summary <- mdr_processed_data %>%
  count(resistant_class_count) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  )

class_count_summary
write_xlsx(class_count_summary, 'Ceftriaxone_resistance_capstone.xlsx')

ggplot(class_count_summary,
       aes(x = n,
           y = factor(resistant_class_count))) +
  
  geom_col() +
  
  geom_text(
    aes(label = paste0(n, " (", percentage, "%)")),
    hjust = -0.15,
    size = 4
  ) +
  
  labs(
    title = "Distribution of Isolates by Number of Resistant Antibiotic Classes",
    x = "Number of Isolates",
    y = "Number of Resistant Antibiotic Classes"
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )





































