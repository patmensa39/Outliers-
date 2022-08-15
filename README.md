# Outliers-
# Detecting outliers 
### Tools to detect outliers are scatterplot, histogram, boxplot 
library(tidyverse)
library(ggplot2)
### Using your own names for the columns 
names <- c('DRG', 'ProviderID', 'Name','Address', 'City', 'State', 'ZipCode',
           'Region', 'Discharges', 'AverageCharges', 'AverageTotalPayments', 
           'AverageMedicarePayments')
types <- 'ccccccccinnn'

### Supplying this to the new  data 
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', 
                      col_names = names, skip = 1, col_types = types)

glimpse(inpatient)
attach(inpatient)

## Histogram 
### In base R 
hist(inpatient$AverageCharges)
### In ggplot
ggplot(data = inpatient) + 
  geom_histogram(mapping = aes(x = AverageCharges))

## Boxplot 
### In base R 
boxplot(inpatient$AverageCharges, horizontal = TRUE)
### In piping
inpatient %>% as_tibble() %>%
select(AverageCharges) %>% boxplot(horizontal = TRUE)
### In ggplot2
ggplot(data = inpatient) + 
  geom_boxplot(mapping = aes("Charges", AverageCharges))

### Filtering the data 
inpatient %>% 
  filter(AverageCharges > 750000)

### Removing these data points and plotting them
inpatient2 <- inpatient %>% 
  filter(AverageCharges < 750000) 
boxplot(inpatient2$AverageCharges, horizontal = TRUE)

ggplot(data = inpatient2) + 
  geom_boxplot(mapping = aes("Charges", AverageCharges))

inpatient %>% filter(AverageCharges > 500000) %>% 
  ggplot() + geom_point(mapping = aes(DRG, AverageCharges)) + 
  theme(axis.title.x = element_text(angle = 90, vjust = 1, hjust = 1))

pacman::p_load(pacman,rio, tidyverse)
## loading and preparing the data (Google search data)
Google.search.data <- import("StateData.xlsx") %>% as_tibble() %>% 
  select(state_code, psychRegions, instagram:modernDance) %>% 
  mutate(psychRegions = as.factor(psychRegions)) %>%
  print()
view(Google.search.data)


### SCATTERPLOTS ### 
## this shows the plot of all associations 
Google.search.data %>% select(instagram:modernDance) %>% plot() 

#Bivariate scatterplot with default
Google.search.data %>% select(scrapbook,modernDance) %>% plot()
#Another way . Remember this will select variables from scrapbook to modernDance
Google.search.data %>% select(scrapbook:modernDance) %>% plot()

## Bivariate scatter with options 
#pch means plotting character
Google.search.data %>% select(scrapbook, modernDance) %>%
  plot(main = "Scatterplot of Ghana with Nana Addo and Adwoa Sarfo", 
       xlab = "scrapbook", ylab = "Modern dance", 
       col = rainbow(6), pch = 20) 

?pch
# Adding a fit linear regressing line (y~x) 
Google.search.data %>% lm(modernDance~scrapbook, data = .) %>% abline()


## identifying outliers 
Google.search.data %>% select(state_code, scrapbook) %>% 
  filter(scrapbook > 4)%>% print() 

#Plotting the Bivariate scatterplot without outliers 

Google.search.data %>% select(scrapbook,modernDance) %>% 
  filter(scrapbook < 4) %>% 
  plot(main = "Scatterplot of Ghana without Nana Addo and Adwoa Sarfo ", 
       xlab = "scrapbook", ylab = "Modern dance", 
       col = rainbow(6), pch = 20)

# Adding a fit without outliers 
Google.search.data %>% filter(scrapbook < 4) %>% 
  lm(modernDance~scrapbook, data = .) %>% abline()


