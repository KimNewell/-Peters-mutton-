installed.packages()
library(tidyverse)
library(knitr)

df <- read_csv("~/Documents/Data School/Peters mutton/data/raw data/bcfa_C17_fa(4).csv")
df <- bcfa_C17_fa_4_
df
#Mutton Flavour
#Fatty acids including MOA, EOA and MNA accumulate in lamb meat and can make meat from older 
#animals taste unpleasant. Another fatty acid, C17:0, also accumlates with age although 
#is not responsible for the unpleasant 'mutton flavour'. C17 is easier to measure than MOA, 
#EOA or MNA; this study investigates whether C17 could be a useful 'proxy' for indicating 
#how 'muttony' meat is likely to taste. 

#The data already seems quite tidy - but simplify more
#by arranging by year and putting all fatty acid measurements in one column.

df2 <- df %>% gather(key = fatty_acid, value = measurement, -Year)
df2
#basic density plot
ggplot(data = df2, aes(x = measurement, fill = fatty_acid)) +
  geom_density(alpha=0.2)

#addedlog10 to the x scale to stretch out the graphs
ggplot(data = df2, aes(x = measurement, fill = fatty_acid)) +
  geom_density(alpha=0.2) + scale_x_log10()

#added facet wrap and separated into the two years
ggplot(data = df2, aes(x = measurement, fill = fatty_acid)) +
  geom_density(alpha=0.2) + scale_x_log10() + facet_wrap(~ Year)
