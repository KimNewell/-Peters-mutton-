installed.packages()
library(tidyverse)


#Mutton Flavour
#Fatty acids including MOA, EOA and MNA accumulate in lamb meat and can make meat from older 
#animals taste unpleasant. Another fatty acid, C17:0, also accumlates with age although 
#is not responsible for the unpleasant 'mutton flavour'. C17 is easier to measure than MOA, 
#EOA or MNA; this study investigates whether C17 could be a useful 'proxy' for indicating 
#how 'muttony' meat is likely to taste. 

df <- read_csv("~/Documents/Data School/Peters mutton/data/raw data/bcfa_C17_fa(4).csv")
df <- bcfa_C17_fa_4_
df
str(df)

#The data already seems quite tidy - it is
#a tibble of 212 rows and 5 columns - but we did simplify it more
#by arranging by year and putting all fatty acid 
#measurements in one column.

df2 <- df %>% gather(key = fatty_acid, value = measurement, -Year)
df2

#basic density plot
mutton_densityplot <- ggplot(data = df2, aes(x = measurement, fill = fatty_acid)) +
  geom_density(alpha=0.2)
mutton_densityplot

#Cant see very much because of the scale - just that C17.0 has a 
#huge peak at a relatively low level compared to the rest of 
#the data. Why? Need to close in a bit so:
#addedlog10 to the x scale to stretch out the plots.

mutton_densityplot2 <- ggplot(data = df2, aes(x = measurement, fill = fatty_acid)) +
  geom_density(alpha=0.2) + scale_x_log10()
mutton_densityplot2

#In discussion with Maddy we now noticed two peaks in the C17.0 results.
#What does this mean?
#we added facet wrap to separate the two years of results 
#to see if that cleared anything up.


mutton_densityplot2Year <- ggplot(data = df2, aes(x = measurement, fill = fatty_acid)) +
  geom_density(alpha=0.2) + scale_x_log10() + facet_wrap(~ Year)

mutton_densityplot2Year

#Which it did - this showed us the two peaks were present because of 
#the two quite different value ranges for measurements of C17.0 
#for each year. Why is this? We dont know.
#We dont know how the data is being measured. We would like to assume 
#the same experimental procedure is applied to all
#samples but we dont know this and we dont even know what 
#units are being measured.
#samples from 2011 and 2014 could be a time course (ie lamb 
#meat versus mutton) but we dont know that.
#Whats happening with the MNA measurements now?
#We can two peaks in that data too now we've stretched it out.

# just for fun we tried a bar chart

mutton_bar <- ggplot(data = df2, aes(x = fatty_acid, y = measurement)) +
  geom_col() + facet_wrap(~ Year)

# Humph! - hopeless just as Kerensa suggested. Indicates 
#levels of all fatty acids tested were higher 
#in 2011 than 2014 but if these fatty acid are supposed to 
#be higher in older meat what does this mean??

#What about a histogram?

mutton_histogram <- ggplot(data = df2, aes(x = measurement, fill = fatty_acid)) +
  geom_histogram(alpha=1) + scale_x_log10() + facet_wrap(~ Year)
mutton_histogram

# Better than the bar chart but apart from being able to see
#a slightly different representation of the spread it was 
#still hard to see whats going on.

#Lets try looking at the data as a dot plot but going back to using 
#the original df before we tidied it up so we can compare each
#fatty acid against C17.0
#First looking at each fatty acid againt C17.0
mutton_dotplotMOA <- ggplot(data = df, aes(x = C17.0, y = MOA)) +
  geom_point() + facet_wrap(~ Year)
mutton_dotplotMOA
#quite scattered distribution in 2011 - better correlation for 2014
#measurements.

mutton_dotplotMNA <- ggplot(data = df, aes(x = C17.0, y = MNA)) +
  geom_point() + facet_wrap(~ Year)
mutton_dotplotMNA
#Similar story ad for MOA

mutton_dotplotEOA <- ggplot(data = df, aes(x = C17.0, y = EOA)) +
  geom_point() + facet_wrap(~ Year)
mutton_dotplotEOA

#Added a line of best fit

mutton_dotplotMOA_L <- ggplot(data = df, aes(x = C17.0, y = MOA)) +
  geom_point() + geom_smooth() + facet_wrap(~ Year)
mutton_dotplotMOA_L
mutton_dotplotMNA_L <- ggplot(data = df, aes(x = C17.0, y = MNA)) +
  geom_point() + geom_smooth() + facet_wrap(~ Year)
mutton_dotplotMNA_L
mutton_dotplotEOA_L <- ggplot(data = df, aes(x = C17.0, y = EOA)) +
  geom_point() + geom_smooth() + facet_wrap(~ Year)
mutton_dotplotEOA_L
#That is some crazy lines (especially for EOA) and quite different stories between the data from 
#each year.



#we added geom_smooth and "lm" for linear method to get a straight line
#otherwise the line follows the best fit and its wavey

mutton_dotplotMOA_SL <- ggplot(data = df, aes(x = C17.0, y = MOA)) +
  geom_point() + geom_smooth(method = "lm" ) + facet_wrap(~ Year)
mutton_dotplotMNA_SL <- ggplot(data = df, aes(x = C17.0, y = MNA)) +
  geom_point() + geom_smooth(method = "lm" ) + facet_wrap(~ Year)
mutton_dotplotEOA_SL <- ggplot(data = df, aes(x = C17.0, y = EOA)) +
  geom_point() + geom_smooth(method = "lm" ) + facet_wrap(~ Year)
mutton_dotplotMOA_SL
mutton_dotplotMNA_SL
mutton_dotplotEOA_SL

#Would be good to see both year plots in a comparable way
#so allow x axis to expand
mutton_dotplotMOA_SLX <- ggplot(data = df, aes(x = C17.0, y = MOA)) +
  geom_point() + geom_smooth(method = "lm" ) + 
  facet_wrap(~ Year, scales =  "free_x")
mutton_dotplotMOA_SLX

mutton_dotplotMNA_SLX <- ggplot(data = df, aes(x = C17.0, y = MNA)) +
  geom_point() + geom_smooth(method = "lm" ) + 
  facet_wrap(~ Year, scales =  "free_x")
mutton_dotplotMNA_SLX

mutton_dotplotEOA_SLX <- ggplot(data = df, aes(x = C17.0, y = EOA)) +
  geom_point() + geom_smooth(method = "lm" ) + 
  facet_wrap(~ Year, scales =  "free_x")
mutton_dotplotEOA_SLX


#need to make another df where we have 
#C17.0 in one column and the other 3 fatty acids together 
#so we can make a graph with C17.0 on the x axis  
#the other 3 fatty acids 
#on the y axis

dfthreefa <- df %>% gather(key = other_fa, 
                         value = measurement, -Year, -C17.0)

dfthreefa

#tell ggplot to graph the "other_fa" in colour

mutton_dotplot_3fa <- ggplot(data = dfthree, aes(x = C17.0, y = measurement, color = other_fa)) +
  geom_point() + geom_smooth(method = "lm" ) + 
  facet_wrap(~ Year, scales =  "free_x")
mutton_dotplot_3fa

#took out the linear measurement just for a look

mutton_dotplot_3fa_2 <- ggplot(data = dfthree, aes(x = C17.0, y = measurement, color = other_fa)) +
  geom_point() + geom_smooth() + 
  facet_wrap(~ Year, scales =  "free_x")
mutton_dotplot_3fa_2

#Just for interest had a look at what happens when
#both axis are "free"


mutton_dotplot_3fa_free <- ggplot(data = dfthree, aes(x = C17.0, y = measurement, color = other_fa)) +
  geom_point() + geom_smooth() + 
  facet_wrap(~ Year, scales = "free")
mutton_dotplot_3fa_free

#Then we combined both sets of data on the same graph with the 
#years in different shapes

mutton_dotplot_3fa_shape <- ggplot(data = dfthree, aes(x = C17.0, 
    y = measurement, color = other_fa)) +
  geom_point(aes(shape = factor(Year))) + geom_smooth()
mutton_dotplot_3fa_shape

#Feel like roat lamb for dinner :-)



