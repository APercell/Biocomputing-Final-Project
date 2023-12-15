# Part 3 of function3 - Percent male and female
library(ggplot2)
library(cowplot)

# First, import the file with all of the data (assuming file is in directory)
all.data <- read.csv(file="allData.csv",header=TRUE)

# To get # of screens ran, use nrow to count how many lines
screensran<-nrow(all.data)
# To get the % of screened patients that were male and female, use a for loop
totalfemale=0
totalmale=0
for (i in 1:screensran) {
if (all.data$gender[i]=="female") {
  totalfemale=totalfemale+1
} else if(all.data$gender[i]=="male") {
  totalmale=totalmale+1
}
}
fractionfemale<-totalfemale/screensran
percentfemale<-fractionfemale*100
fractionmale<-totalmale/screensran
percentmale<-fractionmale*100

# Part 4 of function3 - Age Distribution of Patients
# To do this, generate a histogram of ages

#Age distribution of patients
child= 0 # Ages 0 - 8
preteen = 0 # Ages 9 - 12
teen = 0 # Ages 13 -19
adult = 0 #Ages 20 - 59
senior = 0 #Ages 60+

for(i in 1:screenRun){
  if(all.data$age[i] >= 0 && all.data$age[i] < 9){
    child = child + 1
  }else if(all.data$age[i] >= 9 && all.data$age[i] <13){
    preteen =preteen + 1
  }else if(all.data$age[i] >= 13 && all.data$age[i] <20 ){
    teen = teen + 1
  }else if(all.data$age[i] >= 20 && all.data$age[i] < 60 ){
    adult = ult + 1
  }else if(all.data$age[i] >= 60){
    senior = senior + 1
  }
}

agedf<-data.frame(
  agegroup = c('Child','Preteen','Teen','Adult','Senior'),
  population = c(child,preteen,teen,adult,senior))

# Define the order of age groups
age_order <- c('Child', 'Preteen', 'Teen', 'Adult', 'Senior')

# Convert agegroup to a factor with the specified order
agedf$agegroup <- factor(agedf$agegroup, levels = age_order)

# Create a bar plot for age distribution
ggplot(agedf, aes(x = agegroup, y = population, fill = agegroup)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Age Group", y = "Population", title = "Age Distribution of Patients") +
  scale_fill_manual(values = c('Child' = 'blue', 'Preteen' = 'green', 'Teen' = 'yellow', 'Adult' = 'orange', 'Senior' = 'red'))

