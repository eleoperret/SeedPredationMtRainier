
path<-"C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US"
setwd(path)

predators_data<-read.csv(paste(path,'/Data/',"camera_data.csv",sep = ''),header = TRUE, sep=";")
summary(predators_data)
predators_data_selected<-predators_data[c(1,4,10)]

names_predators<-unique(predators_data_selected$Species_ID)
names_stand<-unique(predators_data_selected$Stand)
names_weeks<-unique(predators_data_selected$Week)
# Count the occurrences of each variable
variable_counts <- table(predators_data_selected)

# Print the variable counts
print(variable_counts)

plot(variable_counts)
list(names_predators)
list(Stand,data=predators_data_selected)
barplot(variable_counts, xlab = "Variable", ylab = "Count", main = "Variable Counts",width=0.5)
write.csv(variable_counts, "my_variable.csv")


peromyscus<-subset(predators_data_selected,predators_data_selected$Species_ID=="Peromyscus maniculatus")
plot(peromyscus)

#To04
TO04_predators<-subset(predators_data_selected,predators_data_selected$Stand=="TO04")
TO04_predators<-TO04_predators[c(2,3)]
variable_counts_TO04 <- table(TO04_predators)
print(variable_counts_TO04)

#AV06
AV06_predators<-subset(predators_data_selected,predators_data_selected$Stand=="AV06")
AV06_predators<-AV06_predators[c(2,3)]
variable_counts_AV06 <- table(AV06_predators)
print(variable_counts_AV06)

#AE10
AE10_predators<-subset(predators_data_selected,predators_data_selected$Stand=="AE10")
AE10_predators<-AE10_predators[c(2,3)]
variable_counts_AE10 <- table(AE10_predators)
print(variable_counts_AE10)


print(variable_counts_TO04)
print(variable_counts_AV06)
print(variable_counts_AE10)



name_predators_TO04<-c("?", "Bird", "L.americanus", "P.maniculatus", "Shrew", "Vole", "Zapus")
name_predators_AV06<-c("Bird", "F.squirrel", "P.maniculatus", "Tamia", "Vole")
name_predators_AE10<-c("?", "Bird", "F.squirrel", "P.maniculatus", "Tamia")


barplot(variable_counts_TO04, xlab = "Variable", ylab = "Count", main = "Variable Counts in TO04",width=0.2, axes=FALSE,axis(1, at = seq(1,length(name_predators_TO04)), labels = c(name_predators)), axis(2, at= seq(0,1500,500), labels=c("0","500","1000","1500") ))
barplot(variable_counts_AV06, xlab = "Variable", ylab = "Count", main = "Variable Counts in AV06")
barplot(variable_counts_AE10, xlab = "Variable", ylab = "Count", main = "Variable Counts in AE10")


# Set the desired range for the y-axis
ylim <- c(0, 1500)

# Plot the variable counts as a bar plot with custom colors, x-axis labels, and y-axis range
barplot(variable_counts_TO04,
        xlab = "Variable",
        ylab = "Count",
        main = "Variable Counts in TO04",
        width = 0.2,
        col = c("skyblue"),
        names.arg = name_predators,
        ylim = ylim)


ylim <- c(0, 1500)

barplot(variable_counts_TO04,xlab = "Variable",ylab = "Count",main = "Variable Counts in TO04",width = 0.2,col = c("darkred"),names.arg = name_predators,ylim = ylim)

barplot(variable_counts_AV06,xlab = "Variable",ylab = "Count",main = "Variable Counts in AV06",width = 0.2,col = c("salmon"),names.arg = name_predators_AV06,ylim = ylim)

barplot(variable_counts_AE10,xlab = "Variable",ylab = "Count",main = "Variable Counts in AE10",width = 0.2,col = c("skyblue"),names.arg = name_predators_AE10,ylim = ylim)


