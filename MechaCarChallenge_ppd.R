# MPG Regression
mecha_cars <- read.csv('MechaCar_mpg.csv', stringsAsFactors = F) #read in dataset
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mecha_cars) #generate multiple linear regression model
MechaModel <- lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mecha_cars)
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mecha_cars)) #generate multiple linear regression summary model
confint(MechaModel, conf.level = 0.95) 

# Suspension Coil Summary
susp_coil <- read.csv('Suspension_Coil.csv', header = T, stringsAsFactors = F) #read in dataset
mean(susp_coil$PSI)
median(susp_coil$PSI)
var(susp_coil$PSI)
sd(susp_coil$PSI)

# Filter the coil dataset by Manfacturing lot
Lot1 <- filter(susp_coil, susp_coil$Manufacturing_Lot == "Lot1")
Lot2 <- filter(susp_coil, susp_coil$Manufacturing_Lot == "Lot2")
Lot3 <- filter(susp_coil, susp_coil$Manufacturing_Lot == "Lot3")

# Determine Variance for the three manufacturing lots
var(susp_coil$PSI)
var(Lot1$PSI)
var(Lot2$PSI)
var(Lot3$PSI)

# Determine Mean for the three manufacturing lots
mean(susp_coil$PSI)
mean(Lot1$PSI)
mean(Lot2$PSI)
mean(Lot3$PSI)


# Suspension Coil t-test, three manufacturing lots
t.test((susp_coil$PSI),mu=1500) #compare suspension coil's psi results to mean population results of 1,500 psi
coil_sample_table <- susp_coil %>% sample_n(30) #randomly sample 30 data points
t.test((coil_sample_table$PSI),mu=mean(susp_coil$PSI)) #compare suspension coil's psi results to mean population results of 1,500 psi

# ANOVA test for three different manufacturing lots, one-way test to determine PSI based on the three lots)
coil_filt <- susp_coil[,c("PSI", "Manufacturing_Lot")] #filter columnts from suspension coil dataset
coil_filt$Manufacturing_Lot <- factor(susp_coil$Manufacturing_Lot) #convert numeric column to factor (is this necessary?)
aov(susp_coil$PSI ~ susp_coil$Manufacturing_Lot, data = coil_filt) # compare means across multiple levels
summary(aov(susp_coil$PSI ~ susp_coil$Manufacturing_Lot, data = coil_filt))

# t-tests for Independent Lots
t.test((Lot1$PSI),mu=mean(susp_coil$PSI)) #compare Lot1's psi results to mean population results of 1,500 psi
t.test((Lot2$PSI),mu=mean(susp_coil$PSI)) #compare Lot2's psi results to mean population results of 1,500 psi
t.test((Lot3$PSI),mu=mean(susp_coil$PSI)) #compare Lot3's psi results to mean population results of 1,500 psi
