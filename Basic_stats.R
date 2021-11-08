#Laura Barraclough 08/11/2021
#Professional Skills: Basic Statistics
#s1729795@ed.ac.uk

#Exercise 1: Basic ANOVA ----
  #create vectors of tree height measurements
A<- c(115, 120, 135, 155, 160, 170, 175, 200, 205, 220)
B<- c(115, 145,75,60,95,95,170,105,130,120)
C<- c(155,75,110,145,85,105,140,75,140,110)

  #create a vector of combined heights of three categories
height<- c(A,B,C)
  #add in treatment
fertiliser<- c(rep("A",length(A)), rep("B",length(B)), rep("C",length(C)))
  #put all the data into one frame
alldata<- data.frame(fertiliser,height)

  #building a asimple statistical model
(height_lm<- lm(height~fertiliser, data = alldata))
anova(height_lm)
summary(height_lm)
  #            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   165.50      10.37  15.955 2.86e-15 ***
  #fertiliserB   -54.50      14.67  -3.715 0.000936 ***
  #fertiliserC   -51.50      14.67  -3.511 0.001590 ** 

  #does our data satisfy the assumptions of the anova test
height_resids<- resid(height_lm)
shapiro.test(height_resids)#because our p>0.05, we can reject the hypothesis that our data is not normally distributed
bartlett.test(height~fertiliser, data = alldata)

  #plot our linear model object
plot(height_lm) #our data satisfies the assumptions of the test

  #fertiliser significantly affects seedling height.
  #although we cannot conclude that fertiliser A leads to significantly taller seedlings than fertilisers B and C.

#Using a Tukey's test to statistically test which fertilisers are driving our significant anova result
(height_aov<- aov(height~fertiliser, data = alldata))
TukeyHSD(height_aov)
  #this shows us that A and B have significantly different heights
  #A and C have significantly different heights, but B and C do not.
  #A differs significantly from B and C.



#Exercise 2:
#Analyses with Single Categorical Explanatory Variable and Continuous Response ----
soils<- read.csv("Peru_Soil_Data.csv")

  #have a quick look at the data
dim(soils)
names(soils)
soils$Soil_pH
head(soils)
summary(soils)

  #explore each variable individually using histograms
hist(soils$Soil_pH, breaks = 10, col="grey",
     xlab="Soil pH", main = "Histogram of Soil pH")
  #add a line showing the mean and median
abline(v= median(soils$Soil_pH))
abline(v= mean(soils$Soil_pH))

  #boxplots of variables across habitat types
boxplot(Soil_pH~Habitat,data=soils)
boxplot(Potassium~Habitat,data=soils)

  #use ANOVA to determine if these Soil_pH and Potassium vary significantly
  #with habitat type
lm_pH<- lm(Soil_pH~ Habitat, data = soils)
anova(lm_pH) #returns significant P values 
lm_K<- lm(Potassium~ Habitat, data = soils)
anova(lm_K) #returns significant P values

  #check the assumptions hold true by extracting teh residuals
lm_pH_resids<- resid(lm_pH)
shapiro.test(lm_pH_resids)
bartlett.test(Soil_pH~ Habitat, data = soils)
lm_K_resids<- resid(lm_K)
shapiro.test(lm_K_resids)
bartlett.test(Potassium~ Habitat, data = soils)
  #plot these
plot.lm(lm_pH)
plot.lm(lm_K)
  #potassium does not have homogenous variance across the floodplain and terra firme

  #visually examining the data to check assumptions