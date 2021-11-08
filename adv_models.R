#Laura Barraclough 08/11/2021
#Assessing Model Fit using AIC and COnstructing GLMs
#s1729795@ed.ac.uk

#Exercise 1: Exploring Model Fit ----
soils<-read.csv("Peru_Soil_Data.csv",
                row.names = 1, stringsAsFactors = T)
  #create a linear model for Soil pH and Habitat
lm_Habitat<- lm(Soil_pH~ Habitat, data = soils)
lm_Habitat_TBS<- lm(Soil_pH~ Habitat +Total_Base_Saturation,
                    data = soils) #Total_Base_Saturation is a random effect
lm_Habitat_TBS_Interaction<- lm(Soil_pH~ Habitat*Total_Base_Saturation,
                                data = soils)
  #compare the AIC values of these models
AIC(lm_Habitat, lm_Habitat_TBS, lm_Habitat_TBS_Interaction)
  #lm_Habitat_TBS has the smallest AIC value 

  #it is often useful to construct a null model in order to assess if even your 
  #simple models are worth using.
  #to do this you put the number 1 in the explanatory variables part of the model
lm_null<- lm(Soil_pH~1,
             data = soils)
AIC(lm_null, lm_Habitat, lm_Habitat_TBS, lm_Habitat_TBS_Interaction)

#Exercise 2: GLMs with a Poisson Response ----
  #load dataset
inga<- read.csv("Inga_abundances.csv", row.names = 1, stringsAsFactors = T)
  #binding the two datasets together
cbind(rownames(soils), rownames(inga))
combo<- cbind(soils, inga)

  #using GLMs to assess how the abundance of different species varies with soil
  #characteristics and habitat type.
  #Look at thibaudiana as an example:
mod1<- glm(thibaudiana~ Habitat,
           data = combo,
           family = "poisson") #tells us the distribution of the data
mod1
  #this model violates the conditions of a possion because the y-intercept is negative
  #however, because we are using a log link, we need to exponentiate 

exp(-1.946) #0.1428443
mean(combo$thibaudiana[combo$Habitat == "floodplain"])
  #make sure this works for uplands
exp(-1.946 + 4.518) #13.09198
mean(combo$thibaudiana[combo$Habitat == "upland"])
  #check the residuals of the functions
summary(mod1)

  #creating a null model by only estimating the intercept
mod_null<- glm(thibaudiana~ 1,
               data = combo,
               family = "poisson")
AIC(mod_null, mod1)
  #even though the AIC of mod1 is very high, the AIC of mod_null is even higher.
  #this shows that the model does explain some of the data

mod2<- glm(thibaudiana~ Soil_pH,
           data = combo,
           family = "poisson")
summary(mod2)
AIC(mod_null, mod1, mod2) #habitat still provides a better explanation of the 
  #data then soil.

  #create a multivariate model of the variables
mod3<- glm(thibaudiana~ Habitat + Soil_pH,
           data = combo,
           family = "poisson")
mod4<- glm(thibaudiana~ Habitat * Soil_pH,
           data = combo,
           family = "poisson")
AIC(mod_null, mod1, mod2, mod3, mod4) #mod3 best describes the variation
summary(mod3) #this model violates one of our assumptions
  #there is some overdispersion
  #we know this as the ratio of teh residual deviance to the 
  #the residual degrees of freedom has decreased

plot(combo$thibaudiana,
     fitted(mod3),
     xlab = "Observed", ylab= "Predicted")
abline(0,1)
  #this plot shows that this model does not predict the distribution of thibaudiana
  #very well
  #we can explore this further
par(mfrow= c(1,2))
plot(resid(mod3)~ Habitat,
     data = combo)
plot(resid(mod3)~Soil_pH,
     data = combo)


combo_fp <- combo[combo$Habitat=="floodplain",]
combo_up <- combo[combo$Habitat=="upland",]
plot(combo$Soil_pH,combo$thibaudiana,type="n")
points(combo_fp$Soil_pH,combo_fp$thibaudiana,pch=21,bg="blue")
points(combo_up$Soil_pH,combo_up$thibaudiana,pch=24,bg="red")
points(combo$Soil_pH,fitted(mod3), col="green")


#Exercise 3: GLMs with a binomial Response ----