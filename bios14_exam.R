library(tidyverse)
library(flexplot)
library(lme4)
library(MASS)
library(corrplot)
library(glmmTMB)
library(viridis)
library(ggpubr)


### Plot aesthetics
fig <- theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.y = element_text(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))



# Load in the data
meta <- read.csv("06_BIOS14_EXAM/exam2023_metadata.csv")

raw <- read.csv("06_BIOS14_EXAM/exam2023_data.csv")


### Data clean up and preparation
# Factorize and removes spaces from values.
raw$Season <- gsub(" ", ".", raw$Season)
raw$Season <- factor(raw$Season, levels = c("Spring.2006",
                                            "Winter.2006",
                                            "Autumn.2007"))

raw$Property <- factor(raw$Property, levels = unique(raw$Property))
raw$Aspect <- factor(raw$Aspect, levels = unique(raw$Aspect))
raw$Landscape.position <- factor(raw$Landscape.position, 
                                 levels = unique(raw$Landscape.position))

# Shorten variable names
raw <- raw %>% 
  rename(
    Land.pos = Landscape.position,
    Ex.An.Grass = ExoticAnnualGrass_cover,
    Ex.An.Herb = ExoticAnnualHerb_cover,
    Ex.Per.Herb = ExoticPerennialHerb_cover,
    Ex.Per.Grass = ExoticPerennialGrass_cover,
    Ex.Shrub = ExoticShrub_cover,
    Na.Per.Fern = NativePerennialFern_cover,
    Na.Per.Grass = NativePerennialGrass_cover,
    Na.Per.Herb = NativePerennialHerb_cover,
    Na.Per.Gram = NativePerennialGraminoid_cover,
    Na.Shrub = NativeShrub_cover,
    Bare.Ground = BareGround_cover,
    Litter = Litter_cover,
    Moss.Lichen = MossLichen_cover,
    Rock = Rock_cover,
    Euc.canopy = Euc_canopy_cover,
    Dist.to.canopy = Distance_to_Eucalypt_canopy.m.,
    An.prec = annual_precipitation,
    Prec.warmest = precipitation_warmest_quarter,
    Prec.coldest = precipitation_coldest_quarter,
    euc.small = euc_sdlgs0_50cm,
    euc.mid = euc_sdlgs50cm.2m,
    euc.tall = euc_sdlgs.2m,
  )

# Looks much better now
glimpse(raw)

      #### Question to analyse
# Which factors influence the abundance of eucalyptus seedlings?

      #### Thoughts.
# Each property has multiple samples from them.
# Thus, those samples might have correlating values as they experience
# the same conditions. I therefore have to account for this potential
# autocorrelation. In other words, the property is an
# random effect and not a fixed effect.
# As well for season, aspect and land.pos




# Lets explore the variables of interest.
respv <- c("Ex.An.Grass", "Ex.An.Herb", "Ex.Per.Herb", "Ex.Per.Grass",
           "Ex.Shrub", "Na.Per.Fern", "Na.Per.Grass", "Na.Per.Herb",
           "Na.Per.Gram", "Na.Shrub", "Litter", "Rock", "Moss.Lichen",
           "Dist.to.canopy", "Bare.Ground", "Euc.canopy",
           "euc.small", "euc.mid", "euc.tall")

# Everyone seems to be Poisson distributed. Ex.Shrub have very few counts > 0
# euc.small has quite an outlier on 80 something cnts.
# euc.tall has barly any counts..
# The Shrubs are not very abundant.

for (i in respv) {
  hist <- hist(raw[[i]], main = i, xlab = i)
  print(hist)
}

# Lets check if there are any NA values
tmpdf <- raw %>% 
  dplyr::select(all_of(respv))

sum(is.na(tmpdf))  # 17 NA's

# Lets see which rows and columns have NA's
# Row 75 seem to be missing values for all coverage. 
# row 253 and 258 is missing values for dist.to.canopy.
which(is.na(tmpdf), arr.ind = TRUE) 


# Lets also check for collinearity when we have the variables.
cormat<- tmpdf %>% 
  na.omit() %>% 
  as.matrix() %>% 
  cor()

# Plot the correlation matrix
# No issues with collinearity, very nice!
corrplot(cormat, type = "upper", order = "hclust", tl.col = "black",
         tl.srt = 45)

# And also remove the rows with NAs
df <- raw %>% 
  dplyr::select(SurveyID, Season, Property, Aspect, Land.pos, all_of(respv))

df <- df[c(-75, -253, -258),]
sum(is.na(df)) # 0



# Determine if i need a mixed model
# Standard linear model without random effects for natives.
# Function to perform linear model analysis and plot residuals
perform_lm_analysis <- function(response_var, data) {
  # Fit the linear model
  chr_formula = "~ . - SurveyID - Season - Property - Aspect - Land.pos"
  formula = as.formula(paste(response_var, chr_formula, sep = ""))
  lm_model <- lm(formula, data = data)
  
  # Calculate the residuals
  lm_resid <- rstandard(lm_model)
  
  # Plot the residuals against potential random factors
  par(mfrow = c(2, 2))
  plot_factors <- c("Property", "Season", "Aspect", "Land.pos")
  for (factor in plot_factors) {
    plot(lm_resid ~ as.factor(data[[factor]]), xlab = factor, ylab = "Residuals",
         main = paste("Residuals for", response_var))
    abline(0, 0, lty = 2, lw = 1.5)
  }
  par(mfrow = c(1, 1))
}

# Plotting the residuals
perform_lm_analysis("euc.small", df)
perform_lm_analysis("euc.mid", df)
perform_lm_analysis("euc.tall", df)

# For euc.small and euc.tall there is really no variation.
# For euc.mid there is some variation in property and little in aspect.



        #### Lets check overdispersion
# Set a base formula
base_form <- "~ Dist.to.canopy + Euc.canopy +
               Ex.An.Grass + Ex.An.Herb + Ex.Per.Herb + Ex.Per.Grass +
               Ex.Per.Herb + Ex.Shrub + Na.Per.Fern + Na.Per.Grass +
               Na.Per.Herb + Na.Per.Gram + Na.Shrub + Litter + Rock +
               Moss.Lichen + Bare.Ground"

# Loop through and check the pearson estimate
for (i in c("euc.small", "euc.mid")) {
  model_formula_str <- paste0(i, base_form)
  model_formula <- as.formula(model_formula_str)
  
  poi_m <- glmmTMB(model_formula, family = poisson, data = df)
  
  overdisp <- sum(residuals(poi_m, type = "pearson")^2) / df.residual(poi_m)
  overdisp <- round(overdisp,2)
  print(paste("Overdispersion for", i, ": ", overdisp))
}

# euc.small = 12.47, euc.mid = 7.17

# Lets see if neg binomial corrects this
# euc.small we get 3.36 with nbinom1, and 1.24 with nbinom2
# euc.mid we get 0.74 with nbinom1, and 0.89 with nbinom2 
model_formula_str <- paste0("euc.small", base_form)
frm <- as.formula(model_formula_str)
test.nb <- glmmTMB(frm, family = nbinom1, data = df)
sum(residuals(test.nb, type = "pearson")^2) / df.residual(test.nb)

# Lets go with nbinom2 as they are closer to 1.


        #### Lets analyse to see what fixed and random effects is best.

# Function to run models
run_models <- function(df, response_var) {
  # Base formula with different response variable
  base_formula <- as.formula(
    paste(response_var, 
          "~ Dist.to.canopy + Euc.canopy + Ex.An.Grass + Ex.An.Herb + 
          Ex.Per.Herb + Ex.Per.Grass + Ex.Per.Herb + Ex.Shrub + Na.Per.Fern + 
          Na.Per.Grass + Na.Per.Herb + Na.Per.Gram + Na.Shrub + Litter + Rock + 
          Moss.Lichen + Bare.Ground"
    )
  )
  
  # Define model formulas
  formulas <- list(
    form1 = update(base_formula, . ~ . + Land.pos),
    form2 = update(base_formula, . ~ . + Aspect),
    form3 = update(base_formula, . ~ . + Land.pos + Aspect),
    form4 = update(base_formula, . ~ . + Land.pos + Aspect + (1|Property)),
    form5 = update(base_formula, . ~ . + Land.pos + Aspect + (1|Season) + 
                     (1|Property)),
    form6 = update(base_formula, . ~ . + Land.pos + (1|Aspect) + (1|Property) + 
                     (1|Season)),
    form7 = update(base_formula, . ~ . + (1|Property) + (1|Aspect) + 
                     (1|Season)),
    form8 = update(base_formula, . ~ . + (1|Property))
  )
  
  # Fit models
  models <- lapply(formulas, function(frm) {
    glmmTMB(frm, family = nbinom2, data = df)
  })
  
  # Compare models
  model_comparison <- MuMIn::model.sel(models[[1]], models[[2]], models[[3]], 
                                       models[[4]], models[[5]], models[[6]], 
                                       models[[7]], models[[8]])
  
  return(model_comparison)
}

# Lets run for small and see results.
small_results <- run_models(df, "euc.small")
# model 1 was best but model 8 is more logical (delta = 0.15).
(aic_res <- small_results[, c("AICc", "delta", "df", "logLik")])

# Now euc.mid, model 8 was once best again. Model 1 has delta 11.4
mid_results <- run_models(df, "euc.mid")
(aic_res <- mid_results[, c("AICc", "delta", "df", "logLik")])


        #### Lets run the models with the best fixed and random effects.
# euc.small
f.small <- paste0("euc.small", base_form)
f.small <- as.formula(f.small)
f.small <- update(f.small, . ~ . + (1|Property))
small.m0 <- glmmTMB(f.small, family = nbinom2, data = df)

# euc.mid
f.mid <- paste0("euc.mid", base_form)
f.mid <- as.formula(f.mid)
f.mid <- update(f.mid, . ~ . + (1|Property))
mid.m0 <- glmmTMB(f.mid, family = nbinom2, data = df)


# Now lets reduce the models.
# small red1
small.red1 <- update(f.small, . ~ . - Ex.An.Grass - Ex.Shrub - Na.Shrub -
                       Rock)
small.m1 <- glmmTMB(small.red1, family = nbinom2, data = df)

# small red 2
small.red2 <- update(small.red1, . ~ . - Moss.Lichen)
small.m2 <- glmmTMB(small.red2, family = nbinom2, data = df)

# small red 3
small.red3 <- update(small.red2, . ~ . - Na.Per.Gram - Na.Per.Grass)
small.m3 <- glmmTMB(small.red3, family = nbinom2, data = df)

# small red 4
small.red4 <- update(small.red3, . ~ . - Na.Per.Herb - Ex.Per.Grass)
small.m4 <- glmmTMB(small.red4, family = nbinom2, data = df)


# mid red 1
mid.red1 <- update(f.mid, . ~ . - Ex.Shrub - Na.Shrub - Rock)
mid.m1 <- glmmTMB(mid.red1, family = nbinom2, data = df)

# mid red 2
mid.red2 <- update(mid.red1, . ~ . - Ex.An.Grass - Na.Per.Grass - Moss.Lichen)
mid.m2 <- glmmTMB(mid.red2, family = nbinom2, data = df)

# mid red 3
mid.red3 <- update(mid.red2, . ~ . - Na.Per.Gram)
mid.m3 <- glmmTMB(mid.red3, family = nbinom2, data = df)



# Lets compare the small models
small_aic_df_red <- MuMIn::model.sel(small.m0, small.m1, small.m2, small.m3, 
                                     small.m4)
# small.m4 is the best model with the lowest AICc and 9 dF
(aic_res_red <- small_aic_df_red[, c("AICc", "delta", "df", "logLik")])

# mid.m4 is the best model with lowest AICc and also 9 dF
mid_aic_df_red <- MuMIn::model.sel(mid.m0, mid.m1, mid.m2, mid.m3)
(aic_res_red <- mid_aic_df_red[, c("AICc", "delta", "df", "logLik")])


# Lets store them in a clean name.
m_small <- small.m4
m_mid <- mid.m3


# Lets convert the cooefficients back to its original values
# I will use these values for my tables in the report.

# small
# Extract and run use exp() to transform the coefficients
small.cof <- coef(summary(m_small))$cond  
small.cof[,1] <- round(exp(small.cof[,1]),4)
small.cof[,2] <- round(exp(small.cof[,2]),4)
small.cof[,3] <- round(small.cof[,3],4)
small.cof[,4] <- round(small.cof[,4],4)

# mid
# Extract and run use exp() to transform the coefficients
mid.cof <- coef(summary(m_mid))$cond
mid.cof[,1] <- round(exp(mid.cof[,1]),4)
mid.cof[,2] <- round(exp(mid.cof[,2]),4)
mid.cof[,3] <- round(mid.cof[,3],4)
mid.cof[,4] <- round(mid.cof[,4],4)





    ### Validating the model

# I got to be vary regarding that mid model might be inflated.
# small model
sum(residuals(m_small, type = "pearson")^2) / df.residual(m_small) # 1.01

# mid model
sum(residuals(m_mid, type = "pearson")^2) / df.residual(m_mid) # 0.74


# Lets try to plot the residuals
# Plot predicted values vs residual values
plot(resid(m_small) ~ fitted(m_small), main = "euc.small", 
     xlab = "Predicted values", 
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

plot(resid(m_mid) ~ fitted(m_mid), main = "euc.mid", 
     xlab = "Predicted values", 
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)



# Lets plot, but first lets set up the data kinda like öystein did...

# small model
sm_df <- expand.grid(
  Dist.to.canopy = seq(min(df$Dist.to.canopy), max(df$Dist.to.canopy),
                       length.out = 200),
  Euc.canopy = mean(df$Euc.canopy),
  Ex.An.Herb = mean(df$Ex.An.Herb),
  Ex.Per.Herb = mean(df$Ex.Per.Herb),
  Na.Per.Fern = mean(df$Na.Per.Fern),
  Litter = mean(df$Litter),
  Bare.Ground = seq(min(df$Bare.Ground), max(df$Bare.Ground), length.out = 200),
  Property = "Barlow"
) 
sm_df$small.euc.pred <- predict(m_small, newdata = sm_df, type = "response")

# mid model
mid_df <- expand.grid(
  Dist.to.canopy = seq(min(df$Dist.to.canopy), max(df$Dist.to.canopy),
                       length.out = 200),
  Euc.canopy = mean(df$Euc.canopy),
  Ex.An.Herb = mean(df$Ex.An.Herb),
  Ex.Per.Herb = mean(df$Ex.Per.Herb),
  Ex.Per.Grass = mean(df$Ex.Per.Grass),
  Na.Per.Fern = mean(df$Na.Per.Fern),
  Na.Per.Herb = mean(df$Na.Per.Herb),
  Litter = seq(min(df$Litter), max(df$Litter), length.out = 200),
  Bare.Ground = mean(df$Bare.Ground),
  Property = "Barlow"
)
mid_df$mid.euc.pred <- predict(m_mid, newdata = mid_df, type = "response")


    ### Contour plots
# Plotting the predicted counts against Litter and Euc.canopy (contour plot)
small_cont <- ggplot(sm_df, aes(x = Dist.to.canopy, y = Bare.Ground,
                                z = small.euc.pred)) +
  geom_tile(aes(fill = small.euc.pred)) + # This creates a heatmap
  geom_contour(aes(z = small.euc.pred)) + # This adds contour lines
  labs(x = "Distance to canopy (m)", y = "Bare Ground Coverage (%)", 
       fill = "Predicted Small Seedlings") +
  theme_minimal() +
  scale_fill_viridis("euc.small")

mid_cont <- ggplot(mid_df,aes(x = Dist.to.canopy, y = Litter,
                              z = mid.euc.pred)) +
  geom_tile(aes(fill = mid.euc.pred)) + 
  geom_contour(aes(z = mid.euc.pred)) + 
  labs(x = "Distance to canopy (m)", y = "Leaf Litter Coverage (%)", 
       fill = "Predicted Mid-sized Seedlings") +
  theme_minimal() +
  scale_fill_viridis("euc.mid")

ggarrange(small_cont, mid_cont, ncol = 2, nrow = 1, common.legend = FALSE, 
          legend = "bottom", labels = c("A", "B"))


    ### Predicted vs Observed plot
# Add the predicted and variance that comes from property?
df$pred.small <- predict(m_small, newdata = df, type = "response")
df$pred.small.pos.sd <- df$pred.small * exp(1.33)
df$pred.small.neg.sd <- df$pred.small / exp(1.33) 


# Plotting Predicted vs Observed for Small Seedlings
small_po <- ggplot(df, aes(x = pred.small, y = euc.small)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
  labs(x = "Predicted Small Seedlings", y = "Observed Small Seedlings") +
  fig

# Add lines for predictions adjusted for the random effect's SD
small_po + 
  geom_line(aes(y = pred.small.pos.sd), color = "blue", linetype = "solid") +
  geom_line(aes(y = pred.small.neg.sd), color = "orange", linetype = "solid") +
  annotate("text", label = "Property + SD", x = 9, y = 28, color = "blue") +
  annotate("text", label = "Property - SD", x = 11, y = 1, color = "orange") +
  annotate("text", label = "model", x = 11, y = 15)


df$pred.mid <- predict(m_mid, newdata = df, type = "response")
df$pred.mid.pos.sd <- df$pred.mid * exp(1.33)
df$pred.mid.neg.sd <- df$pred.mid / exp(1.33)


# Plotting Predicted vs Observed for Mid-sized Seedlings
mid_po <- ggplot(df, aes(x = pred.mid, y = euc.mid)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
  labs(x = "Predicted Mid-sized Seedlings", 
       y = "Observed Mid-sized Seedlings") +
  fig

# Add lines for predictions adjusted for the random effect's SD
mid_po + 
  geom_line(aes(y = pred.mid.pos.sd), color = "blue", linetype = "solid") +
  geom_line(aes(y = pred.mid.neg.sd), color = "orange", linetype = "solid")  +
  annotate("text", label = "Property + SD", x = 9, y = 28, color = "blue") +
  annotate("text", label = "Property - SD", x = 11, y = 2, color = "orange") +
  annotate("text", label = "model", x = 11, y = 13)




