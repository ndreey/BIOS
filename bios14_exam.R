library(tidyverse)
library(flexplot)
library(lme4)
library(MuMIn)
library(MASS)


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


# Lets check which properties even have seedlings.
df_1 <- raw %>% 
  # Select the columns required.
  dplyr::select(Property, euc.small, euc.mid, euc.tall) %>% 
  # Group (temporary split) the data into each property
  group_by(Property) %>%
  # Sum the number of seedlings for each property
  summarise(tot.seed = sum(euc.small, euc.mid, euc.tall))

# Plot the total number of seedlings for each property
df_1 %>% 
  arrange(desc(tot.seed)) %>%
  ggplot(aes(x = reorder(Property, -tot.seed), y = tot.seed)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Seedlings per Property",
       x = "Property",
       y = "Total Number of Seedlings") +
  fig +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We see that Olive, Rokahr and Taylor have no seedlings at all.
# Furthermore, Yorston, Stoney, Wakefield, and JClark have 1,2,6,9 respectively.
df_1 %>%  arrange(tot.seed)

# Lets see how they behave over the seasons
low_count <- df_1 %>% 
  filter(tot.seed < 10) %>%
  dplyr::select(Property)

# Use the low count properties to filter.
low_df <- raw %>% 
  # Select the columns required.
  dplyr::select(Property, Season, euc.small, euc.mid, euc.tall) %>% 
  filter(Property %in% low_count$Property) %>% 
  # Group by Property and then by Season
  group_by(Property, Season) %>%
  # Sum the number of seedlings for each season for each property
  summarise(tot.seed = sum(euc.small + euc.mid + euc.tall)) %>% 
  # Filter away the ones that we know are zero
  filter(!Property %in% c("Olive", "Rokahr", "Taylor"))

# I will remove them as well. Might add Wakefield and JCclark back later.
df <- raw %>% 
  filter(!Property %in% low_count$Property)




        ### Data exploration
# Lets see how the seedlings are distributed across seasons
flexplot(euc.small ~ Season, data = df)  # Quite similar, one big outlier in Winter
flexplot(euc.mid ~ Season, data = df)  # Quite even
flexplot(euc.tall ~ Season, data = df) # Autumn sees a big spike.

# Lets see how the seedlings are distributed across properties
flexplot(euc.small ~ Property, data = df)
flexplot(euc.mid ~ Property, data = df)
flexplot(euc.tall ~ Property, data = df)

# Lets see how the seedlings are distributed across Aspect
flexplot(euc.small ~ Aspect, data = df)
flexplot(euc.mid ~ Aspect, data = df)
flexplot(euc.tall ~ Aspect, data = df)

# Now, lets check across landscape position
# Slope seems to increase the numbers for all seedlings
flexplot(euc.small ~ Land.pos, data = df)
flexplot(euc.mid ~ Land.pos, data = df)
flexplot(euc.tall ~ Land.pos, data = df)

# LEts try with distance to canopy
# Definitely seems to show that if they exist, they exist closer to the canopy
flexplot(euc.small ~ Dist.to.canopy, data = df)
flexplot(euc.mid ~ Dist.to.canopy, data = df)
flexplot(euc.tall ~ Dist.to.canopy, data = df)

# What about the canopy cover
# Seems like the less canopy, the more seedlings + got some NA values.
flexplot(euc.small ~ Euc.canopy, data = df)
flexplot(euc.mid ~ Euc.canopy, data = df)
flexplot(euc.tall ~ Euc.canopy, data = df)

# Lets check bare ground
# Hmm, still the smaller values seems to do the deed.
flexplot(euc.small ~ Bare.Ground, data = df)
flexplot(euc.mid ~ Bare.Ground, data = df)
flexplot(euc.tall ~ Bare.Ground, data = df)

# It is clear that the quadrats are independent as the coordinates differ each
# season.
df %>% 
  dplyr::select(Property,Season, Quadrat.no, Northing, Easting) %>% 
  group_by(Property) %>% 
  filter(Property %in% c("Barlow", "Blaber") & Quadrat.no == 1) %>% 
  arrange(Property)

glimpse(df)

      ### Lets see if there is any correlation between predictor variables.
# Lets start with the continuous variables
cor_mat <- df %>% 
  dplyr::select(-SurveyID, -Date, -Season, -Property, -Quadrat.no, -Northing, 
         -Easting, -Aspect, -Land.pos) %>%
  na.omit() %>%
  as.matrix() %>% 
  cor()

# Plot the correlation matrix
corrplot(cor_mat, type = "upper", order = "hclust", tl.col = "black",
         tl.srt = 45)
 

# We see that An.prec, Prec.warmest, and Prec.coldest are highly correlated.
# I also notice that precipitation is highly neg. correlated with the PET.
# I will therefore remove Pre.warmest, Prec.coldest and PET.
# As well as MrVBF as i dont quite understand it.
cor_mat2 <- df %>% 
  dplyr::select(-SurveyID, -Date, -Season, -Property, -Quadrat.no, -Northing, 
                -Easting, -Aspect, -Land.pos, -Prec.warmest, -Prec.coldest, 
                -MrVBF, -PET) %>%
  na.omit() %>%
  as.matrix() %>% 
  cor()

# Plot the correlation matrix
corrplot(cor_mat2, type = "upper", order = "hclust", tl.col = "black",
         tl.srt = 45)

# Lets take a quick look at the distributions.
for (i in colnames(cor_mat2)) {
  hist <- hist(df_con_pred[[i]], main = i, xlab = i)
  print(hist)
}

# SRad_JUL, U_ppm, TH_ppm norm
# SRad_Jan mostly norm, skewed to the right
# K_perc kinda norm, might even be binomial
# An.prec nrom, skewed to the left.
# All cover and dist are Poisson.
# Ex.Shrub is however 0 for all except one...


        ### Lets start modelling

df_model <- df %>% 
  dplyr::select(-SurveyID, -Date, -Northing, -Easting, -Prec.warmest, -Prec.coldest, 
                -MrVBF, -PET) %>% 
  na.omit()


m0 <- glmer.nb(euc.small ~ Ex.An.Grass+Ex.An.Herb + Ex.Per.Herb +
                 Ex.Per.Grass + Ex.Shrub + Na.Per.Fern + Na.Per.Grass + 
                 Na.Per.Herb + Na.Per.Gram + Na.Shrub + Bare.Ground +
                 Litter + Moss.Lichen + Rock + Euc.canopy + (1|Property),
               data = df_model)


summary(m0)

m1 <- glm(euc.small ~ U_ppm + K_perc + Th_ppm,
               data = df_model)

summary(m1)




        
### Thoughts
# Because the same property is used we cannot use a simple linear model.
# Instead, we will have to use a mixed model where property will be accounted
# as a random affect.

# Lets see if there is some overdispersion rocking this dataset.
# Ill calculate the mean and var for each property for each season.

# Lets start with the small seedlings
df_2 <- df %>%
  dplyr::select(Property, Season, euc.small) %>%
  group_by(Property, Season) %>%
  summarise(mean = mean(euc.small, na.rm = TRUE),
            var = var(euc.small, na.rm = TRUE))


# Plot mean values on x and variance on y for each Season
for (i in unique(df_2$Season)) {
  overdisp <- df_2 %>%
    dplyr::filter(Season == i) %>%
    ggplot(aes(x = mean, y = var)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    labs(title = paste("Mean vs Variance for", i),
         x = "Mean euc.small",
         y = "Variance") +
    theme_bw()
  
  print(overdisp) # Print the plot for each Season
}


# Now mid seedlings
df_2 <- df %>%
  dplyr::select(Property, Season, euc.mid) %>%
  group_by(Property, Season) %>%
  summarise(mean = mean(euc.mid, na.rm = TRUE),
            var = var(euc.mid, na.rm = TRUE))


# Plot mean values on x and variance on y for each Season
for (i in unique(df_2$Season)) {
  overdisp <- df_2 %>%
    dplyr::filter(Season == i) %>%
    ggplot(aes(x = mean, y = var)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    labs(title = paste("Mean vs Variance for", i),
         x = "Mean euc.mid",
         y = "Variance") +
    theme_bw()
  
  print(overdisp) # Print the plot for each Season
}


# Now tall seedlings
df_2 <- df %>%
  dplyr::select(Property, Season, euc.tall) %>%
  group_by(Property, Season) %>%
  summarise(mean = mean(euc.tall, na.rm = TRUE),
            var = var(euc.tall, na.rm = TRUE))


# Plot mean values on x and variance on y for each Season
for (i in unique(df_2$Season)) {
  overdisp <- df_2 %>%
    dplyr::filter(Season == i) %>%
    ggplot(aes(x = mean, y = var)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    labs(title = paste("Mean vs Variance for", i),
         x = "Mean euc.tall",
         y = "Variance") +
    theme_bw()
  
  print(overdisp) # Print the plot for each Season
}



# Finally, all seedlings
df_2 <- df %>%
  dplyr::select(Property, Season, euc.small, euc.mid, euc.tall) %>%
  group_by(Property, Season) %>%
  mutate(tot.euc = euc.small + euc.mid + euc.tall) %>% 
  summarise(mean = mean(tot.euc, na.rm = TRUE),
            var = var(tot.euc, na.rm = TRUE))


# Plot mean values on x and variance on y for each Season
for (i in unique(df_2$Season)) {
  overdisp <- df_2 %>%
    dplyr::filter(Season == i) %>%
    ggplot(aes(x = mean, y = var)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    labs(title = paste("Mean vs Variance for", i),
         x = "Mean tot.euc",
         y = "Variance") +
    fig
  
  print(overdisp) # Print the plot for each Season
}







      ### Data wrangling
# Date will not be needed.
# I'll add a new variable for total number of seedlings.
df <- raw %>% 
  # select(-Date) %>% 
  mutate(tot.euc = euc.small + euc.mid + euc.tall) %>% 
  mutate(tot.Ex = Ex.An.Grass + Ex.An.Herb + Ex.Per.Herb + Ex.Per.Grass 
         + Ex.Shrub) %>%
  mutate(tot.Na = Na.Per.Fern + Na.Per.Grass + Na.Per.Herb + Na.Per.Gram 
         + Na.Shrub)



    ### Mean counts for small, mid and tall each season.
# Step 1: Calculate mean counts per property and season
property_means <- df %>%
  group_by(Property, Season) %>%
  summarize(
    mean_euc_small = mean(euc.small, na.rm = TRUE),
    mean_euc_mid = mean(euc.mid, na.rm = TRUE),
    mean_euc_tall = mean(euc.tall, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Aggregate these means across all properties for each season
season_means <- property_means %>%
  group_by(Season) %>%
  summarize(
    mean_euc_small = mean(mean_euc_small),
    mean_euc_mid = mean(mean_euc_mid),
    mean_euc_tall = mean(mean_euc_tall)
  ) %>% 
  ungroup()

long_df <- season_means %>% 
  pivot_longer(cols = c(mean_euc_small, mean_euc_mid, mean_euc_tall),
               names_to = "seedling",
               values_to = "mean.count")


long_df$seedling <- factor(long_df$seedling, levels = c("mean_euc_small", "mean_euc_mid", "mean_euc_tall"))

# Plotting
ggplot(long_df, aes(x = Season, y = mean.count, group = seedling, color = seedling)) +
  geom_line() +  # Ensure that lines are drawn for each seedling type across seasons
  geom_point() +
  labs(x = "Season", y = "Mean Count", color = "Seedling Type") +
  theme_bw()
######################



# What factors influence the number of seedlings?










