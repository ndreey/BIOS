library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(car)

# Read the table
raw_df <- read.table("00_DATA/butterflies.csv", sep=",", header=TRUE)

#### Some quick info of the dataframe  ####
dim(raw_df)   # 287 rows, 8 columns
glimpse(raw_df)
head(raw_df)

colnames(raw_df)
# [1] "LarvalID"        "LarvalHost"      "Sex"            
# [4] "MaternalHost"    "MotherID"        "DevelopmentTime"
# [7] "AdultWeight"     "GrowthRate" 

# There are two unique host plants, Barbarea and Berteroa.
unique(raw_df[,2])

# The maternal host plant did not decide if the imago butterfly would ovideposit
# on the same host plant.
unique(raw_df[,c(2,4)])

# Factorizing LarvalHost and MaternalHost
raw_df[,2] <- factor(raw_df[,2], levels=c("Barbarea", "Berteroa"))
raw_df[,4] <- factor(raw_df[,4], levels=c("Barbarea", "Berteroa"))



# Generate a new column stating if there is a generational same host plant
# As well to factorize and give levels.
df <- raw_df %>%
  mutate(GenHost = if_else(MaternalHost == LarvalHost, "gen", "diff")) %>% 
  mutate(GenHost = factor(GenHost, levels=c("gen", "diff"))) %>% 
  select(GenHost, DevelopmentTime, AdultWeight, GrowthRate)

# Checking if there are any NA's, which there were not
sum(is.na(df))

# There are 154 with generational same host plants
# There are 133 with different host plants
summary(df)


# Scatterplot to get an overview of the data.
scat_dev <- df %>% 
  ggplot(aes(GenHost, DevelopmentTime, color=GenHost)) +
  geom_jitter() +
  theme_bw() +
  theme(legend.position = "none", legend.title=element_blank()) +
  labs(x=NULL)

scat_aw <- df %>% 
  ggplot(aes(GenHost, AdultWeight, color=GenHost)) +
  geom_jitter() +
  theme_bw() +
  theme(legend.position = "none", legend.title=element_blank()) +
  labs(x=NULL)

scat_gr <- df %>% 
  ggplot(aes(GenHost, GrowthRate, color=GenHost)) +
  geom_jitter() +
  theme_bw() +
  theme(legend.position = "none", legend.title=element_blank()) +
  labs(x=NULL)

# Plot them together
ggarrange(scat_dev, scat_aw, scat_gr, nrow=1, ncol=3, common.legend = TRUE,
          legend="none")


# Add the residuals columns
df <- df %>% 
  mutate(Dev.res = residuals(aov(DevelopmentTime ~ GenHost, data = df))) %>% 
  mutate(AW.res = residuals(aov(AdultWeight ~ GenHost, data = df))) %>% 
  mutate(GR.res = residuals(aov(GrowthRate ~ GenHost, data = df)))


## Checking assumptions for the dependent variables

# Histogram   number of bins = sqrt(nrows) ~ 14
# Setting a color palette
pal <- brewer.pal(3,"Set2")

hist_dev <- df %>%
  ggplot(aes(x=Dev.res)) +
  geom_histogram(aes(y=..density..),bins=14, fill=pal[3], color="black",
                 alpha=0.8, position = "identity") +
  geom_density(lwd = 0.2, colour=1) +
  theme_bw() +
  labs(fill="")

hist_aw <- df %>%
  ggplot(aes(x=AW.res)) +
  geom_histogram(aes(y=..density..),bins=14, fill=pal[3], color="black",
                 alpha=0.8, position = "identity") +
  geom_density(lwd = 0.2, colour=1) +
  theme_bw() +
  labs(fill="")

hist_gr <- df %>%
  ggplot(aes(x=GR.res)) +
  geom_histogram(aes(y=..density..),bins=14, fill=pal[3], color="black",
                 alpha=0.8, position = "identity") +
  geom_density(lwd = 0.2, colour=1) +
  theme_bw() +
  labs(fill="")

# Plot them together
ggarrange(hist_dev, hist_aw, hist_gr, nrow=1, ncol=3, common.legend = TRUE,
          legend="none")


# QQ-plot
qq_dev <- df %>% 
  ggplot(aes(sample = Dev.res)) +
  stat_qq() +
  stat_qq_line(col=1) +
  theme_bw() +
  labs(x= "Dev.res")

qq_aw <- df %>% 
  ggplot(aes(sample = AW.res)) +
  stat_qq() +
  stat_qq_line(col=1) +
  theme_bw() +
  labs(x= "AW.res")

qq_gr <- df %>% 
  ggplot(aes(sample = GR.res)) +
  stat_qq() +
  stat_qq_line(col=1) +
  theme_bw() +
  labs(x= "GR.res")

# Plot them together
ggarrange(qq_dev, qq_aw, qq_gr, nrow=1, ncol=3, common.legend = TRUE,
          legend="none")


# Normality test
# Null Hypothesis is that the data comes from a normal distri.
# IF p-value < 0.05 then we reject null, hence only AW was norm.distri
shapiro.test(df$Dev.res)  # p-value = 5.715e-13
shapiro.test(df$AW.res)   # p-value = 0.6862
shapiro.test(df$GR.res)   # p-value = 4.752e-09

# Homogeneity of variance
# Using the function leveneTest() from the car package we can check if the 
# variance is homo- or heteroskedastic.
# Null hypothesis is that the variance is homoskedastic.
leveneTest(AdultWeight ~ GenHost, data = df) # p-value = 0.7102

## The only variable that meet the assumptions was AdultWeight.



# ANOVA

# Generate some summary statistics
# They are very similar
group_by(df, GenHost) %>%
  summarise(
    mean = mean(AdultWeight, na.rm = TRUE),
    sd = sd(AdultWeight, na.rm = TRUE),
    var = var(AdultWeight, na.rm = TRUE)
  )

# One-way ANOVA
oneway.test(AdultWeight ~ GenHost, data = df, var.equal = TRUE)




# Most basic violin chart
ggplot(df, aes(x=GenHost, y=AdultWeight, fill=GenHost)) + 
  geom_violin() +
  geom_boxplot(width=0.1, color = "black", fill = "white", alpha = 0.7) +
  scale_fill_brewer(palette="Set2") +
  theme_bw()

ggplot(df, aes(x=GenHost, y=DevelopmentTime, fill=GenHost)) + 
  geom_violin() +
  geom_boxplot(width=0.1, color = "black", fill = "white", alpha = 0.7) +
  scale_fill_brewer(palette="Set2") +
  theme_bw()


ggplot(df, aes(x=GenHost, y=GrowthRate, fill=GenHost)) + 
  geom_violin() +
  geom_boxplot(width=0.1, color = "black", fill = "white", alpha = 0.7) +
  scale_fill_brewer(palette="Set2") +
  theme_bw()





























