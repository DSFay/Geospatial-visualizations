library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(data.table)
library(rgdal)
library(broom)


# ------------- Task 1 ---------------------
# 1. First convert the dataset from ʻwideʼ (each observation is a mother) to ʻlongʼ (each observation is a birth, with associated mother id). This will make the following tasks a lot easier. Write out the reshaped (i.e. `longʼ) version of the dataset to MotherData_reshaped_[your last name]


# import data
mothers <- read.csv("./data/MotherData.csv", header=TRUE)

# reshape data from wide to long
#   each observation is a birth associated with a mother id
mothers_l <- reshape(mothers, 
                    idvar= "caseid",
                    varying = 79:438, 
                    sep="_",
                    direction="long",
                    timevar="order")

# order data by mothers' caseid
mothers_long <- mothers_l[order(mothers_l$caseid),]

# create output file and save long version of data
dir.create("output", showWarnings = FALSE)
write.csv(mothers_long, "./output/MotherDataLong.csv", row.names = FALSE)


# ------------- Task 2 ---------------------
# 2. Construct an indicator variable for whether or not a child died at or before his/her first birthday (i.e. died at or before 12 months). So this will take on a value ==1 if the child died by age 1, 0 otherwise. How many total births and how many total deaths are in the sample?


# create dummy variable = 1 if child died on or before 1st birthday
mothers_long$diedyr_one <- ifelse(mothers_long$b7 <= 12 & !is.na(mothers_long$b7) & is.na(mothers_long$b8), 1,0)

# ensure diedyr_one = NA when both cols b7 & b8 = NA
mothers_long$diedyr_one <- ifelse(is.na(mothers_long$b7) & is.na(mothers_long$b8), NA, mothers_long$diedyr_one)

# checking total number of births and number of children who died before year one
births_info <- addmargins(table(mothers_long$diedyr_one),1)

# Printing the below frequency table summarizes the diedyr_one column
#   There are 19,644 total births and 2018 total deaths by the first birthday in the sample
births_info

# Summarizing column b7 to count total deaths in the sample
#   3144 children died in the sample
death_count <- summary(!is.na(mothers_long$b7))
death_count

# verifying the total death count is consistent with data for surviving children in column b8
#   16500 children survived in the sample 
#      adding total deaths and surviving children equals the number of births found above
       # 3144 + 16500 = 19,644
lived_count <- summary(!is.na(mothers_long$b8))
lived_count

# ------------- Task 3 ---------------------
# 3. Run a linear regression of this variable on the wealth score,and briefly interpret this regression and what it tells us about the relationship between wealth and infant death in Nigeria. What does it tell us? What does it not tell us? Please write at most 5 sentences.


# linear regression of children who died on or before their first birthday on the wealth score
reg_diedyr_one_wealth <- lm(diedyr_one~v191, data=mothers_long) 
summary(reg_diedyr_one_wealth)

# Call:
#   lm(formula = diedyr_one ~ v191, data = mothers_long)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.13818 -0.12141 -0.10185 -0.07467  0.95474 
# 
# Coefficients:
#                 Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept)    0.098237   0.002199   44.67   <2e-16 ***
#   v191        -0.024690   0.002280  -10.83   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3027 on 19642 degrees of freedom
# (107236 observations deleted due to missingness)
# Multiple R-squared:  0.005935,	Adjusted R-squared:  0.005884 
# F-statistic: 117.3 on 1 and 19642 DF,  p-value: < 2.2e-16

# Regression interpretation:
  # The results show that a one-unit increase in the wealth index is associated with
  # a decrease of about 2.47 percentage points in the probability of a child dying
  # before their first birthday in Nigeria. This wealth index coefficient is highly significant,
  # past the 1% threshold. This does not tell us how high or low levels of
  # wealth impact this probability, which would be essential to measure since the literature
  # has established that wealth effects are non-linear.

# quick peek at summary statistics for column diedyr_one
table_diedyr_one <- addmargins(table(mothers_long$diedyr_one),FUN=sum)
table_diedyr_one
#     0     1   sum 
# 17626  2018 19644 

avg_infant_mort <- mean(mothers_long$diedyr_one, na.rm = TRUE)
avg_infant_mort  # avg = 0.1027286

# ------------- Task 4 ---------------------
# 4. Imagine in the previous exercise that you had found that higher infant mortality was associated with higher wealth ‒ a finding contrary to existing literature and likely contrary to the hypothesis we might have had before running this regression. Before showing your regression results to your supervisor, what might you do to make sure you understand your results? In 5 sentences or less, describe 1-2 specific things you would do.


    # To check for outliers and for goodness-of-fit for the linear specification, I would  make a scatter plot  of the wealth index variable and the variable for child dying by their first birthday. A scatter plot would also allow me to check on the mortality effects of different wealth levels which would paint a much richer picture than the linear probability model's average effect over the entire sample.

    # I would also check the existing literature for theory on what important control variables are for estimating wealth impacts on children's health in Nigeria and rerun the regression using them, data permitting. While going through the literature I would be on the lookout for indications that there were issues with the DHS survey data within my sample's time frame, historical reasons why high income families might have experienced higher mortality, and any other explanations for why the results do not fit with prior theory.

# ------------- Task 5 ---------------------

# 5. Does the effect of wealth on infant mortality depend on the gender of the child? 
#   Please run a regression to explore this, and briefly discuss (3 sentences).


# making a dummy variable for female
mothers_long$isfemale <- ifelse(mothers_long$b4 == "female", 1,NA)
# this 2 line coding ensures isfemale = NA when b4 = NA
mothers_long$isfemale <- ifelse(mothers_long$b4 == "male", 0, mothers_long$isfemale)

# making a dummy variable for male
mothers_long$ismale <- ifelse(mothers_long$b4 == "male", 1, NA)
# second line to ensure ismale = NA when b4 = NA
mothers_long$ismale <- ifelse(mothers_long$b4 == "female", 0, mothers_long$ismale)

# regressing the wealth index interacted with gender on probability of child dying by first birthday
reg_diedyr_one_wealth_gender <- lm(diedyr_one ~ v191*isfemale, data=mothers_long)
summary(reg_diedyr_one_wealth_gender)

# Call:
#   lm(formula = diedyr_one ~ v191 * isfemale, data = mothers_long)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.15171 -0.11755 -0.09947 -0.07374  0.95724 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      0.107029   0.003053  35.060  < 2e-16 ***
#   v191          -0.027616   0.003145  -8.781  < 2e-16 ***
#   isfemale      -0.018238   0.004399  -4.146  3.4e-05 ***
#   v191:isfemale  0.006167   0.004563   1.351    0.177    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3026 on 19640 degrees of freedom
# (107236 observations deleted due to missingness)
# Multiple R-squared:  0.007043,	Adjusted R-squared:  0.006891 
# F-statistic: 46.43 on 3 and 19640 DF,  p-value: < 2.2e-16

#  Regression interpretation:
  # This regression does not show conclusive evidence that the wealth effect of mortality depends on a child's gender.Though the interaction term is not significant it suggests that increases in wealth no longer result in a lower probability of mortality when the child is female, since wealth is non-linear the interaction term should also be checked using different segments of the sample.The intercept value here shows the average probability that male children will die before their first birthday (10.703% in the sample), the isfemale coefficient suggests that female children are 1.824 percentage points less likely than males, and the v191 coefficient shows with each one unit increase in the wealth index that there is a 2.76 percentage point drop in probability of death by the first birthday.

# ------------- Task 6 ---------------------
# 6. Now letʼs say weʼre interested in village (i.e. “cluster”, equal to enumeration areas) level outcomes, averaged over all the births that happened in that cluster. Make a simple scatter plot of village-average wealth score versus village-average probability of infant death (this is just the simple average of the variable you constructed in #2, ignoring birth year). Label the axes nicely and label this Figure 1. What are average wealth and average infant mortality in the sample?


# Makings a subset of main data frame with the grouped means by village
mothers_cluster_means <- mothers_long %>% group_by(v001) %>% 
  summarise(mean_wealth=mean(v191, na.rm = TRUE),
            mean_infant_mort= mean(diedyr_one, na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()
mothers_cluster_means

# making scatter plot for village-avg wealth score vs village-avg prob of infant death
F1 <- ggplot(mothers_cluster_means, aes(x = mean_wealth, y = mean_infant_mort, color=v001)) +
geom_point(shape = 1, size = 1)
F1
# adding title and tidy axis labels to the scatter plot
F2 <- F1 + labs(title = "Figure 1: DHS Nigeria Village Average Wealth & Infant Mortality ", x = "Village-Average Wealth", y = "Village-Average Infant Mortality",
                color="Village ID") +  theme(panel.grid.major = element_line(color = "lightgray"),
                                            panel.grid.minor = element_blank(),
                                            panel.background = element_blank(),
                                            plot.background = element_blank()) + 
                                            scale_color_gradient(low = "#00FF00", high = "black")

F2
ggsave("Figure1_inf_mort_wealth_DHS_village.pdf", F2, dpi=300)

# Average infant mortality in the sample?
  # From Task 3, I found average infant mortality = 0.1027286
avg_infant_mort <- mean(mothers_long$diedyr_one, na.rm = TRUE)
avg_infant_mort 
  # Average wealth in the sample is 0.03578406 on the wealth index
avg_wealth <- mean(mothers_long$v191, na.rm = TRUE)
avg_wealth

# ------------- Task 7 ---------------------
# 7. Using the cluster-level lat-lon information in the Locations.csv file , make a simple plot of the cluster locations, overlaid on a map of Nigeria (any shapefile of Nigerian boundaries you can find online is fine for this purpose), with the plots colored according to their infant mortality rate. Label this plot figure 2.

library(sp)

# Making a map of sample village locations in Nigeria 
#  colored by infant mortality

# plotting village locations
point_villages <- read.csv("./data/Locations.csv", as.is= TRUE)
village_data_frame <- merge(mothers_cluster_means, point_villages, by="v001")

# adding in shapefile
nigeria_border <- readOGR(
  dsn= paste0(getwd(),"/Shapefile_Nigeria/") ,
  layer="NGA_adm0",
  verbose=FALSE)

# convert shapefile into a dataframe to use with ggplot2
nigeria_df <- tidy(nigeria_border)

gg_1 <- ggplot() + geom_polygon(data= nigeria_df, 
                                aes(x=long, y=lat, group=group, fill=NA), 
                                color="black", fill=NA, size=0.5)

gg_2 <- gg_1 + geom_point(data=village_data_frame, aes(x=lon, y=lat,
                                color=mean_infant_mort)) + scale_color_gradient(
                                low="yellow", high="red", name="Infant Mortality", na.value = NA) + coord_quickmap()

gg_3 <-  gg_2 + ggtitle("Figure 2: DHS Villages Nigeria") + xlab(NULL) + ylab(NULL)

gg_4 <-  gg_3 + theme(axis.line =  element_blank(),
                      axis.text =  element_blank(),
                      axis.ticks =  element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      legend.key.size = unit(0.5, 'cm'), #change legend key size
                      legend.key.height = unit(0.5, 'cm'), #change legend key height
                      legend.key.width = unit(0.5, 'cm'), #change legend key width
                      legend.title = element_text(size=8), #change legend title font size
                      legend.text = element_text(size=6)) #change legend text font size) 

ggsave("Figure2_inf_mort_DHS_Nigeria_village.pdf", gg_4, dpi=300)

# citation for shapefile used: 
  # Hijmans, R. and University of California, Berkeley, Museum of Vertebrate Zoology. 
  # (2015). Boundary, Nigeria, 2015. UC Berkeley, Museum of Vertebrate Zoology. 
  # Available at: http://purl.stanford.edu/cv573dj9896


# ---------------- Task 8 -----------------
#   Your goal now is to understand how average mortality rates in each village correlate with average temperature. Download average temperature data from WorldClim (https://www.worldclim.org/data/worldclim21.html) using the coarsest data they have (the 10m maps). Extract the average June temperature in each village, and make a scatter plot showing the relationship between average temperature (x-axis) and average mortality rate (y-axis) in each village, overlaid with a regression line relating the two variables. Label this plot as Figure 3. Make sure axes are labeled and values are legible. Briefly discuss what the scatter plot and regression line tell us about the relationship between June temperature and average mortality rate (3 sentences).


library(raster)

# Load temperature data for June
temperature <- raster("./data/wc2.1_10m_tavg/wc2.1_10m_tavg_06.tif")

village_coords <- village_data_frame[,c("lon", "lat")]

# Extract average June temperature for each village
june_temp <- extract(temperature, village_coords, method = "simple")

# Create a data frame with temperature and mortality data
temp_mort_df <- data.frame(june_temp, village_data_frame$mean_infant_mort)

# Create scatter plot with regression line
plot(june_temp, village_data_frame$mean_infant_mort)
abline(reg_temp_mort)

temp_mort_scatter <-  ggplot(temp_mort_df, aes(x = june_temp, y = village_data_frame$mean_infant_mort)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average June Temperature (°C)", y = "Average Infant Mortality") +
  ggtitle("Figure 3: DHS Nigeria Village Average Temperature & Infant Mortality") + 
  theme(panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()) 
temp_mort_scatter

ggsave("Figure3_reg_inf_mort_temp_DHS_Nigeria_village.pdf", temp_mort_scatter, dpi=300)

reg_temp_mort <- lm(village_data_frame$mean_infant_mort ~ june_temp, data=temp_mort_df)
summary(reg_temp_mort)

# Call:
#   lm(formula = village_data_frame$mean_infant_mort ~ june_temp, 
#      data = temp_mort_df)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.12014 -0.03971 -0.00750  0.03456  0.18860 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.097889   0.052379  -1.869 0.062875 .  
# june_temp    0.007049   0.001962   3.593 0.000397 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.05591 on 237 degrees of freedom
# Multiple R-squared:  0.05167,	Adjusted R-squared:  0.04767 
# F-statistic: 12.91 on 1 and 237 DF,  p-value: 0.000397


  # Figure analysis: 
  # The regression estimated in the plot indicates that for every one Celsius degree increase in average June temperature there is a  0.7049% increase in average infant mortality. However, the scatter plot shows that average June temperatures are most densely clustered between 25 & 27 degrees C where there is a wide range of mortality rates. The mortality rates are lower at lower average temperatures, but there is not enough data at the lower temperatures to determine what the relationship is.


# ---------------- Task 9 -----------------
install.packages("MASS")
library(lmtest)
library(MASS)

# 9. Is the relationship between average temperature and average mortality linear? How might you test whether the relationship is linear vs non-linear?

plot(reg_temp_mort)

# Breusch-Pagan test for homoskedasticity
bptest(reg_temp_mort)
  # studentized Breusch-Pagan test
  # 
  # data:  reg_temp_mort
  # BP = 1.8769, df = 1, p-value = 0.1707

sresid <- studres(reg_temp_mort) 
shapiro.test(sresid)
  # Shapiro-Wilk normality test
  # 
  # data:  sresid
  # W = 0.96903, p-value = 4.527e-05



  # Looking at the linear diagnostic plots (Residuals vs Leverage, Residuals vs Fitted, Normal Q-Q, Scale-Location), we can see the data clustering reflected. The data appear non linear: the variance of the data has a wide spread of variance, large outliers, and many high-leverage points with large residuals.
  # A Breusch-Pagan test for homoskedasticity shows that the errors are mostly homoskedastic, but a Shapiro-Wilk Normality test confirms that they are not normally distributed.
  # Further tests like a rainbow test, a Chow test, or a RAMSEY test could be conducted to test for linearity in the relationship. Theory suggests that there are important variables like agricultural shocks that drive the relationship between heat and infant mortality, which would be important to include in an full estimation of the impact of temperature.










