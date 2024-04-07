##################################################################################################
# Business Analytics Master Project
# MCT 4: Maximilian Drüschler, Ivana Grbus, Kim Hoffmann, Mika Sang, Wolfram Stahl
##################################################################################################


getwd()

# Installing and loading packages
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot", "corrplot")
# install packages in list
lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)

# loading data
data <- BAMP_Master_Excel_v6_WIP_corrected_


data_selected <- data %>%
  select(c(1:8, 13:14, 19:20, 25:26, 31:32, 37:38, 43:44, 49:50, 55:56, 61:62, 67:68, 73:74, 79:80, 85:86, 91:92, 97:98, 99:107))


# checking last columns 
summary(data_selected)[, 39:47]
# column 99 -> Platzhalter, keine Werte
# Sales keine Kennzahl der OSC-> brauchen wir für Korrelationsanalyse nicht

data_selected <- data_selected %>%
  select(c(1:38, 44:45))

summary(data_selected)

# creating one data frame for YTD features and one for MTD features

data_YTD <- data_selected %>%
  select(c(1:8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 39))

data_MTD <- data_selected %>%
  select(c(1:8, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 40))

# turning 'NULL' into 'NA' and changing column types to numerical


# Assuming df is your dataframe and columns_to_change are the column numbers you want to change
numerical_columns <- c(9:24) # replace with your column numbers

# for YTD dataframe
data_YTD[, numerical_columns] <- lapply(data_YTD[, numerical_columns], function(x) {
  x[x == 'NULL'] <- NA
  return(x)
})


data_YTD[, numerical_columns] <- lapply(data_YTD[, numerical_columns], function(x) {
  x <- as.numeric(x)
  return(x)
})

summary(data_YTD)

# for MTD dataframe
data_MTD[, numerical_columns] <- lapply(data_MTD[, numerical_columns], function(x) {
  x[x == 'NULL'] <- NA
  return(x)
})


data_MTD[, numerical_columns] <- lapply(data_MTD[, numerical_columns], function(x) {
  x <- as.numeric(x)
  return(x)
})

summary(data_MTD)

## Correlation analysis
# MTD
# Remove rows with missing values
MTD_no_NA <- na.omit(data_MTD)

# Standardize the variables
MTD_noNA_scaled <- scale(MTD_no_NA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD <- cor(MTD_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD <- get_upper_tri_MTD(cor_matrix_MTD)

melted_cormat_MTD <- melt(upper_tri_MTD, na.rm = TRUE)

cor_heat_MTD <- ggplot(data = melted_cormat_MTD, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()

cor_heat_MTD



# YTD
# Remove rows with missing values
YTD_no_NA <- na.omit(data_YTD)

# Standardize the variables
YTD_noNA_scaled <- scale(YTD_no_NA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD <- cor(YTD_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD <- get_upper_tri_YTD(cor_matrix_YTD)

melted_cormat_YTD <- melt(upper_tri_YTD, na.rm = TRUE)

cor_heat_YTD <- ggplot(data = melted_cormat_YTD, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()

cor_heat_YTD




# Koeffizienten + Korrelationsmatrix = normal + absoluten Werte 
# OR absolut, Profitabilität --> nicht auf Abweichung konzentrieren, da das Subjektivität reinbringt. 
# Lineare Regression versuchen