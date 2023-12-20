##################################################################################################
# Course:            Data Science for Business Managers II 
# Lecturer:          Dr. Julian Runge
# Group 4:           Maximilian Drüschler, Ivana Grbus, Kim Hoffmann, Mika Sang, Wolfram Stahl
##################################################################################################
# Individual Assignment - Day 1
# Customer lifetime value prediction

### set working environment
# add packages to the list as needed
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot",
             "randomForest")

# install packages in list
lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)
library(arrow)

# set wd
setwd("~/Desktop/PTMMA 25.nosync/Data Science for Business II/Day 1")

### read in data
data <- read_parquet("day1-data.parquet", header = TRUE)

### some data formatting
data$d30_spend <- as.numeric(data$d30_spend)
data$d14_spend <- as.numeric(data$d14_spend)
data$d7_spend <- as.numeric(data$d7_spend)
data$d3_spend <- as.numeric(data$d3_spend)
data$d1_spend <- as.numeric(data$d1_spend)
data$count_p_1 <- as.numeric(data$count_p_1)
data$max_p_2 <- as.numeric(data$max_p_2)
data$p_4 <- as.numeric(data$p_4)
data$p_5 <- as.numeric(data$p_5)
data$sum_p_6 <- as.numeric(data$sum_p_6)
data$count_p_7 <- as.numeric(data$count_p_7)
data$count_p_8 <- as.numeric(data$count_p_8)
data$count_p_9 <- as.numeric(data$count_p_9)
data$count_p_10 <- as.numeric(data$count_p_10)
data$count_p_11 <- as.numeric(data$count_p_11)
data$count_p_12 <- as.numeric(data$count_p_12)
data$count_p_13 <- as.numeric(data$count_p_13)
data$len_p_14 <- as.numeric(data$len_p_14)
data$count_p_15 <- as.numeric(data$count_p_15)
data$count_p_19 <- as.numeric(data$count_p_19)
data$count_p_20 <- as.numeric(data$count_p_20)

# impute 0 to deal with missings
data[is.na(data) == TRUE] <- 0 # error message
# impute 0 or "unknown to deal with missing values
library(dplyr)
data <- data %>% 
  mutate_all(funs(ifelse(is.na(.) & is.character(.), 'unknown', 
                         ifelse(is.na(.) & (is.numeric(.) | is.integer(.)), 0, .))))
summary(data)


# create outcome variable as d30 - d14 spend
data$y <- data$d30_spend - data$d14_spend
data$y[data$y<0] <- 0

### correlation heatmap
# select variables used in analysis for heatmap
cor_vars <- c("y","d30_spend","d14_spend","d7_spend","d3_spend","d1_spend",
              "count_p_1","max_p_2","p_4","p_5","sum_p_6","count_p_7",
              "count_p_8","count_p_9","count_p_10","count_p_11","count_p_12",
              "count_p_13","len_p_14","count_p_15","count_p_19","count_p_20")

cor_data <- data[cor_vars]

# create correlation matrix
# fill all missings with 0 to allow for correlation calculation
cor_data[is.na(cor_data) == TRUE] <- 0
cormat <- round(cor(cor_data),2)

# helper function to get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# label levels of factors to make heatmap nice to read
levels(melted_cormat$Var1)[levels(melted_cormat$Var1)=="years_since_first_spend"] <- "Years active on platform"
levels(melted_cormat$Var2)[levels(melted_cormat$Var2)=="years_since_first_spend"] <- "Years active on platform"

cor_heat <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()

cor_heat

### fit random forest
# subset data
data_sub <- data[sample(nrow(data), 30000), ]

# select x vars for 3 different models x1_vars -> new variables chosen by group, x1_vars_old -> variables in lecture, x2_vars -> without spend from lecture
x1_vars <- c("d14_spend","d7_spend","d3_spend","d1_spend",
             "count_p_1","sum_p_6","count_p_7", "count_p_8","count_p_9",
             "count_p_11","count_p_12","count_p_13","len_p_14","count_p_15")
             # not considered: "max_p_2", "p4", "p5", "count_p_10","count_p_19","count_p_20"

x1_vars_old <- c("d14_spend","d7_spend","d3_spend","d1_spend",
                 "count_p_1","max_p_2","p_4","p_5","sum_p_6","count_p_7")
            #"count_p_8","count_p_9","count_p_10","count_p_11","count_p_12",
            #"count_p_13","len_p_14","count_p_15","count_p_19","count_p_20")
        
x2_vars <- c("count_p_1","max_p_2","p_4","p_5","sum_p_6","count_p_7")
             #"count_p_8","count_p_9","count_p_10","count_p_11","count_p_12",
             #"count_p_13","len_p_14","count_p_15","count_p_19","count_p_20")

# set seed and specify cross-validation
set.seed(825)
cv_fold <- trainControl(method = "cv",
                        number = 5,
                        search = "grid")

# train random forests for all 3 models
rf_x1 <- train(as.formula(paste("y", paste(x1_vars, collapse = " + "), sep = " ~ ")), 
               data = data_sub,
               method = "ranger",
               #method = "rpart",
               trControl = cv_fold,
               metric = "RMSE",
               importance = "impurity",
               tuneLength=30)

rf_x1_old <- train(as.formula(paste("y", paste(x1_vars_old, collapse = " + "), sep = " ~ ")), 
               data = data_sub,
               method = "ranger",
               #method = "rpart",
               trControl = cv_fold,
               metric = "RMSE",
               importance = "impurity",
               tuneLength=30)

rf_x2 <- train(as.formula(paste("y", paste(x2_vars, collapse = " + "), sep = " ~ ")), 
               data = data_sub, 
               method = "ranger",
               #method = "rpart",
               trControl = cv_fold,
               metric = "RMSE",
               importance = "impurity",
               tuneLength=30)

print(rf_x1)
print(rf_x1_old)
print(rf_x2)
plot(rf_x1, main = "Model X1")
plot(rf_x1_old, main = "Model X1 Old")
plot(rf_x2, main = "Model X2")

# show results
rf_x1$results
rf_x1_old$results
rf_x2$results

### look at variable importance
print(varImp((rf_x1)))
plot(varImp((rf_x1)))
print(varImp((rf_x1_old)))
plot((varImp((rf_x1_old))))
print(varImp((rf_x2)))
plot(varImp((rf_x2)))


rf_x1_var_imp <- vip(rf_x1$finalModel,
                     #bar = FALSE,
                     size = 1.5,
                     num_features = 15)

rf_x2_var_imp <- vip(rf_x2$finalModel,
                     #bar = FALSE,
                     size = 1.5,
                     num_features = 15)

rf_x1_old_var_imp <- vip(rf_x1_old$finalModel,
                     #bar = FALSE,
                     size = 1.5,
                     num_features = 15)

vip_compare <- grid.arrange(rf_x1_var_imp,
                            rf_x1_old_var_imp,
                            nrow = 1,
                            ncol = 2)

### generate partial dependency plots with main x variable
rf_x1_par <- partial(rf_x1,
                     pred.var = c("d14_spend"),
                     chull = TRUE)

rf_x1_plot <- ggplot(rf_x1_par, aes(x=d14_spend, y=yhat)) + 
  geom_line() +
  geom_smooth(method="auto") +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("D14 Spend") +
  ylab("y = D30 Spend - D14 Spend")

rf_x1_plot

rf_x1_par <- partial(rf_x1,
                     pred.var = c("d14_spend"),
                     chull = TRUE)

rf_x1_plot <- ggplot(rf_x1_par, aes(x=d14_spend, y=yhat)) + 
  geom_line() +
  geom_smooth(method="auto") +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("D14 Spend") +
  ylab("y = D30 Spend - D14 Spend")

rf_x1_plot

rf_x2_par <- partial(rf_x2,
                     pred.var = c("count_p_1"),
                     chull = TRUE)

rf_x2_plot <- ggplot(rf_x2_par, aes(x=count_p_1, y=yhat)) + 
  geom_line() +
  geom_smooth(method="auto") +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("Count P1") +
  ylab("y = D30 Spend - D14 Spend")

### generate partial dependency plots with two main x variables
# d14_spend & d7_spend --> in final ppt presentation
rf_x1_par2 <- partial(rf_x1,
                      pred.var = c("d14_spend","d7_spend"),
                      chull = TRUE)
rf_x1_plot2 <- ggplot(rf_x1_par2, aes(x=d14_spend, y=d7_spend, z=yhat)) +
  geom_tile(aes(fill=yhat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(9,"YlGnBu")), name = "y = D30 Spend - D14 Spend", labels = NULL) +
  stat_contour(bins=10,aes(x=d14_spend, y=d7_spend, z=yhat), color="black", size=0.2) +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("D14 Spend") +
  ylab("D7 Spend")

rf_x1_plot2

# count_p_12 & d14_spend --> in final ppt presentation
rf_x1_par2 <- partial(rf_x1,
                      pred.var = c("count_p_12","d14_spend"),
                      chull = TRUE)
rf_x1_plot2 <- ggplot(rf_x1_par2, aes(x=count_p_12, y=d14_spend, z=yhat)) +
  geom_tile(aes(fill=yhat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(9,"YlGnBu")), name = "y = D30 Spend - D14 Spend", labels = NULL) +
  stat_contour(bins=10,aes(x=count_p_12, y=d14_spend, z=yhat), color="black", size=0.2) +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("Count P12") +
  ylab("D14 Spend")
rf_x1_plot2

# count_p_12 & count_p_11
rf_x1_par3 <- partial(rf_x1,
                      pred.var = c("count_p_12","count_p_11"),
                      chull = TRUE)
rf_x1_plot3 <- ggplot(rf_x1_par3, aes(x=count_p_12, y=count_p_11, z=yhat)) +
  geom_tile(aes(fill=yhat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(9,"YlGnBu")), name = "y = D30 Spend - D14 Spend", labels = NULL) +
  stat_contour(bins=10,aes(x=count_p_12, y=count_p_11, z=yhat), color="black", size=0.2) +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("Count P12") +
  ylab("Count P11")
rf_x1_plot3