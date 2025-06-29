library(ggplot2)
library(dplyr)
library(corrplot)
library(gridExtra)

#useful functions
logit <- function(x){log(x/(1-x))}
expit <- function(x){exp(x)/(1+exp(x))}

# N <- 200
# Sigma <- outer(1:10, 1:10, function(x,y) 0.5^abs(x-y))
# x <- mvrnorm(N, rep(0,10), Sigma)
#
# x[,3] <- ifelse(x[,3] > 0.5, 1, 0)
# x[,4] <- ifelse(x[,4] > 0, 1, 0)
# x[,5] <- cut(x[,5], breaks=c(-Inf, -1, 0, 1, Inf), labels = FALSE)
# x[,8] <- ifelse(x[,8] > 1, 0.5, 0)
# x[,9] <- ifelse(x[,9] > 1.5, 1, 0)
# x[,10] <- cut(x[,10], breaks=c(-Inf, -1, 0.5, 1, Inf), labels = FALSE)
# data.bin.complete <- data.frame(x)
# colnames(data.bin.complete) <- c(paste0("x", 1:5), paste0("z", 1:5))
#
# logit.py <- with(data.bin.complete,-2+x1+0.2*x1^2+
#                    0.3*x2+0.1*x2^2+0.2*(x3==2)+0.2*(x4==2)+0.2*(x5==2)-
#                    0.1*(x5==3)+0.2*(x5==4)+rnorm(N,0,0.1))
# py <- expit(logit.py)
# data.bin.complete$y <- rbinom(N,1,py)
#
# data.bin.complete[,c(3:5, 8:10, 11)] <- lapply(data.bin.complete[,c(3:5, 8:10, 11)], factor)
#
#
# data.bin.complete %>% count(data.bin.complete$y)
# saveRDS(data.bin.complete[,c("x1", "x2", "x3", "x4", "x5", "y")], 'dataset.rds')

df = readRDS('dataset.rds')
head(df)


summary(df)

colSums(is.na(df))

continuous_vars <- c("x1", "x2")
categorical_vars <- c("x3", "x4", "x5", "y")

continuous_data <- df[, continuous_vars]
sapply(continuous_data, function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Missing = sum(is.na(x)))
})


theme_custom <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11))

# Histograms for continuous variables
p1 <- ggplot(df, aes(x = x1)) +
  geom_histogram(bins = 20, fill = "skyblue", alpha = 0.7, color = "black") +
  labs(title = "Distribution of X1", x = "X1", y = "Frequency") + theme_minimal()

p2 <- ggplot(df, aes(x = x2)) +
  geom_histogram(bins = 20, fill = "lightgreen", alpha = 0.7, color = "black") +
  labs(title = "Distribution of X2", x = "X2", y = "Frequency") + theme_minimal()

# Combine plots
grid.arrange(p1, p2, ncol = 2, top = "Distribution of Continuous Variables")


# 4.2 Bar plots for categorical variables
p5 <- ggplot(df, aes(x = x3)) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of X3", x = "X3", y = "Count") + theme_custom

p6 <- ggplot(df, aes(x = x4)) +
  geom_bar(fill = "darkgreen", alpha = 0.7) +
  labs(title = "Distribution of X4", x = "X4", y = "Count") + theme_custom

p7 <- ggplot(df, aes(x = x5)) +
  geom_bar(fill = "purple", alpha = 0.7) +
  labs(title = "Distribution of X5", x = "X5", y = "Count") + theme_custom

p8 <- ggplot(df, aes(x = y)) +
  geom_bar(fill = "red", alpha = 0.7) +
  labs(title = "Distribution of Outcome Y", x = "Y", y = "Count") + theme_custom

grid.arrange(p5, p6, p7, p8, ncol = 2, top = "Distribution of Categorical Variables")


correlation(df)

result <- correlation(df)
plot(result)
corr_df = as.data.frame(result)

p10 <- ggplot(df, aes(x = y, y = x1, fill = y)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "X1 by Outcome Y", x = "Y", y = "X1") + theme_custom +
  theme(legend.position = "none")

p11 <- ggplot(df, aes(x = y, y = x2, fill = y)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "X2 by Outcome Y", x = "Y", y = "X2") + theme_custom

grid.arrange(p10, p11, ncol = 2, top = "Continuous Predictors by Outcome")





vars <- c("x3", "x4", "x5")
plot_list <- list()  # to store plots


for (var in vars) {
  p <- ggplot(df, aes_string(x = var, fill = "factor(y)")) +
    geom_bar(position = "fill") +  # stacked proportion bars
    ylab("Proportion") +
    labs(fill = "y", title = paste(var," by Outcome Y")) + theme_custom

  plot_list[[var]] <- p
}

# Arrange all plots in a 2x2 grid
grid.arrange(grobs = plot_list, ncol = 2, top = "Cetagorical Predictors by Outcome")


