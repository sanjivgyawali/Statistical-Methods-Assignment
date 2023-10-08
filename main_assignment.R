# importing data
library(tidyverse)
library(lubridate)
library(ggplot2)

#  TASK 1

# Loading data
  data <- read.csv("./customer_shopping_data.csv")

# Print if there are any missing data
  print(colSums(is.na(data)))

# Data Processing
  ## Convert 'invoice_date' to Date type and 'price' to numeric
  data$invoice_date <- as.Date(data$invoice_date, format="%d/%m/%Y")
  data$price <- as.numeric(data$price)
  data$sales <- data$price * data$quantity

## Convert text data to numerical form
  data$payment_method_encoded <- as.numeric(as.factor(data$payment_method))
  data$shopping_mall_encoded <- as.numeric(as.factor(data$shopping_mall))
  data$category_encoded <- as.numeric(as.factor(data$category))
  data$gender_numeric <- as.numeric(as.factor(data$gender))


## Calculate total sales and create daily total sales data
  data$sales <- data$price * data$quantity
  daily_sales <- data %>% 
    group_by(date = invoice_date) %>% 
    summarise(
      sales = sum(sales),
      avg_age = mean(age)
    )


#  TIME SERIES
  
  ### Total sales
  #### Plot daily total sales
  ggplot(daily_sales, aes(x=date, y=sales)) +geom_line(color="red") +labs(title="Daily Total Sales",x="Date",y="Total Sales ($)") +theme_minimal()
  
  ### Ts Age
  #### Plot daily average age
  ggplot(daily_sales, aes(x=date, y=avg_age)) + geom_line(color="green") + labs(title="Daily Average Age of Customers", x="Date",y="Average Age") +theme_minimal()
  
  ### Gender
  daily_gender_count <- data %>%  group_by(date = invoice_date, gender) %>% summarise(trans_count = n())
  
  #### Plot daily transaction count by gender
  ggplot(daily_gender_count, aes(x=date, y=trans_count, color=gender)) + geom_line() + labs(title="Daily Transaction Count by Gender",x="Date",y="Transaction Count") +theme_minimal()
  
  ###category 
  
  # Daily Transaction Count by Category
  daily_category_count <- data %>%  group_by(date = invoice_date, category) %>% summarise(trans_count = n())
  
  # Plot daily transaction count by category
  ggplot(daily_category_count, aes(x=date, y=trans_count, color=category)) +geom_line() + labs(title="Daily Transaction Count by Category",x="Date",y="Transaction Count") + theme_minimal()
  
  ### payment 
  
  # Time series plot: Daily Transaction Count by Payment Method
  daily_payment_count <- data %>% group_by(date = invoice_date, payment_method) %>% summarise(trans_count = n())
  
  # Plot daily transaction count by payment method
  ggplot(daily_payment_count, aes(x=date, y=trans_count, color=payment_method)) + geom_line() + labs(title="Daily Transaction Count by Payment Method", x="Date", y="Transaction Count") + theme_minimal()
  
  ### SHOPPING MALL
  
  # Time series plot: Daily Transaction Count by Shopping Mall
  daily_mall_count <- data %>%  group_by(date = invoice_date, shopping_mall) %>% summarise(trans_count = n())
  
  # Plot daily transaction count by shopping mall
  ggplot(daily_mall_count, aes(x=date, y=trans_count, color=shopping_mall)) + geom_line() + labs(title="Daily Transaction Count by Shopping Mall", x="Date", y="Transaction Count") +theme_minimal()
  

# 1.2 
## DISTRUBUTION OF DATA
  
  # Plotting
  par(mfrow=c(3, 1), mar=c(4, 4, 2, 2), oma=c(0, 0, 2, 0))
  
  # Plot distribution of quantity
  hist(data$quantity, breaks=30, col=rgb(0.2,0.8,0.5,0.5), main="Distribution of Quantity", xlab="Quantity", ylab="Frequency")
  
  # Plot distribution of price
  hist(data$price, breaks=30, col=rgb(0.2,0.5,0.8,0.5), main="Distribution of Price", xlab="Price ($)", ylab="Frequency")
  
  # Plot distribution of total sales
  hist(data$sales, breaks=30, col=rgb(0.8,0.2,0.5,0.5), main="Distribution of Total Sales", xlab="Total Sales ($)", ylab="Frequency")
  
  # Adding overall title
  title("Distribution Plots for Sales Data",outer=TRUE)

# 1.3 Scatter view

  ## invoice date
  ggplot(data, aes(x=invoice_date, y=quantity)) +geom_point(alpha=0.5, color="red") +labs(title="Scatter Plot: Quantity vs. invoice_date",x="invoice_date",y="Quantity") +theme_minimal()
  
  ## Sales data
  ggplot(data, aes(x=sales, y=quantity)) + geom_point(alpha=0.5, color="red") + labs(title="Scatter Plot: Sales vs. quantity", x="Sales", y="Quantity") +  theme_minimal()
  
  ## age
  ggplot(data, aes(x=age, y=quantity)) + geom_point(alpha=0.5, color="red") + labs(title="Scatter Plot: Quantity vs. Age", x="Age", y="Quantity") + theme_minimal()
  
  ## payment 
  ggplot(data, aes(x = payment_method_encoded, y = quantity, color=payment_method)) +geom_point(alpha = 0.5) +labs(title = "Scatter Plot:  Quantity vs Payment Method",x = "Payment Method (Encoded)",y = "Quantity") +theme_minimal()
  
  ## shopping mall
  ggplot(data, aes(x = shopping_mall_encoded, y = quantity,color =shopping_mall )) +geom_point(alpha = 0.5) +labs(title = "Scatter Plot: Quantity vs Shopping Mall",x = "Shopping Mall (Encoded)",y = "Quantity") +theme_minimal()
  
  ## category
  ggplot(data, aes(x = category_encoded, y = quantity, color = category )) + geom_point(alpha = 0.5) + labs(title = "Scatter Plot: Quantity vs Category", x = "Category (Encoded)",y = "Quantity") +theme_minimal()
  
#Task 2
  
  
  
  #Task 2#
  
  
  x <- data
  X <- x
  
  #x$X1 <- x$age
  #x$X2 <- x$category
  #x$X3 <- x$price
  #x$X4 <- x$payment_method
  
  #x <- x[, c("X1", "X2", "X3", "X4")]
  
  #x <- as.matrix(x)
  y <- as.matrix(data$quantity)
  y
  
  
  head(x)
  
  
  df <- data.frame(
    x1 = x[, "age"],
    x2 = x[, "category_encoded"],
    x3 = x[, "price"],
    x4 = x[, "payment_method_encoded"],
    y = x[, "quantity"]
  )
  df[1:5]
  
  
  
  thetaHat <- function(model, y){
    return(solve(t(model) %*% model) %*% t(model) %*% y)
  }
  
  x
  
  model1 <- lm(y ~ poly(x4, 1, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
                 poly(x2, 4, raw = TRUE) + poly(x1, 4, raw = TRUE), data = df)
  
  model2 <- lm(y ~ poly(x4, 1, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)
  
  model3 <- lm(y ~ poly(x3, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)
  
  model4 <- lm(y ~ poly(x2, 1, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)
  
  model5 <- lm(y ~ poly(x4, 1, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
                 poly(x3, 4, raw = TRUE), data = df)
  
  
  estimated_parameters_list <- list(
    Model1 = coef(model1),
    Model2 = coef(model2),
    Model3 = coef(model3),
    Model4 = coef(model4),
    Model5 = coef(model5)
  )
  
  
  extract_coefficients <- function(parameters) {
    coef_list <- list()
    coef_list$θ1 <- parameters["poly(x4, 4, raw = TRUE)1"]
    coef_list$θ2 <- parameters["poly(x1, 3, raw = TRUE)1"]
    coef_list$θ3 <- parameters["poly(x3, 4, raw = TRUE)1"]
    coef_list$θ4 <- parameters["poly(x2, 2, raw = TRUE)1"]
    coef_list$θbias <- parameters["(Intercept)"]
    return(coef_list)
  }
  
  
  coefficients_df <- data.frame(
    Model = character(0),
    θ1 = numeric(0),
    θ2 = numeric(0),
    θ3 = numeric(0),
    θ4 = numeric(0),
    θbias = numeric(0)
  )
  
  
  for (model_name in names(estimated_parameters_list)) {
    parameters <- estimated_parameters_list[[model_name]]
    coefficients <- extract_coefficients(parameters)
    
    # Add coefficients to the DataFrame
    coefficients_df <- rbind(coefficients_df, cbind(Model = model_name, as.data.frame(t(coefficients))))
  }
  
  print(estimated_parameters_list$Model1)
  print(estimated_parameters_list$Model2)
  print(estimated_parameters_list$Model3)
  print(estimated_parameters_list$Model4)
  print(estimated_parameters_list$Model5)

  
  rss_values <- c(
    sum(model1$residuals^2),
    sum(model2$residuals^2),
    sum(model3$residuals^2),
    sum(model4$residuals^2),
    sum(model5$residuals^2)
  )

  rss_df <- data.frame(
    Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
    RSS = rss_values
  )
  
  
  rss_df
  
  calculate_log_likelihood <- function(model) {
    n <- length(model$residuals)
    sigma_sq <- sum(model$residuals^2) / (n - length(model$coefficients))
    log_likelihood <- -n/2 * log(2 * pi * sigma_sq) - sum(model$residuals^2) / (2 * sigma_sq)
    return(log_likelihood)
  }
  
  
  
  log_likelihood_values <- c(
    calculate_log_likelihood(model1),
    calculate_log_likelihood(model2),
    calculate_log_likelihood(model3),
    calculate_log_likelihood(model4),
    calculate_log_likelihood(model5)
  )
  
  
  log_likelihood_df <- data.frame(
    Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
    LogLikelihood = log_likelihood_values
  )
  print(log_likelihood_df)
  
  
  
  
  aic_values <- c(
    AIC(model1),
    AIC(model2),
    AIC(model3),
    AIC(model4),
    AIC(model5)
  )
  
  
  
  
  aic_df <- data.frame(
    Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
    AIC = aic_values
  )
  aic_df
  
  
  bic_values <- c(
    BIC(model1),
    BIC(model2),
    BIC(model3),
    BIC(model4),
    BIC(model5)
  )
  
  
  
  bic_df <- data.frame(
    Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
    BIC = bic_values
  )
  bic_df
  
  
  predictions1 <- predict(model1)
  predictions2 <- predict(model2)
  predictions3 <- predict(model3)
  predictions4 <- predict(model4)
  predictions5 <- predict(model5)
  
  
  
  errors1 <- df$y - predictions1
  errors2 <- df$y - predictions2
  errors3 <- df$y - predictions3
  errors4 <- df$y - predictions4
  errors5 <- df$y - predictions5
  
  
  error_list <- list(errors1, errors2, errors3, errors4, errors5)
  
  
  
  plot_qq <- function(errors, model_name) {
    qqnorm(errors, main = paste("Q-Q Plot for", model_name))
    qqline(errors, col = "red")
  }
  
  
  
  layout(matrix(1:5, nrow = 1))
  
  
  for (i in 1:5) {
    plot_qq(error_list[[i]], model_name = paste("Model", i))
  }
  
  
  mean_errors <- c(mean(errors1), mean(errors2), mean(errors3), mean(errors4), mean(errors5))
  
  
  result_table <- data.frame(
    Model = paste("Model", 1:5),
    Mean_Error = mean_errors,
    AIC = aic_values,
    BIC = bic_values,
    Likelihood = log_likelihood_values
  )
  result_table
  
  
  ### Task 2.6 ###
  # Set the seed for reproducibility
  set.seed(123)
  
  # Split the data into training and testing datasets (70% train, 30% test)
  train_index <- sample(1:nrow(df), 0.7 * nrow(df))
  train_data <- df[train_index, ]
  test_data <- df[-train_index, ]
  
  
  # Fit the "best" model (Model 4) using the training data
  best_model <- lm(y ~ poly(x2, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = train_data)
  
  
  # Predictions on the testing data
  predictions <- predict(best_model, newdata = test_data, interval = "prediction", level = 0.95)
  
  
  
  # Create a data frame to store results
  results <- data.frame(
    x1 = test_data$x1,
    x2 = test_data$x2,
    x3 = test_data$x3,
    y_true = test_data$y,
    y_pred = predictions[, 1],  # Predicted values
    lower_bound = predictions[, 2],  # Lower bound of the prediction interval
    upper_bound = predictions[, 3]   # Upper bound of the prediction interval
  )
  results
  
  
  plot(results)
  
  
  # Create a scatterplot of the testing data points with prediction intervals
  ggplot(results, aes(x = x3, y = y_true)) +
    geom_point() +
    geom_line(aes(x = x3, y = y_pred), color = "blue", linewidth = 1) +
    geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "red", size = 1) +
    ggtitle("Model 4: Testing Data vs. Predictions with 95% Prediction Intervals") +
    xlab("x3 (Price)") +
    ylab("Total Sales Quantity")
  
  
# task 3
  
  
  numbers <- c(estimated_parameters_list$Model4)
  sorted_numbers <- sort(abs(numbers), decreasing=TRUE)
  largest_two_values <- sorted_numbers[1:2]
  
  # Choosing parameters
  thetabias <- largest_two_values[1] 
  thetafour <- largest_two_values[2]
  
  # Initial values
  arr_1 <- 0
  arr_2 <- 0
  f_value <- 0
  s_value <- 0
  

  #values from thetahat
  thetebias <- 0.483065688 #selected parameter
  thetaone <-0.143578928 # selected parameter
  thetatwo <- 0.010038614 # constant value
  thetathree <- 0.001912836 # constant value
  
  
  RSS_Model <- df[df$Model == "Model 4", "RSS"]
  
  Epison <- RSS_Model * 2 ## fixing value of eplision
  num <- 100 #number of iteration
  ##Calculating Y-hat for performing rejection ABC
  counter <- 0
  for (i in 1:num) {
    range1 <- runif(1,-0.483065688,0.483065688) # calculatchaining the range
    range1
    range2 <- runif(1,-0.143578928,0.143578928)
    New_thetahat <- matrix(c(range1,range2,thetatwo,thetathree))
    New_Y_Hat <- Y2 %*% New_thetahat ## calculating new Y-hat
    new_RSS <- sum((y-New_Y_Hat)^2)
    new_RSS
    if (new_RSS > Epison){
      arr_1[i] <- range1
      arr_2[i] <- range2
      counter = counter+1
      f_value <- matrix(arr_1)
      s_value <- matrix(arr_2)
    }
  }
  hist(f_value)
  hist(s_value)
  
  ###ploting Joint and Marginal Posterior Distribution of the graph
  plot(f_value,s_value, col = c("brown", "blue"), main = "Joint and Marginal Posterior Distribution")
  par(mfrow=c(1,1))
  
  
  