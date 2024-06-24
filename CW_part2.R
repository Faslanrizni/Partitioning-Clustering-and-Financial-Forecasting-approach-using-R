# Load required libraries
library(nnet)
library(caret)
library(readxl)
library(ggplot2)

install.packages("nnet")
install.packages("readxl")
install.packages("ggplot2")
install.packages("caret")




# Step 1 - Data Collection
ExchangeUSD_2_ <- read_excel("E:/sem4/ML/CW/ExchangeUSD (2).xlsx")
View(ExchangeUSD_2_)
str(ExchangeUSD_2_)

summary(ExchangeUSD_2_)

exchange_rate <- ExchangeUSD_2_[[3]] # Select only the 3rd column (USD/EUR exchange rate)
exchange_rate


# Step 2 - Exploring and Preparing the Data 

normalize <- function(x) {
  if (is.numeric(x)) {
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(x)  # Return non-numeric columns as they are
  }
}

ExchangeUSD_2_norm <- as.data.frame(lapply(ExchangeUSD_2_, normalize))


summary(ExchangeUSD_2_norm )   # Check the summary for all normalized variables vs the previous summary

# Step 3 - Training a Model on the Data (using the neuralnet function)
# Split data into training and testing sets
training_data <- exchange_rate[1:400]
View(training_data)
testing_data <- exchange_rate[401:500]
testing_data


library(neuralnet)

## Loading required package: grid
## Loading required package: MASS

library(grid)
library(MASS)


# Task b) Experiment with various input vectors and construct I/O matrices
# Function to create input/output matrix for AR approach



create_input_output <- function(data, delay) {
  input <- matrix(NA, nrow = length(data) - delay, ncol = delay)
  output <- data[(delay + 1):length(data)]
  for (i in 1:delay) {
    input[, i] <- data[(delay - i + 1):(length(data) - i)]
  }
  return(list(input = input, output = output))
}


# Experiment with various time delays

# way one
time_delays <- 1:4 # Up to (1 to 4) 
io_matrices <- lapply(time_delays, function(delay) {
  io_matrix <- create_input_output(exchange_rate, delay)
  return(io_matrix)
})

# way 2
delay_values <- c(1,2,3,4)
io_matrices <- lapply(delay_values, function(delay) create_input_output(as.vector(training_data),delay))

# Print the first input/output matrix to check
print(io_matrices[[1]])


# Task c) Normalize data
# Function to normalize data
normalize_data <- function(data) {
  normalized_data <- scale(data)
  return(normalized_data)
}


training_input <- lapply(io_matrices, function(input) input$input)
training_output <- lapply(io_matrices, function(input) input$output)

# Task d) Train MLP models and evaluate testing performance
# Function to train MLP model
# Train MLP models and evaluate testing performance
# Function to train MLP model

train_mlp <- lapply(io_matrices, function(input) {
  lapply(c(5, 10, 15), function(size) {
    nnet(training_input[[1]], training_input[[1]], size = size,decay = 1e-5,maxit=1000,linout=TRUE)
  })
})
# Flatten the list of models
models_list <- unlist(train_mlp, recursive = FALSE)
models_list

str(models_list)

# Check the number of elements in models_list
length(models_list)

# Function to evaluate MLP model
evaluate_mlp <- lapply(models_list, function(model) {
  lapply(io_matrices, function(input) {
    predictions <- predict(model, input$input)
    output <- input$output
    rmse <- sqrt(mean((predictions - output)^2))
    mae <- mean(abs(predictions - output))
    mape <- mean(abs((predictions - output) / output)) * 100
    smape <- mean(200 * abs(predictions - output) / (abs(predictions) + abs(output)))
    return(list(rmse = rmse, mae = mae, mape = mape, smape = smape))
  })
})

# Combine the evaluation results into a single data frame
evaluation_results <- data.frame(
  Hidden_Layers = rep(c(1, 2), each = 3 * length(io_matrices)),
  Nodes = rep(c(5, 10, 15), each = length(io_matrices)),
  Input_Delay = rep(delay_values, times = 6),
  Activation_Function = rep(c("logistic", "tanh"), each = 3 * length(io_matrices)),
  RMSE = unlist(lapply(evaluate_mlp, function(x) sapply(x, function(y) y$rmse))),
  MAE = unlist(lapply(evaluate_mlp, function(x) sapply(x, function(y) y$mae))),
  MAPE = unlist(lapply(evaluate_mlp, function(x) sapply(x, function(y) y$mape))),
  sMAPE = unlist(lapply(evaluate_mlp, function(x) sapply(x, function(y) y$smape)))
)

# Print the evaluation results
print(evaluation_results)





# Task e) Explanation of statistical indices
# RMSE: Root Mean Squared Error - Measures the square root of the average of squared differences between predicted and actual values.
# MAE: Mean Absolute Error - Measures the average of absolute differences between predicted and actual values.
# MAPE: Mean Absolute Percentage Error - Measures the average percentage difference between predicted and actual values.
# sMAPE: Symmetric Mean Absolute Percentage Error - A symmetric version of MAPE that avoids issues with division by zero.

# Task f) Create comparison table of testing performances
# Define a function to generate a description of the MLP structure
generate_description <- function(hidden_layers, nodes, activation_func, input_delay) {
  description <- paste(hidden_layers, "Hidden Layer(s),", nodes, "Node(s),", 
                       activation_func, "Activation,", input_delay, "Input Delay")
  return(description)
}

# Initialize a list to store the results
evaluation_results_with_desc <- list()

# Combine the evaluation results into a single data frame with descriptions
evaluation_results_with_desc_df <- data.frame(
  Hidden_Layers = evaluation_results$Hidden_Layers,
  Nodes = evaluation_results$Nodes,
  Input_Delay = evaluation_results$Input_Delay,
  Activation_Function = evaluation_results$Activation_Function,
  RMSE = evaluation_results$RMSE,
  MAE = evaluation_results$MAE,
  MAPE = evaluation_results$MAPE,
  sMAPE = evaluation_results$sMAPE
)

selected_results <- evaluation_results_with_desc_df[1:12, ]

# Add a column for NN structure description=====================
selected_results$NN_Description <- sapply(1:nrow(selected_results), function(i) {
  generate_description(selected_results[i, "Hidden_Layers"], 
                       selected_results[i, "Nodes"],
                       selected_results[i, "Activation_Function"],
                       selected_results[i, "Input_Delay"])
})

# Print the comparison table
print(selected_results)



# Calculate total number of weights for one hidden layer
total_weights_one_hidden_layer <- (delay_values * evaluation_results$Nodes + 1) * evaluation_results$Nodes + (evaluation_results$Nodes + 1)
total_weights_one_hidden_layer

# Calculate total number of weights for two hidden layers
total_weights_two_hidden_layers <- (delay_values * evaluation_results$Nodes + 1) * evaluation_results$Nodes +(evaluation_results$Nodes + 1) * evaluation_results$Nodes + (evaluation_results$Nodes + 1)
total_weights_two_hidden_layers


# Task h) Results presentation for best MLP network

# prdeict using best mlp model


# Find the index of the model with the lowest RMSE
best_mlp_index <- which.min(evaluation_results$RMSE)

# Extract the details of the best MLP model
best_mlp_details <- evaluation_results_with_desc_df[best_mlp_index, ]

# Print the details of the best MLP model
print(best_mlp_details)





# plot actual vs predicted exchange rate
predicted_values <- predict(models_list[[1]],as.matrix(training_input[[1]]))
actual_vs_predicted<-data.frame(Actual = training_output[[1]], Predicted = predicted_values)

# Print the head of the actual vs predicted data frame
print(head(actual_vs_predicted))

dev.off()  # Reset the Graphics Device- when needed

ggplot(actual_vs_predicted, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1,color = "red") +
  labs(x = "Actual Exchange Rate", y = "Predicted Exchange Rate", title = " Actual vs Predicted Exchange Rates")

# plot RMSE vs number of neurons
rmse_vs_neurons <- data.frame(Neurons = rep(c(5, 10, 15), each = 4), RMSE = evaluation_results_with_desc_df$RMSE)


ggplot(rmse_vs_neurons, aes(x = factor(Neurons), y = RMSE)) +
  geom_boxplot() +
  labs(x = "Neurons count", y="RMSE",title="Neuron Count - RMSE")


# Density Plot of Residuals
residuals <- actual_vs_predicted$Actual-actual_vs_predicted$Predicted
ggplot(data.frame(Residuals = residuals),aes(x=Residuals))+
  geom_density(fill="green",color="red")+labs(x="Residual",y="Density",title = "Destinity plot prediction")

# time series plot - Actual vs predicted exchange rate
exchange_ts <- data.frame(
  Date = seq(as.Date("2024-04-01"),by="day",length.out=length(training_output[[1]])),
  Actual = training_output[[1]],Predicted = predicted_values
)

# Time Series Analysis of Exchange Rates
ggplot(exchange_ts, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed") +
  labs(x = "Date", y = "Exchange Rate", color = "Series", title = "Time Series Analysis of Exchange Rates")

# Calculate and print test performance metrics
test_rmse <- sqrt(mean((predicted_values - training_output[[1]])^2))
test_mae <- mean(abs(predicted_values - training_output[[1]]))
test_mape <- mean(abs((predicted_values - training_output[[1]]) / training_output[[1]])) * 100
test_smape <- mean(200 * abs(predicted_values - training_output[[1]]) / (abs(predicted_values) + abs(training_output[[1]]))) * 100

cat("Test RMSE:", test_rmse, "\n")
cat("Test MAE:", test_mae, "\n")
cat("Test MAPE:", test_mape, "\n")
cat("Test sMAPE:", test_smape, "\n")


actual_vs_predicted <- data.frame(Actual = training_output[[1]], Predicted = predicted_values)

# Plot actual vs predicted exchange rates with statistical indices annotation
ggplot(actual_vs_predicted, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Exchange Rate", y = "Predicted Exchange Rate", 
       title = "Actual vs Predicted Exchange Rates with Statistical Indices") +
  geom_text(aes(label = paste("RMSE:", round(test_rmse, 2), "\nMAE:", round(test_mae, 2), "\nMAPE:", round(test_mape, 2), "\nsMAPE:", round(test_smape, 2))), 
            x = max(actual_vs_predicted$Actual), y = min(actual_vs_predicted$Predicted), hjust = 1, vjust = 0)
