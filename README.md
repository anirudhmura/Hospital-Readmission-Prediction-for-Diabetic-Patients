# Diabetic Readmission Prediction

This R script analyzes diabetic patient data to predict hospital readmission within 30 days. It includes:

1. Data Preprocessing and Feature Engineering
   - Handles missing values
   - Encodes categorical variables
   - Groups diagnoses into broader categories

2. Random Forest Model
   - Trains a Random Forest classifier
   - Evaluates model performance
   - Visualizes feature importance

3. Logistic Regression
   - Used for feature selection

4. Neural Network Model
   - Implements a neural network using Keras and TensorFlow
   - Includes data normalization and model training

5. K-Nearest Neighbors (KNN) Classification
   - Performs cross-validation to find the optimal k value
   - Trains and evaluates the KNN model

## Requirements

- R (version 3.6.0 or higher recommended)
- Required R packages: tree, randomForest, dplyr, caret, purrr, class, ggraph, igraph, ggplot2, neuralnet, fastDummies, tensorflow, keras

## Usage

1. Ensure you have all required packages installed.
2. Set your working directory to the location of the script and data file.
3. Run the script in an R environment.

## Data

The script expects a CSV file named 'diabetic_data_og.csv' containing diabetic patient data. Ensure this file is in your working directory before running the script.

## Output

The script generates various plots and prints model performance metrics to the console. Key outputs include:
- Random Forest tree visualization
- Feature importance plot
- Neural Network model summary
- KNN model accuracy and confusion matrix

The code demonstrates various machine learning techniques for healthcare data analysis and prediction.
