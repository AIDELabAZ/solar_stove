#Functions for data cleaning/reading and anonymizing ingredient data====

#Reading and cleaning all week, day, meal, ingredient data ####
read_and_clean_sheet <- function(file, sheet_name) {
  data <- read_excel(file, sheet = sheet_name)
  data <- mutate_all(data, as.character) # Convert all columns to character
  colnames(data) <- make.names(colnames(data), unique = TRUE) # Ensure unique column names
  return(data)
}

excel_files  <- list.files(path = "D:/2 Bioversity/spia/2024/runs_files_2024/ingridient_dish_meal_records", 
                           full.names = TRUE, 
                           pattern = ("\\.xlsx$"))



# Function to simplify figure creation
figure_box_plot <- function(data, xlabel, ylabel, fill_label, name) {
  # Create the plot
  hh_data_distribution <- ggplot(data, aes_string(x=xlabel, y=ylabel, fill=fill_label)) +
    geom_boxplot(notch = TRUE) +
    geom_violin(trim = FALSE, fill = NA) +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) +
    xlab(name) +
    ylab("") +
    theme(legend.position = "none")
  
  # Return the plot
  return(hh_data_distribution)
}


# Fit the felm model




robust_test <- coeftest(model, vcov = robust_se)

tidy_results <- tidy(robust_test)



# Function to run the felm model with treatment, covariates, and fixed effects
run_felm_model_EHW_se <- function(data, outcome_var, treatment_var, covariates, fixed_effects) {
  
  # Ensure the response variable and village_cor_col are in the data
  if (!all(c(response_var, village_cor_col) %in% colnames(data))) {
    stop("Response variable or village correlation column not found in data")
  }
  
  # Create the formula dynamically
  covariates_str <- paste(covariates, collapse = " + ")
  fixed_effects_str <- paste(fixed_effects, collapse = " + ")
  formula_str <- paste(outcome_var, "~", treatment_var, "+", covariates_str, "|", fixed_effects_str)
  formula <- as.formula(formula_str)
  
  # Debugging: Print or inspect formula_str
  print("Generated Formula:")
  print(formula_str)
  
  # Print the first few rows of data for debugging
  cat("First few rows of data:\n")
  print(head(data))
  
  # Fit the felm model
  model <- tryCatch({
    felm(formula, data = data)
  }, error = function(e) {
    message("Error fitting the model: ", conditionMessage(e))
    return(NULL)
  })
  
  if (is.null(model)) {
    stop("Model fitting failed.")
  }
  
  # Print model summary
  print(summary(model))
  # Calculate robust standard errors
  robust_se <- tryCatch({
    vcovHC(model, type = "HC1")
  }, error = function(e) {
    message("Error calculating robust standard errors: ", conditionMessage(e))
    return(NULL)
  })
  
  if (!is.null(robust_se)) {
    robust_test <- coeftest(model, vcov = robust_se)
    tidy_results <- tidy(robust_test)}
}



# Define a custom labeller function to wrap text and use custom labels
wrap_text <- function(labels, width = 10) {
  lapply(labels, function(label) {
    custom_label <- custom_labels[label]
    paste(strwrap(custom_label, width = width), collapse = "\n")
  })
}


#winsorize each column ====
winsorize_column <- function(x, probs = c(0.05, 0.95)) {
  return(Winsorize(x, probs = probs, na.rm=TRUE))
}
