#' Plot and Save Partial Effects from Model
#'
#' This function extracts significant variables from a fitted model, 
#' generates **partial effect plots**, and saves the resulting visualization.
#'
#' @param model A fitted regression model (e.g., from `spaMM::fitme`).
#' @param data The dataset used for modeling, containing predictor variables.
#' @param model_name A string representing the model name, used for saving the output file.
#'
#' @return Saves a PNG file containing partial effect plots and displays them.
#'
#' @details
#' 1. Extracts **significant variables** (`p_value < 0.01` and `|t-value| > 1.96`).
#' 2. **Groups IUCN categories** into a single variable (`iucn_cat`).
#' 3. **Renames variables** for better interpretability.
#' 4. Uses **`visreg::visreg()`** to compute partial effects.
#' 5. Creates **scatter plots for categorical variables** and **line plots with confidence bands for continuous variables**.
#' 6. Uses **`ggplot2`** for visualization with `my_custom_theme()`.
#' 7. Arranges all plots in a grid (`3 per row`) and **saves them as**:
#'    - `figures/supp/<model_name>_partial_effects.png`
#'
#' @examples
#' plot_and_save_partial_effects(mod_spamm_binomial, mpa_vessel_model, "mod_spamm_binomial")
#'

# Function to select variables and plot partial effects, then save the plot
plot_and_save_partial_effects <- function(model, data, model_name) {
  
  # Extract the summary of the model
  model_summary <- summary(model, details = list(p_value = TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    clean_names() %>%
    filter(variable != "(Intercept)") %>%  # Exclude the intercept
    filter(t_value < -1.96 | t_value > 1.96)
  
  # Adjust IUCN categories to a single variable "iucn_cat"
  model_summary <- model_summary %>%
    mutate(variable = ifelse(grepl("^iucn_cat", variable), "iucn_cat", variable)) %>%
    distinct(variable, .keep_all = TRUE)  # Remove duplicates after generalizing iucn_cat
  
  # Rename variables as per your naming convention
  model_summary <- model_summary %>%
    mutate(Variable = case_when(
      variable == "HDI" ~ "Human development index",
      variable == "iucn_cat" ~ "IUCN category",
      variable == "area_correct" ~ "MPA size", 
      variable == "depth" ~ "Depth",
      variable == "dist_to_shore" ~ "Distance to the shore",
      variable == "sd_chl" ~ "Primary productivity (standard deviation)",
      variable == "mean_chl" ~ "Primary productivity (average)",
      variable == "mean_sst" ~ "Sea surface temperature (average)",
      variable == "sd_sst" ~ "Sea surface temperature (standard deviation)",
      variable == "MarineEcosystemDependency" ~ "Marine ecosystem dependency",
      variable == "hf" ~ "Human footprint",
      variable == "gdp" ~ "Gross domestic product",
      variable == "travel_time" ~ "Travel time to the nearest city",
      variable == "marine2" ~ "marine",
      TRUE ~ variable  # Keep original name if not matched
    ))
  
  # Get the names of selected variables
  selected_variables <- model_summary$variable 
  selected_variables <- ifelse(selected_variables == "marine2", "marine", selected_variables)
  
  # Initialize an empty list to store plots
  plot_list <- list()
  
  # Loop over selected variables and create partial effect plots
  for (var in selected_variables) {
    
    var_name <- model_summary %>% filter(variable == var) %>%
      mutate(Variable = ifelse(Variable ==  "marine2","marine",Variable)) %>%
      pull(Variable) 
    
    # Check if the variable is a factor
    if (is.factor(data[[var]])) {
      partial_effects <- visreg(model, var, type = "conditional", plot = FALSE)
      plot <- ggplot(partial_effects$fit, aes_string(x = var, y = "visregFit")) +
        geom_point(size = 2) +
        geom_errorbar(aes(ymin = visregLwr, ymax = visregUpr), width = 0.1) +
        labs(title = paste("Partial Effect of", var_name), x = var_name, y = "Partial Effect") +
        my_custom_theme()
    } else {
      partial_effects <- visreg(model, var, type = "conditional", plot = FALSE)
      plot <- ggplot(partial_effects$fit, aes_string(x = var, y = "visregFit")) +
        geom_line() +
        geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
        labs(title = paste("Partial Effect of", var_name), x = var_name, y = "Partial Effect") +

        my_custom_theme()
    }
    
    plot_list[[var]] <- plot
  }
  
  # Arrange the plots together with 3 plots per row
  plot_grid <- do.call("grid.arrange", c(plot_list, ncol = 2))
  
  # Save the plot to a file
  file_name <- paste0("figures/supp/", model_name, "_partial_effects.png")
  ggsave(file_name, plot_grid, width = 297 * 1.75, height = 297 * 1.3, units ="mm")
  
  # Display the saved plot
  grid.newpage()
  grid.draw(plot_grid)
}