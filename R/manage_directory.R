# Function to: extract output from glmer/lmer list and produce
# Output: in-text format, either {b, SE, t(df), p}, {b, 95% CI [], p}
# Arg: glmer/lmer/glm/lm
# Return: data frame?

# Function to: extract output from emmeans/contrast


# Function: sort and create/save plots

create_plots_folder <- function() {
  folder_path <- getwd()
  # check if /plots exist, if not create it.
  if(dir.exists(paste0(folder_path, "/plots"))) {
    message("Current path has a 'plots' folder.")
  } else {
    message("No 'plots' folder detected. Creating...")
    dir.create(paste0(folder_path, "/plots"))
  }

}

