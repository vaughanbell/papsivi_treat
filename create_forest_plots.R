############################################################################
#
# Script to produce forest plots from output data
#
############################################################################

library(dplyr)
library(forester)   # from https://github.com/rdboyes/forester

data_dir <- "./output/"
output_dir <- "./forestplots/"

#
# replace_exposure_labels: Function to replace exposure labels with human friendly ones
#
# Parameters:
#  results_df: dataframe with exposure, estimate, and confidence intervals
# Returns:
#  dataframe with re-written exposure labels
#
replace_exposure_labels <- function(df) {
  df %>%
    mutate(
      Variable = case_match(Variable,
                            "hechovictimizante_despojo_tierras"   ~ "Forced land dispossession",
                            "hechovictimizante_hostigamientos"    ~ "Witness to terrorism/combat",
                            "hechovictimizante_amenaza"           ~ "Threats",
                            "hechovictimizante_confinamiento"     ~ "Confinement",
                            "hechovictimizante_violenciasexual"   ~ "Sexual violence",
                            "hechovictimizante_desparacion"       ~ "Forced disappearance",
                            "hechovictimizante_desplazamiento"    ~ "Forced displacement",
                            "hechovictimizante_homocidio"         ~ "Homicide",
                            "hechovictimizante_lesion_fis"        ~ "Physical injury",
                            "hechovictimizante_lesion_psic"       ~ "Psychological injury",
                            "hechovictimizante_minas"             ~ "Mines, improvised explosives",
                            "hechovictimizante_perdida_bienes"    ~ "Loss of personal belongings",
                            "hechovictimizante_secuestro"         ~ "Kidnapping",
                            "hechovictimizante_tortura"           ~ "Torture",
                            "hechovictimizante_reclut_ninos"      ~ "Child recruitment",
                            .default = Variable  # Keep original value if no match
      )
    )
}

#
# forester_plot: Function to generate a forest plot from the dataframe of estimates and CIs
#
# Parameters:
#  results_df: dataframe with exposure, estimate, and confidence intervals
#  min_ci_lb: the lower limit of the scale for the plot
#  max_ci_ub: the upper limit of the scale for the plot
#  render_as: filetype to write image file as
#  file_path: the path to write the PNG file
# Returns:
#  nothing
#
forester_plot <- function(results_df, min_ci_lb, max_ci_ub, render_as, file_path) {
  forest_df <- results_df %>%
    arrange(desc(OddsRatio))
  
  forest_df <- forest_df %>%
    mutate(est_str = sprintf("%.2f (%.2f - %.2f)", OddsRatio, LowerCI, UpperCI))

    # Extract column for the left side column needed for forester
  forest_df_lsd <- forest_df %>%
    select("Exposure" = Variable)
  
  # Extract columns for the right side column needed for forester (and round Q to 2 decimals places)
  forest_df_rsd <- forest_df %>%
    select("OR (95% CI)" = est_str)
  
  # Plot as forest plot
  if (plot == 1) {
    forester(left_side_data = forest_df_lsd,
             right_side_data = forest_df_rsd,
             estimate = forest_df$OddsRatio,
             ci_low = forest_df$LowerCI,
             ci_high = forest_df$UpperCI,
             null_line_at = 1,
             stripe_colour = "#ffffff",
             estimate_precision = 2,
             font_family = "sans",
             arrows = TRUE, 
             arrow_labels = c("Less likely", "More likely"),
             ggplot_width = 30,
             nudge_x = 1,
             xlim = c(min_ci_lb, max_ci_ub),
             render_as = render_as,
             file_path = file_path,
             display = TRUE)
  }
}

plot <- 1

#
# Plot and write files
#

# Individual sessions

modind_ad_filename <- paste(data_dir, "modind_ad_results.csv", sep = "")
modind_ad_df <- read.csv(modind_ad_filename, header = TRUE)

modind_ad_df <- replace_exposure_labels(modind_ad_df)

filename <- paste(output_dir, "individual_sessions_forest.png", sep = "")
forest_plot <- forester_plot(modind_ad_df, 0, 5, render_as = "png", filename)

# Group sessions

modgrp_ad_filename <- paste(data_dir, "modgrp_ad_results.csv", sep = "")
modgrp_ad_df <- read.csv(modgrp_ad_filename, header = TRUE)

modgrp_ad_df <- replace_exposure_labels(modgrp_ad_df)

filename <- paste(output_dir, "group_sessions_forest.png", sep = "")
forest_plot <- forester_plot(modgrp_ad_df, -0.5, 12, render_as = "png", filename)

# Family sessions

modfam_ad_filename <- paste(data_dir, "modfam_ad_results.csv", sep = "")
modfam_ad_df <- read.csv(modfam_ad_filename, header = TRUE)

modfam_ad_df <- replace_exposure_labels(modfam_ad_df)

filename <- paste(output_dir, "family_sessions_forest.png", sep = "")
forest_plot <- forester_plot(modfam_ad_df, 0, 2, render_as = "png", filename)

# Community sessions

modcom_ad_filename <- paste(data_dir, "modcom_ad_results.csv", sep = "")
modcom_ad_df <- read.csv(modcom_ad_filename, header = TRUE)

modcom_ad_df <- replace_exposure_labels(modcom_ad_df)

filename <- paste(output_dir, "community_sessions_forest.png", sep = "")
forest_plot <- forester_plot(modcom_ad_df, 0, 2, render_as = "png", filename)



