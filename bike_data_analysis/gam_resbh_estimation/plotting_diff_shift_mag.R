Sys.setlocale("LC_TIME", "English")
library(mgcv)
library(ggplot2)

# -------------------- setup --------------------
seed_option <- c(31)
shift_cond <- c("base", "_big", "_small")  # "base" replaces the empty string

load(file = "../data_pre_processing/initial_data/starting_timepoint.RData")
load(file = "../data_pre_processing/initial_data/merged_data_new_nosl60.RData")
load(file = "results_nosl60_new/median_deltat_recip_events.RData")
load(file = "results_nosl60_new/median_deltat_repet_events.RData")

new_subfolder <- "final_plots_diff_shift_mag"
dir.create(new_subfolder, showWarnings = FALSE)

# -------------------- descriptive names for smooth terms --------------------
term_names <- c(
  "1" = "global_time",
  "2" = "temperature",
  "3" = "precipitation",
  "4" = "distance",
  "5" = "reciprocity",
  "6" = "repetition",
  "7" = "time_of_day",
  "8" = "residual_cum_baseline_hazard"
)

# -------------------- load all GAM models --------------------
gam_fits <- list()
for (seed in seed_option) {
  gam_fits[[as.character(seed)]] <- list()
  for (sh in shift_cond) {
    file_suffix <- ifelse(sh == "base", "", sh)
    fn <- paste0("results_nosl60/gam_model_fit_", seed, file_suffix, ".RData")
    if (!file.exists(fn)) {
      warning("File not found: ", fn)
      next
    }
    load(fn)  # loads l0.gam
    gam_fits[[as.character(seed)]][[sh]] <- l0.gam
  }
}

# -------------------- extract smooths --------------------
extract_smooths <- function(gam_model) {
  pdf(NULL)
  res <- plot(gam_model, se = TRUE)
  invisible(dev.off())
  return(res)
}

smooths_all <- lapply(gam_fits, function(seed_models) {
  lapply(seed_models, extract_smooths)
})

# -------------------- helper: transform x-axis per smooth term --------------------
transform_smooth <- function(sm, term_index, seed, shift, starting_timepoint, 
                             med_rec_events, med_rep_events, weekday_plot = FALSE) {
  
  if (length(sm$x) == 0 || length(sm$fit) == 0) {
    message(paste0("WARNING: Empty smooth detected -> seed=", seed, 
                   ", shift='", shift, "', term_index=", term_index))
    return(NULL)
  }
  
  if (term_index == 1) {
    x_seconds <- sm$x * 60
    x <- starting_timepoint + x_seconds
    xlab <- "Time"
    ylab <- "Global time effect (log-scale)"
    
    if (weekday_plot) {
      # Keep continuous time but map to day-of-week
      x <- as.POSIXlt(x)$wday  # 0=Sunday,...6=Saturday
      xlab <- "Day of week"
    }
  }
  else if (term_index == 2) {
    x <- sm$x
    xlab <- "Temperature (°C)"
    ylab <- "Temperature smooth effect"
  } else if (term_index == 3) {
    x <- exp(sm$x) - 1
    xlab <- "Precipitation (mm)"
    ylab <- "Precipitation smooth effect"
  } else if (term_index == 4) {
    x <- exp(sm$x) - 1
    xlab <- "Route distance (mins.)"
    ylab <- "Distance smooth effect"
  } else if (term_index == 5) {
    x <- (-(2*med_rec_events)*log(sm$x))/60
    xlab <- "Time since last reciprocal event (hrs.)"
    ylab <- "Reciprocity"
  } else if (term_index == 6) {
    x <- (-(2*med_rep_events)*log(sm$x))/60
    xlab <- "Time since last same event (hrs.)"
    ylab <- "Repetition"
  } else if (term_index == 7) {
    x <- sm$x
    xlab <- "Time of day (hrs)"
    ylab <- "Time of day smooth effect"
  }
  
  data.frame(
    x = x,
    fit = sm$fit,
    upper = sm$fit + sm$se,
    lower = sm$fit - sm$se,
    xlab = xlab,
    ylab = ylab,
    shift = shift
  )
}

# -------------------- helper: combine shift_cond for plotting --------------------
make_comparison_df <- function(smooths_list, term_index, seed, shift_labels,
                               starting_timepoint, med_rec_events, med_rep_events,
                               weekday_plot = FALSE) {
  df_list <- list()
  for (sh in shift_labels) {
    sm <- smooths_list[[sh]][[term_index]]
    tmp <- transform_smooth(sm, term_index, seed, sh, starting_timepoint,
                            med_rec_events, med_rep_events, weekday_plot)
    if (!is.null(tmp)) {
      df_list[[sh]] <- tmp
    }
  }
  if (length(df_list) == 0) {
    message(paste0("NOTE: No valid smooths found for seed=", seed, 
                   ", term_index=", term_index))
    return(NULL)
  }
  df <- do.call(rbind, df_list)
  return(df)
}

# -------------------- helper: ggplot overlay --------------------
plot_comparison <- function(df, term_index, seed) {
  
  # Clean legend labels: map to expressions
  df$legend_label <- gsub("^_", "", df$shift)  # remove "_" prefix
  label_map <- c("base" = expression(nu == 1),
                 "big" = expression(nu == 10),
                 "small" = expression(nu == 0.1))
  
  # Colors
  colors_clean <- c("base" = "black", "big" = "#56B4E9", "small" = "darkorange")
  
  # x-axis limits
  xlim_vals <- switch(as.character(term_index),
                      "1" = range(df$x),      # global time
                      "2" = range(df$x),      # temperature
                      "3" = range(df$x),      # precipitation
                      "4" = c(0, 60),         # distance
                      "5" = c(0, 24),         # reciprocity
                      "6" = c(0, 84),         # repetition
                      "7" = c(0, 24),         # time of day
                      NULL)
  
  # y-axis limits
  ylim_vals <- switch(as.character(term_index),
                      "4" = c(-1, max(df$upper)),   # distance
                      "5" = c(-0.5, max(df$upper)), # reciprocity
                      NULL)
  
  p <- ggplot(df, aes(x = x, y = fit, color = legend_label, fill = legend_label)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    theme_minimal(base_size = 16) +
    labs(
      x = unique(df$xlab),
      y = unique(df$ylab),
      color = "Seed 1",  # Legend title set explicitly
      fill  = "Seed 1"
    )  +
    scale_color_manual(values = colors_clean,
                       breaks = c("base", "big", "small"),
                       labels = label_map) +
    scale_fill_manual(values = colors_clean,
                      breaks = c("base", "big", "small"),
                      labels = label_map) +
    coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
  
  return(p)
}

# -------------------- generate GAM smooth plots --------------------
for (seed in seed_option) {
  for (i in 1:7) {
    term_label <- term_names[as.character(i)]
    
    if (i == 1) {
      # Calendar plot
      df <- make_comparison_df(smooths_all[[as.character(seed)]], i, seed,
                               shift_cond, starting_timepoint,
                               med_rec_events, med_rep_events,
                               weekday_plot = FALSE)
      if (!is.null(df)) {
        pdf(file = paste0(new_subfolder, "/", term_label, "_calendar_seed_", seed, ".pdf"))
        print(plot_comparison(df, i, seed))
        dev.off()
      }
      
      # Weekday plot
      df <- make_comparison_df(smooths_all[[as.character(seed)]], i, seed,
                               shift_cond, starting_timepoint,
                               med_rec_events, med_rep_events,
                               weekday_plot = TRUE)
      if (!is.null(df)) {
        pdf(file = paste0(new_subfolder, "/", term_label, "_weekday_seed_", seed, ".pdf"))
        print(plot_comparison(df, i, seed))
        dev.off()
      }
      
    } else {
      df <- make_comparison_df(smooths_all[[as.character(seed)]], i, seed,
                               shift_cond, starting_timepoint,
                               med_rec_events, med_rep_events)
      if (!is.null(df)) {
        pdf(file = paste0(new_subfolder, "/", term_label, "_seed_", seed, ".pdf"))
        print(plot_comparison(df, i, seed))
        dev.off()
      }
    }
  }
}

# -------------------- residual cumulative baseline hazard with fitted lines --------------------
for (seed in seed_option) {
  df_list <- list()
  
  for (sh in shift_cond) {
    file_suffix <- ifelse(sh == "base", "", sh)
    fn <- paste0("results_nosl60_new/res_basehaz_", seed, file_suffix, ".RData")
    if (!file.exists(fn)) {
      warning("File not found: ", fn)
      next
    }
    load(fn)  # loads c_est
    
    time_vals <- sort(merged_data$DateTime*60 + starting_timepoint)
    
    df_tmp <- data.frame(
      time = time_vals,
      L0 = c_est$model$L0,
      fitted_line = sort(merged_data$DateTime)*c_est$coefficients,
      shift = sh
    )
    df_list[[sh]] <- df_tmp
  }
  
  if (length(df_list) == 0) next
  df <- do.call(rbind, df_list)
  df$legend_label <- gsub("^_", "", df$shift)  # base, big, small
  
  # Colors
  colors_clean <- c("base" = "black", "big" = "#56B4E9", "small" = "darkorange")
  
  # Plot
  p <- ggplot(df) +
    geom_line(aes(x = time, y = L0, color = legend_label), size = 1.2) +
    geom_line(aes(x = time, y = fitted_line, color = legend_label), linetype = "dashed", size = 1) +
    theme_minimal(base_size = 16) +
    labs(
      x = "Time",
      y = "Breslow estimator",
      color = paste("Seed", seed)
    ) +
    scale_color_manual(values = colors_clean)
  
  pdf(file = paste0(new_subfolder, "/residual_cum_baseline_hazard_seed_", seed, ".pdf"),
      height = 8, width = 12)
  print(p)
  dev.off()
}
