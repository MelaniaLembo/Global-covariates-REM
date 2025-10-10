Sys.setlocale("LC_TIME", "English")
library(mgcv)
library(ggplot2)

# -------------------- setup --------------------
seed_option <- c(31, 17, 12)
shift_cond <- c("base")

load(file = "../data_pre_processing/initial_data/starting_timepoint.RData")
load(file = "../data_pre_processing/initial_data/merged_data_new_nosl60.RData")
load(file = "results_nosl60_new/median_deltat_recip_events.RData")
load(file = "results_nosl60_new/median_deltat_repet_events.RData")

new_subfolder <- "final_plots_fixed_shift_diff_ne_samp"
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

# -------------------- helper: combine seeds for plotting --------------------
make_seed_comparison_df <- function(smooths_all, term_index, shift_label,
                                    starting_timepoint, med_rec_events, med_rep_events,
                                    weekday_plot = FALSE) {
  df_list <- list()
  
  for (seed in names(smooths_all)) {
    sm <- smooths_all[[seed]][[shift_label]][[term_index]]
    tmp <- transform_smooth(sm, term_index, seed, shift_label,
                            starting_timepoint, med_rec_events, med_rep_events,
                            weekday_plot)
    if (!is.null(tmp)) {
      tmp$seed <- seed
      df_list[[seed]] <- tmp
    }
  }
  
  if (length(df_list) == 0) return(NULL)
  df <- do.call(rbind, df_list)
  return(df)
}

# -------------------- helper: ggplot overlay across seeds --------------------
plot_seed_comparison <- function(df, term_index) {
  # Keep only base shift
  df <- df[df$shift == "base", ]
  
  # Relabel seeds
  df$seed <- factor(df$seed, levels = c("31", "17", "12"), labels = c("Seed 1", "Seed 2", "Seed 3"))
  
  # Colors for seeds
  colors_seed <- c("Seed 1" = "black", "Seed 2" = "#56B4E9", "Seed 3" = "darkorange") 
  
  # Uniform alpha for all ribbons
  ribbon_alpha <- 0.2
  
  # x-axis limits
  xlim_vals <- switch(as.character(term_index),
                      "1" = range(df$x),
                      "2" = range(df$x),
                      "3" = range(df$x),
                      "4" = c(0, 60),
                      "5" = c(0, 24),
                      "6" = c(0, 84),
                      "7" = c(0, 24),
                      NULL)
  
  # y-axis limits
  ylim_vals <- switch(as.character(term_index),
                      "4" = c(-1, max(df$upper)),
                      "5" = c(-0.5, max(df$upper)),
                      NULL)
  
  ggplot(df, aes(x = x, y = fit, color = seed, fill = seed)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = ribbon_alpha, color = NA) +
    theme_minimal(base_size = 16) +
    labs(
      x = unique(df$xlab),
      y = unique(df$ylab),
      color = expression(nu == 1),
      fill  = expression(nu == 1)
    ) +
    scale_color_manual(values = colors_seed) +
    scale_fill_manual(values = colors_seed) +
    coord_cartesian(xlim = xlim_vals, ylim = ylim_vals) 
}

# -------------------- generate GAM smooth plots across seeds (shift = base only) --------------------
for (i in 1:7) {
  term_label <- term_names[as.character(i)]
  
  if (i == 1) {
    # Calendar plot
    df <- make_seed_comparison_df(smooths_all, i, "base",
                                  starting_timepoint, med_rec_events, med_rep_events,
                                  weekday_plot = FALSE)
    if (!is.null(df)) {
      pdf(file = paste0(new_subfolder, "/", term_label, "_calendar_seeds.pdf"))
      print(plot_seed_comparison(df, i))
      dev.off()
    }
    
    # Weekday plot
    df <- make_seed_comparison_df(smooths_all, i, "base",
                                  starting_timepoint, med_rec_events, med_rep_events,
                                  weekday_plot = TRUE)
    if (!is.null(df)) {
      pdf(file = paste0(new_subfolder, "/", term_label, "_weekday_seeds.pdf"))
      print(plot_seed_comparison(df, i))
      dev.off()
    }
    
  } else {
    df <- make_seed_comparison_df(smooths_all, i, "base",
                                  starting_timepoint, med_rec_events, med_rep_events)
    if (!is.null(df)) {
      pdf(file = paste0(new_subfolder, "/", term_label, "_seeds.pdf"))
      print(plot_seed_comparison(df, i))
      dev.off()
    }
  }
}
