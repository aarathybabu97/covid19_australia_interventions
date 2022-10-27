macro_data <- macrodistancing_data()
readRDS("outputs/fitted_macro_model.RDS")->fitted_model
nsim <- coda::niter(fitted_model$draws) * coda::nchain(fitted_model$draws)
nsim <- min(10000, nsim)

# check posterior calibration
sdlog <- fitted_model$out$sdlog
meanlog <- log(fitted_model$out$predictions) - (sdlog ^ 2) / 2
contacts_ga <- discrete_lognormal(
  meanlog = meanlog,
  sdlog = sdlog,
  breaks = fitted_model$data$breaks
)
OC_t_state <- fitted_model$predictions$mean_daily_contacts

# get trend predictions
pred_sim <- calculate(c(OC_t_state), values = fitted_model$draws, nsim = nsim)[[1]][, , 1]
quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

survey_points <- readRDS("outputs/macro_data_fit.RDS")


# get holiday dates and subset to where they overlap with surveys
holiday_lines <- survey_points %>%
  mutate(date_start = wave_date - wave_duration / 2,
         date_end = wave_date + wave_duration / 2) %>%
  select(state, date_start, date_end) %>%
  left_join(
    holiday_dates() %>%
      mutate(state = abbreviate_states(state))
  ) %>%
  filter(date < date_end & date > date_start)

holiday_lines <- holiday_dates() %>%
  mutate(
    state = abbreviate_states(state)
  ) %>%
  filter(
    date <= max(data$contacts$date) &
      date >= as.Date("2020-03-01")
  )

type <- 1
states <- unique(fitted_model$data$location_change_trends$state)
dates <- unique(fitted_model$data$location_change_trends$date)
n_states <- length(states)

# mock up data object for plotting
plot_data <- list(
  dates = list(
    infection_project = dates,
    latest_mobility = max(dates)
  ),
  states = states,
  n_states = length(states),
  n_dates_project = length(dates)
)

macro_ticks_labels <- split_ticks_and_labels(
  data = survey_points$wave_date,
  tick_freq = "1 month",
  label_freq = "4 months",
  label_format = "%b%y",
  label_last = FALSE # for some reason this is having opposite effect, i.e. FALSE is labelling last (as desired)
)

simulations=pred_sim
data = plot_data
max_date = max(macro_data$contacts$date)
multistate = TRUE
min_date = NA
keep_only_rows = NULL

if(is.na(min_date)){
  min_date = as.Date("2020-03-01")
}


mean <- colMeans(simulations)
ci_90 <- apply(simulations, 2, quantile, c(0.05, 0.95)) 
ci_50 <- apply(simulations, 2, quantile, c(0.25, 0.75))

if (multistate) {
  states <- rep(data$states, each = data$n_dates_project)
  dates <- rep(data$dates$infection_project, data$n_states)
} else {
  dates <- data$dates$infection_project
  states <- NA
}

df <- tibble(date = dates,
             state = states,
             mean = mean,
             ci_50_lo = ci_50[1, ],
             ci_50_hi = ci_50[2, ],
             ci_90_lo = ci_90[1, ],
             ci_90_hi = ci_90[2, ])

if (!is.null(keep_only_rows)) {
  df <- df[keep_only_rows, ]
}

df <- df %>%
  filter(
    date >= min_date,
    date <= max_date
  ) %>%
  mutate(type = "Nowcast")

df%>%
  filter(state=="NSW")->nsw_non_household_contacts


  date_breaks <- "3 month"
  date_minor_breaks <- "1 month"
  date_labels <- "%b%y"
  x_text_angle <- 0
  x_text_size <- 9
  x_text_hjust <- 0.5
  x_text_vjust <- 0.5

  base_colour = purple
  ylim = c(0, 20)
  ybreaks = NULL
  intervention_at = interventions()
  projection_at = NA
  plot_voc = FALSE
  plot_vax = FALSE
  
  
if (is.null(ylim)) {
  ylim <- c(min(df$ci_90_lo), max(df$ci_90_hi)) 
}

if (is.null(ybreaks)){
  if(range(ylim)[2] - range(ylim)[1] >= 4 & range(ylim)[2] - range(ylim)[1] <= 10){
    y_scale <- scale_y_continuous(position = "right", breaks = seq(from = ylim[1], to = ylim[2], by = 1))
    
  } else(
    y_scale <- scale_y_continuous(position = "right")
  )
} else {
  y_scale <- scale_y_continuous(position = "right", breaks = seq(from = ybreaks[1], to = ybreaks[2], by = 1))
}

  hline_at = NULL
  
   ggplot(nsw_non_household_contacts) + 
    
    aes(date, mean, fill = type) +
    
    xlab(element_blank()) +
    
    coord_cartesian(ylim = ylim) +
    y_scale +
    scale_x_date(date_breaks = date_breaks, date_minor_breaks = date_minor_breaks, date_labels = date_labels) +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    
    geom_vline(
      aes(xintercept = date),
      data = intervention_at,
      colour = "grey75"
    ) +
    
    geom_ribbon(aes(ymin = ci_90_lo,
                    ymax = ci_90_hi),
                alpha = 0.2) +
    geom_ribbon(aes(ymin = ci_50_lo,
                    ymax = ci_50_hi),
                alpha = 0.5) +
    geom_line(aes(y = ci_90_lo),
              colour = base_colour,
              alpha = 0.8) + 
    geom_line(aes(y = ci_90_hi),
              colour = base_colour,
              alpha = 0.8) + 
    
    geom_hline(yintercept = hline_at, linetype = "dotted") +
    
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = x_text_size, angle = x_text_angle,
                                     hjust = x_text_hjust, vjust = x_text_vjust)) + 
     ggtitle(label = paste("Macro-distancing trend :",unique(nsw_non_household_contacts$state)),
             subtitle = "Rate of non-household contacts") +
     ylab("Estimated mean number of non-household contacts per day") + 
     
     # add baseline estimate
     geom_point(
       aes(date, estimate),
       data = baseline_point,
       size = 0.5,
       colour = grey(0.5)
     ) +
     geom_errorbar(
       aes(
         date,
         estimate,
         ymin = lower,
         ymax = upper
       ),
       data = baseline_point,
       width = 0,
       colour = grey(0.5)
     ) + 
     
     # rug marks for holidays
     geom_rug(
       aes(date),
       data = holiday_lines,
       col = green,
       size = 1,
       length = unit(0.05, "npc"),
       sides = "b",
       inherit.aes = FALSE
     ) +
   scale_x_date(
     breaks = macro_ticks_labels$ticks,
     labels = macro_ticks_labels$labels
   ) +
     theme(
       axis.text.x = element_text(size = 8),
       axis.ticks.x = element_line(colour = macro_ticks_labels$tick.cols)
     )
  
   ggsave("outputs/figures/nsw_macrodistancing.png",width = 13, height = 6)
   
   write_csv(nsw_non_household_contacts,file = "outputs/nsw_non_household_contacts.csv") 
 