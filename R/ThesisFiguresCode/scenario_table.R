scenario_strings <- ""

create_ae_string <- function(ae, scenario_name, ae_type, i) {
  time_to_first_event <- ae$time_to_first_event
  event_duration <- ae$event_duration
  time_between_events <- ae$time_between_events
  severity_probability <- paste(round(ae$severity_probability,3), collapse = ", ")

  paste0(scenario_name, " - ", ae_type, i, " & ",
         event_duration, " & ", time_to_first_event, " & ",
         time_between_events, " & ", severity_probability, " \\\\ \\hline")
}

for (scenario_name in names(SCENARIOS)) {
  scenario <- SCENARIOS[[scenario_name]]

  for (i in seq_along(scenario$control_AEs)) {
    ae_str <- create_ae_string(scenario$control_AEs[[i]], scenario_name, "Control AE", i)
    scenario_strings <- paste0(scenario_strings ,"\n",ae_str)
  }

  for (i in seq_along(scenario$treatment_AEs)) {
    ae_str <- create_ae_string(scenario$treatment_AEs[[i]], scenario_name, "Treatment AE", i)
    scenario_strings <- paste0(scenario_strings ,"\n",ae_str)
  }
  scenario_strings <- paste0(scenario_strings , "\n"," \\hline")

}

cat(scenario_strings)