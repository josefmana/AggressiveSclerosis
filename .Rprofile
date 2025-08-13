targets::tar_load(finished_data)

naming <- data.frame(
  var = names(finished_data)[-c(1:3, 17, 23, 29)],
  nam = c(
    "Platform_5", "Platform_10", "HET_5", "HET_10", "LE HET_5", "LE HET_10", "Time to Treat",
    "Relapse Count",
    "T2 lesions_Y2", "T1 blackholes_Y2", "Brain Atrophy_Y2", "T1 TBV_Y2", "Corpus Callosum_Y2",
    "T2 lesions_Y5", "T1 blackholes_Y5", "Brain Atrophy_Y5", "T1 TBV_Y5", "Corpus Callosum_Y5",
    "T2 lesions_Y10", "T1 blackholes_Y10", "Brain Atrophy_Y10", "T1 TBV_Y10", "Corpus Callosum_Y10"
  ),
  typ = c(
    rep("Medication", 6), "Medication time", "Relapses", rep("MRI", 15)
  )
)

mri_data <- finished_data |>
  select(id, aggressive, all_of(naming$var[naming$typ == "MRI"]))

for(i in which(naming$typ == "MRI")) {
  mri_data[[naming$nam[i]]] <- mri_data[[naming$var[i]]]
}

mri_data <- mri_data |>
  select(id, aggressive, all_of(naming$nam[naming$typ == "MRI"]))

mri_data |>
  pivot_longer(-c(id, aggressive), names_to = c("Measure", "Year"), names_sep = "_", values_to = "Value") |>
  mutate(Year = factor(Year, levels = c("Y2", "Y5", "Y10"), ordered = TRUE)) |>
  ggpubr::ggboxplot(
    x = "aggressive", xlab = "Agressive MS",
    y = "Value",
    facet.by = c("Measure", "Year"),
    scales = "free_y",
    repel = TRUE
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25))  # 10% extra space on top
  ) +
  ggpubr::stat_compare_means(vjust = -1.2, label = "p.format")

ggsave(
  filename = here::here("figures", "MRI_desc.jpg"),
  plot = last_plot(),
  dpi = 300,
  width = 8.5,
  height = 11.4
)


med_data <- finished_data |>
  select(id, aggressive, all_of(naming$var[naming$typ == "Medication"]))

for(i in which(naming$typ == "Medication")) {
  med_data[[naming$nam[i]]] <- med_data[[naming$var[i]]]
}

med_data <- med_data |>
  select(id, aggressive, all_of(naming$nam[naming$typ == "Medication"]))

med_data |>
  pivot_longer(-c(id, aggressive), names_to = c("Medication", "Year"), names_sep = "_", values_to = "Value") |>
  ggpubr::ggviolin(
    x = "aggressive", xlab = "Agressive MS",
    y = "Value",
    facet.by = c("Medication", "Year"),
    add = "median",
    short.panel.labs = FALSE
  ) +
  ggpubr::stat_compare_means(vjust = -3.6, label = "p.format")

ggsave(
  filename = here::here("figures", "Medication_desc.jpg"),
  plot = last_plot(),
  dpi = 300
)
