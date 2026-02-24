
GE_results = read.csv("GE_results.csv")

GE_results$date = as.Date(GE_results$date)
party_cols = names(GE_results)[3:ncol(GE_results)]

print(party_cols)
#[1] "Other_Right"  "Reform_UKIP"  "Conservative" "Lib_Dem"      "Labour"       "SNP"          "Green"        "Other_Left"


library(ggplot2)
ggplot_data = reshape2::melt(GE_results, id.vars = "date", measure.vars = party_cols,
                              variable.name = "Party", value.name = "Percentage")


names(ggplot_data)

ggplot_data$Party = factor(ggplot_data$Party, levels = party_cols)

party_cols_named <- setNames(
  c("darkblue", "purple", "#6688ff", "orange", "#ff7777", "yellow", "#55ff55", "darkred"),
  party_cols
)

ggplot(ggplot_data, aes(x = date, y = Percentage, fill = Party)) +
    geom_area() +
    labs(title = "UK General Election Results", x = "date", y
 = "Percentage Votes") +
    scale_fill_manual(values = party_cols_named) +
    # add horizontal line at 0.5
    geom_hline(yintercept = 0.5, linetype = 5, color = "#111111") +
    geom_hline(yintercept = 0.45, linetype = 2, color = "#222222") +
    geom_hline(yintercept = 0.55, linetype = 2, color = "#222222") +
    geom_hline(yintercept = 0.25, linetype = 2, color = "#555555") +
    geom_hline(yintercept = 0.75, linetype = 2, color = "#555555") +
    
    theme_minimal()

