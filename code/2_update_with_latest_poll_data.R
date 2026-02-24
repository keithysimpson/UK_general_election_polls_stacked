library(rvest)

source("code/_poll_functions.R")

#--- define the urls: ---------------
url_latest = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_United_Kingdom_general_election"

#--- download the html: ---------------
page_latest = read_html(url_latest)


#--- extract the tables: ---------------

#----- latest: ---------------
latest_years = getYears(page_latest)
#latest_table_list = lapply(latest_years, getTableByYear, page = page_latest, party_cols = party_cols)
latest_table_list = list()
for (this_year in latest_years) {
    #this_year = latest_years[3]
    print(this_year)
    latest_table_list[[paste0(this_year)]] = getTableByYear(page = page_latest, year = this_year, party_cols = party_cols)
}



#--- combine tables ---------------------------
# initially we will have one "other" column
# ...but we will use the GE results to guide how this should be split between "right" and "left"

#--- load historic GE data
GE_results = read.csv("data/GE_results.csv")

GE_results$date = as.Date(GE_results$date)
GE_party_cols = names(GE_results)[3:ncol(GE_results)]

# sort by date
GE_results = GE_results[order(GE_results$date),]




formated_GE_polls_latest = formatPollTable(latest_table_list, 2024)


formated_GE_polls_latest$Pollster = gsub("\\[.*\\]$", "", formated_GE_polls_latest$Pollster)

#---- load existing data -----------------

formated_GE_polls_all = read.csv("data/GE_polls_all.csv")

formated_GE_polls_all$date = as.Date(formated_GE_polls_all$date)

all_date_pollster = paste0(formated_GE_polls_all$date,"_",formated_GE_polls_all$Pollster)
latest_date_pollster = paste0(formated_GE_polls_latest$date,"_",formated_GE_polls_latest$Pollster)

# ...but remove any wikipedia ref numbers as these will change e.g. [22]
all_date_pollster = gsub("\\[.*\\]", "", all_date_pollster)
latest_date_pollster = gsub("\\[.*\\]", "", latest_date_pollster)

formated_GE_polls_NEW = formated_GE_polls_latest[!latest_date_pollster %in% all_date_pollster,]


# add to existing data
formated_GE_polls_all = rbind(formated_GE_polls_all, formated_GE_polls_NEW)

# sense check:
#plot(formated_GE_polls_all$date, formated_GE_polls_all$percent_left)

#plot(this_GE_polls$date, this_GE_polls$Other_Left)
#plot(this_GE_polls$date, this_GE_polls$Other_Right)


#--- save result --------------------
write.csv(formated_GE_polls_all, "data/GE_polls_all.csv", row.names = FALSE)



#--- plot ---------------
if (FALSE) {
    library(ggplot2)
    ggplot_data = reshape2::melt(formated_GE_polls_all, id.vars = "date", measure.vars = GE_party_cols,
                                variable.name = "Party", value.name = "Percentage")


    names(ggplot_data)

    ggplot_data$Party = factor(ggplot_data$Party, levels = GE_party_cols)

    party_cols_named <- setNames(
    c("darkblue", "purple", "#6688ff", "orange", "#ff7777", "yellow", "#55ff55", "darkred"),
    GE_party_cols
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
}