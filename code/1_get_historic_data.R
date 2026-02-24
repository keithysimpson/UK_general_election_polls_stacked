library(rvest)

source("_poll_functions.R")

#--- define the urls: ---------------
url_GE2015 = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_United_Kingdom_general_election"
url_GE2017 = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2017_United_Kingdom_general_election"
url_GE2019 = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_United_Kingdom_general_election"
url_GE2024 = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2024_United_Kingdom_general_election"
url_latest = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_United_Kingdom_general_election"

#--- download the html: ---------------
page_GE2015 = read_html(url_GE2015)
page_GE2017 = read_html(url_GE2017)
page_GE2019 = read_html(url_GE2019)
page_GE2024 = read_html(url_GE2024)
page_latest = read_html(url_latest)


#--- extract the tables: ---------------

#----- 2015: ---------------
GE2015_years = getYears(page_GE2015)

GE2015_table_list = list()
for (this_year in GE2015_years) {
    #this_year = GE2015_years[3]
    print(this_year)
    GE2015_table_list[[paste0(this_year)]] = getTableByYear(page = page_GE2015, year = this_year, party_cols = party_cols)
}





#----- 2017: ---------------
GE2017_years = getYears(page_GE2017)

GE2017_table_list = list()
for (this_year in GE2017_years) {
    #this_year = GE2017_years[3]
    print(this_year)
    GE2017_table_list[[paste0(this_year)]] = getTableByYear(page = page_GE2017, year = this_year, party_cols = party_cols)
}


#----- 2019: ---------------
GE2019_years = getYears(page_GE2019)

GE2019_table_list = list()
for (this_year in GE2019_years) {
    #this_year = GE2019_years[3]
    print(this_year)
    GE2019_table_list[[paste0(this_year)]] = getTableByYear(page = page_GE2019, year = this_year, party_cols = party_cols)
}

#getTableByYear(page = page_GE2019, year =  2019, party_cols = party_cols)

#----- 2024: ---------------
GE2024_years = getYears(page_GE2024)

GE2024_table_list = list()
for (this_year in GE2024_years) {
    #this_year = GE2024_years[3]
    print(this_year)
    GE2024_table_list[[paste0(this_year)]] = getTableByYear(page = page_GE2024, year = this_year, party_cols = party_cols)
}


#----- latest: ---------------
latest_years = getYears(page_latest)
#latest_table_list = lapply(latest_years, getTableByYear, page = page_latest, party_cols = party_cols)
latest_table_list = list()
for (this_year in latest_years) {
    #this_year = latest_years[3]
    print(this_year)
    latest_table_list[[paste0(this_year)]] = getTableByYear(page = page_latest, year = this_year, party_cols = party_cols)
}


# for each lets get all the column names:
# table(sapply(GE2015_table_list, names))
# table(sapply(GE2017_table_list, names))
# table(sapply(GE2019_table_list, names))
# table(sapply(GE2015_table_list, names))
# table(sapply(latest_table_list, names))



#--- combine tables ---------------------------
# initially we will have one "other" column
# ...but we will use the GE results to guide how this should be split between "right" and "left"

#--- load historic GE data
GE_results = read.csv("GE_results.csv")

GE_results$date = as.Date(GE_results$date)
GE_party_cols = names(GE_results)[3:ncol(GE_results)]

# sort by date
GE_results = GE_results[order(GE_results$date),]



formated_GE_polls_2015 = formatPollTable(GE2015_table_list, 2015)

formated_GE_polls_2017 = formatPollTable(GE2017_table_list, 2017)

formated_GE_polls_2019 = formatPollTable(GE2019_table_list, 2019)

formated_GE_polls_2024 = formatPollTable(GE2024_table_list, 2024)

formated_GE_polls_latest = formatPollTable(latest_table_list, 2024)

formated_GE_polls_all = do.call(rbind, list(formated_GE_polls_2015, formated_GE_polls_2017, formated_GE_polls_2019, formated_GE_polls_2024, formated_GE_polls_latest))


# sense check:
#plot(formated_GE_polls_all$date, formated_GE_polls_all$percent_left)

#plot(this_GE_polls$date, this_GE_polls$Other_Left)
#plot(this_GE_polls$date, this_GE_polls$Other_Right)

# clean the pollster names, as they sometimes end in a wikipedia ref
formated_GE_polls_all$Pollster = gsub("\\[.*\\]$", "", formated_GE_polls_all$Pollster)

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