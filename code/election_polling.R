library(rvest)




#View(poll_data)
party_cols = list("Ref_UKIP" = c("Ref","Reform", "Reform UK", "ReformUK", "Reform UK", "Brexit","UKIP"),
               "Con" = "Con", 
               "LD" = c("LD", "Lib Dem", "Lib Dems", "Lib. Dems"), 
               "Lab" = c("Lab"),
              "Grn" = c("Green", "Greens", "Grn"), 
              "SNP" = c("SNP"), 
              "PC" = c("PC", "Plaid", "Plaid Cymru"), 
              "Others" = c("Other", "Others", "Change UK"))


parse_poll_date = function(date_string, year = 2024) {
    # Remove whitespace
    date_string = trimws(date_string)
    
    
    # common error, sometimes short day like June is "June" rather than "Jun"
    date_string = gsub("June", "Jun", date_string)
    
    # less common, but there is one instance of a date "Pre-23 Apr", so just drop the "Pre-"
    date_string = gsub("Pre-", "", date_string)
    
    # Month abbreviations to numbers
    months_list = c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6,
               "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)
    
    #date_string = "4 Jul 2024"

    # Check if it's a date range (contains – or -)
    if (grepl("–|−|-", date_string)) {
        # Split on dash/en-dash
        parts = strsplit(date_string, "–|−|-")[[1]]
        parts = trimws(parts)
        
        # Start date
        start_part = parts[1]
        start_day = as.numeric(gsub("^([0-9]{1,2}).*", "\\1", start_part))
        
        # End date (always complete with day and month)
        end_part = parts[2]
        end_day = as.numeric(gsub("^([0-9]{1,2}).*", "\\1", end_part))
        end_month_name = gsub("[0-9 ]", "", end_part)
        # potentially need to remove year, as it is sometimes included
        end_month_name = trimws(gsub(year, "", end_month_name))
        
        end_month = months_list[end_month_name]
        
        # If start date doesn't have a month, use end month
        if (grepl("[A-Za-z]", start_part)) {
            start_month_name = gsub("[0-9 ]", "", start_part)
            start_month = months_list[start_month_name]
        } else {
            start_month = end_month
        }
        
        # Create start and end dates
        start_date = as.Date(paste(year, start_month, start_day, sep = "-"))
        end_date = as.Date(paste(year, end_month, end_day, sep = "-"))
        
        # Return midpoint
        mid_date = start_date + floor((end_date - start_date) / 2)
        return(mid_date)
        
    } else {
        # Single date
        day = as.numeric(gsub("^([0-9]{1,2}).*", "\\1", date_string))
        month_name = gsub("[0-9 ]", "", date_string)
        
        # potentially need to remove year, as it is sometimes included
        #month_name = trimws(gsub(year, "", month_name))
        
        month = months_list[month_name]
        
        return(as.Date(paste(year, month, day, sep = "-")))
    }
}

getTableByYear <- function(page, year, party_cols) {
    # find the table for this year
    xpath = paste0("//h3[@id='", year, "']/ancestor::div/following-sibling::table[1]")
    this_table = html_node(page, xpath = xpath) |> html_table(fill = TRUE)
    
    # Drop the first row if it's a duplicate header
    if (this_table[1,1] == names(this_table)[1]) {
      # drop the first row
      this_table = this_table[-1, ]
    }
  
    # print column names
    print(names(this_table))  
  
    #--- get a clean date
    names(this_table)[1] = "Dates_conducted"
    suppressWarnings(as.Date(sapply(this_table$Dates_conducted[75:80], parse_poll_date, year = year)))
    this_table$date = suppressWarnings(as.Date(sapply(this_table$Dates_conducted, parse_poll_date, year = year)))
    
    #--- clean the sample size column
    names(this_table)[grep("sample",names(this_table),ignore.case = T)] = "Samplesize"
    this_table$Samplesize = suppressWarnings(as.numeric(gsub(",", "", this_table$Samplesize)))
    
    this_table = this_table[!is.na(this_table$Samplesize), ]
    
    
    #--- if pollster and client is mixed then split them...
    pollster_client_col = grep("Poll.*/Client", names(this_table), ignore.case = T)
    
    if (length(pollster_client_col) == 1) {
        
        
        pollster_client_split = strsplit(unlist(this_table[,pollster_client_col]), split = "/", fixed = TRUE)
        this_table$Pollster = sapply(pollster_client_split, function(x) trimws(x[1]))
        
        this_table$Client = sapply(pollster_client_split, function(x) {
            if (length(x) > 1) {
                return(trimws(x[2]))
            } else {
                return(NA)
            }
        })
        this_table[,pollster_client_col] = NULL
    }
    
    
    #--- deal with different party names:
    
    # sometimes partys ends with a ".", lets remove this
    names(this_table) = gsub("\\.$", "", names(this_table))
    
    
    # loop through party_cols to rename
    
    for (standard_name in names(party_cols)) {
        variations = party_cols[[standard_name]]
        names(this_table)[names(this_table) %in% variations] = standard_name
    }
    
    
    # check for any new names
    expected_cols = c("Dates_conducted","date", "Pollster", "Area", "Client" , "Samplesize", names(party_cols) ,"Lead")
    unexpected_cols = setdiff(names(this_table),expected_cols)
    if (length(unexpected_cols) > 0) {
        warning(paste0("Unexpected columns found: ", paste(unexpected_cols, collapse = ", ")))
    }
    
    #--- clean the party columns
    avail_party_cols = intersect(names(party_cols), names(this_table))
    
    missing_party_cols = setdiff(names(party_cols), avail_party_cols)
    if (length(missing_party_cols) > 0) {
        warning(paste0("Missing party columns: ", paste(missing_party_cols, collapse = ", ")))
    }
    
    this_table[,avail_party_cols] = suppressWarnings(sapply(this_table[,avail_party_cols], function(x) as.numeric(gsub("%", "", x))))
    
    # replace NAs with 0
    this_table[,avail_party_cols][is.na(this_table[,avail_party_cols])] = 0
    
    # scale:
    this_table[,avail_party_cols] = this_table[,avail_party_cols] / 100
    
    
    
    return(this_table)
}





#---- GE 2010 polling data ---------


#---- GE 2015 polling data ---------

url_GE2015 = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_United_Kingdom_general_election"
page_GE2015 = read_html(url_GE2015)

# find all the h3 tags, and get there titles
all_h3_tags = html_nodes(page_GE2015, "h3")
all_h3_titles = html_text(all_h3_tags)
# get all the years
all_years = as.numeric(grep("^[0-9]{4}", all_h3_titles,value = T))

GE2015_table_list = lapply(all_years, getTableByYear, page = page_GE2015, party_cols = party_cols)

GE2015_year_2010_table = getTableByYear(page = page_GE2015, year =  2010, party_cols = party_cols)
GE2015_year_2011_table = getTableByYear(page = page_GE2015, year =  2011, party_cols = party_cols)
GE2015_year_2012_table = getTableByYear(page = page_GE2015, year =  2012, party_cols = party_cols)
GE2015_year_2013_table = getTableByYear(page = page_GE2015, year =  2013, party_cols = party_cols)
GE2015_year_2014_table = getTableByYear(page = page_GE2015, year =  2014, party_cols = party_cols)
GE2015_year_2015_table = getTableByYear(page = page_GE2015, year =  2015, party_cols = party_cols)


# ***** DON't have SNP DATA...
# Before we can combine this with the rest, we need to deal with small parties

# A good way to deal with them would be to class them as:
# "Other, right", "Other, left", and "Other centre"

# And when there is no polling data for a period, we could use the actual general election results to fill the gap
# Potentially could take the figures for each party, and election before and after this period, and assume a straight line between them to give share at any particular date




#---- GE 2017 polling data ---------



url_GE2017 = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2017_United_Kingdom_general_election"
page_GE2017 = read_html(url_GE2017)
GE2017_year_2015_table = getTableByYear(page = page_GE2017, year =  2015, party_cols = party_cols)
GE2017_year_2016_table = getTableByYear(page = page_GE2017, year =  2016, party_cols = party_cols)
GE2017_year_2017_table = getTableByYear(page = page_GE2017, year =  2017, party_cols = party_cols)


this_overall_cols = c("date", "Pollster" , "Samplesize",names(party_cols))

this_overall_cols = this_overall_cols[this_overall_cols != "PC"]

GE2017_table = rbind(
    GE2017_year_2015_table[,this_overall_cols],
    GE2017_year_2016_table[,this_overall_cols],
    GE2017_year_2017_table[,this_overall_cols]
)



#---- GE 2019 polling data ---------

url_GE2019 = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_United_Kingdom_general_election"
page_GE2019 = read_html(url_GE2019)
GE2019_year_2017_table = getTableByYear(page = page_GE2019, year =  2017, party_cols = party_cols)
GE2019_year_2018_table = getTableByYear(page = page_GE2019, year =  2018, party_cols = party_cols)
GE2019_year_2019_table = getTableByYear(page = page_GE2019, year =  2019, party_cols = party_cols)


this_overall_cols = c("date", "Pollster" , "Samplesize",names(party_cols))

GE2019_table = rbind(
    GE2019_year_2017_table[,this_overall_cols],
    GE2019_year_2018_table[,this_overall_cols],
    GE2019_year_2019_table[,this_overall_cols]
)


#---- GE 2024 polling data ---------
url_GE2024 = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2024_United_Kingdom_general_election"
page_GE2024 = read_html(url_GE2024)



GE2024_year_2020_table = getTableByYear(page = page_GE2024,year =  2020, party_cols = party_cols)
GE2024_year_2021_table = getTableByYear(page = page_GE2024,year =  2021, party_cols = party_cols)
GE2024_year_2022_table = getTableByYear(page = page_GE2024,year =  2022, party_cols = party_cols)
GE2024_year_2023_table = getTableByYear(page = page_GE2024,year =  2023, party_cols = party_cols)
GE2024_year_2024_table = getTableByYear(page = page_GE2024,year =  2024, party_cols = party_cols)


this_overall_cols = c("date", "Pollster" , "Samplesize",names(party_cols))
this_overall_cols = this_overall_cols[this_overall_cols != "PC"]

GE2024_table = rbind(
    GE2024_year_2020_table[,this_overall_cols],
    GE2024_year_2021_table[,this_overall_cols],
    GE2024_year_2022_table[,this_overall_cols],
    GE2024_year_2023_table[,this_overall_cols],
    GE2024_year_2024_table[,this_overall_cols]
)


#--- latest polling data -------
# Read the Wikipedia page for latest UK general election polling data
url_latest = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_United_Kingdom_general_election"
page_latest = read_html(url_latest)


year_2024_table = getTableByYear(page_latest, 2024, party_cols)
year_2025_table = getTableByYear(page_latest, 2025, party_cols)
year_2026_table = getTableByYear(page_latest, 2026, party_cols)

# some eariler ones don't have PC, so just add to other
year_2024_table[,"Others"] = year_2024_table[,"Others"] + year_2024_table[,"PC"]
year_2025_table[,"Others"] = year_2025_table[,"Others"] + year_2025_table[,"PC"]
year_2026_table[,"Others"] = year_2026_table[,"Others"] + year_2026_table[,"PC"]

year_2024_table[,"PC"] = NULL
year_2025_table[,"PC"] = NULL
year_2026_table[,"PC"] = NULL


this_overall_cols = c("date", "Pollster" , "Samplesize",names(party_cols))
this_overall_cols = this_overall_cols[this_overall_cols != "PC"]


latest_table = rbind(
    year_2024_table[,this_overall_cols],
    year_2025_table[,this_overall_cols],
    year_2026_table[,this_overall_cols]
)

#--- combine all years

overall_cols = c("date", "Pollster" , "Samplesize",names(party_cols))
overall_cols = overall_cols[overall_cols != "PC"]


all_poll_data = rbind(
    GE2017_table[,overall_cols],
    
    GE2019_table[,overall_cols],
    
    GE2024_table[,overall_cols],
    
    latest_table[,overall_cols]

)

# make sure party_cols add to 100%
# ...and drop others
party_cols_2 = names(party_cols)[names(party_cols) %in% c("Others","PC") == F]
all_poll_data[,"Others"] = NULL
all_poll_data[, party_cols_2] = all_poll_data[, party_cols_2] / rowSums(all_poll_data[, party_cols_2])

#--- aggregate to a table of average polling percentages per party
# - there should be a row per day
# - we should weight by sample size
# - the number on that date should not just be the polls on that date, but an average of most recent polls, weighting more recent polls more heavily, using some sort of gaussian kernel




get_weighted_polls_base <- function(all_poll_data, party_cols, this_sigma = 7) {

    # Dynamically identify party columns (numeric columns except Samplesize)
    # Create the timeline
    all_dates = seq(min(all_poll_data$date), max(all_poll_data$date), by = "day")
    
    # 2. Updated Kernel Calculation
    calc_weighted_avg = function(target_date, all_poll_data, this_sigma, cols) {
        # Time difference in days (Target - Poll Date)
        diffs <- as.numeric(target_date - all_poll_data$date)
        
        # CALCULATE CAUSAL WEIGHTS:
        # If diffs < 0, the poll is in the future relative to target_date.
        # We set those weights to 0.
        time_weights <- exp(-(diffs^2) / (2 * this_sigma^2))
        time_weights[diffs < 0] <- 0 
        
        # Combined weight: Time Decay * Sample Size
        combined_weights <- time_weights * all_poll_data$Samplesize
        
        # Handle the case where no polls exist yet for a date
        if (sum(combined_weights) == 0) {
            return(rep(NA, length(cols)))
        }
        
        # Calculate weighted mean
        res <- sapply(cols, function(p) {
            weighted.mean(all_poll_data[[p]], combined_weights, na.rm = TRUE)
        })
        
        return(res)
    }
    
    calc_weighted_avg(target_date = all_dates[10], all_poll_data, this_sigma, party_cols)
    
    # 3. Apply over the sequence
    weighted_matrix <- t(sapply(all_dates, function(d) {
        calc_weighted_avg(d, all_poll_data, this_sigma, party_cols)
    }))
    
    # 4. Final Formatting
    results <- data.frame(date = all_dates, weighted_matrix)
    names(results)[-1] = party_cols # Ensure party names are preserved
    rownames(results) = NULL
    
    return(results)
}

weighted_polls = get_weighted_polls_base(all_poll_data, party_cols = party_cols_2, this_sigma = 30)

# # plot timeline with a line for each party (using base R)
# plot(weighted_polls$date, weighted_polls$Lab, type = "l", col = "red", ylim = c(0, 0.5), ylab = "Polling Percentage", xlab = "Date", main = "UK General Election Polling Averages")
# lines(weighted_polls$date, weighted_polls$Con, col = "blue")
# lines(weighted_polls$date, weighted_polls$LD, col = "orange")
# lines(weighted_polls$date, weighted_polls$Grn, col = "green")
# lines(weighted_polls$date, weighted_polls$SNP, col = "purple")
# lines(weighted_polls$date, weighted_polls$Ref, col = "brown")
# #lines(weighted_polls$date, weighted_polls$PC, col = "pink")
# #lines(weighted_polls$date, weighted_polls$Others, col = "grey")


#--- plot a stacked area chart
library(ggplot2)
ggplot_data = reshape2::melt(weighted_polls, id.vars = "date", measure.vars = party_cols_2,
                              variable.name = "Party", value.name = "Percentage")
ggplot(ggplot_data, aes(x = date, y = Percentage, fill = Party)) +
    geom_area() +
    labs(title = "UK General Election Polling Averages", x = "Date", y
 = "Polling Percentage") +
    scale_fill_manual(values = c("Lab" = "#ff7777",
                                 "LD" = "orange",
                                 "Con" = "#6688ff",
                                 
                                 "Grn" = "#55ff55",
                                 "SNP" = "yellow",
                                 "Ref_UKIP" = "purple",
                                 "PC" = "pink")) +
    # add horizontal line at 0.5
    geom_hline(yintercept = 0.5, linetype = 5, color = "#111111") +
    geom_hline(yintercept = 0.45, linetype = 2, color = "#222222") +
    geom_hline(yintercept = 0.55, linetype = 2, color = "#222222") +
    geom_hline(yintercept = 0.25, linetype = 2, color = "#555555") +
    geom_hline(yintercept = 0.75, linetype = 2, color = "#555555") +
    
    theme_minimal()



# library(glmnet)
# test_c = readRDS("../../Hongyi Handover/WU Analysis Handover/WU Analysis Handover/Models/insolvencyModel_claimant_R.rds")

# class(test_c)

# # get coefficeints from this model
# this_coeff = test_c$coefficients

