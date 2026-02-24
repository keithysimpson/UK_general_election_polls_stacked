#View(poll_data)
party_cols = list("Reform_UKIP" = c("Ref","Reform", "Reform UK", "ReformUK", "Reform UK", "Brexit", "Brx","UKIP"),
               "Conservative" = "Con", 
               "Lib_Dem" = c("LD", "Lib Dem", "Lib Dems", "Lib. Dems"), 
               "Labour" = c("Lab"), 
              "SNP" = c("SNP"), 
              "Green" = c("Green", "Greens", "Grn"),
              "Others" = c("PC", "Plaid", "Plaid Cymru","Other", "Others", "Change UK","Chg"))


right_party_cols = c("Reform_UKIP")
left_party_cols = c("SNP","Green")


parse_poll_date = function(date_string, year = 2024) {
    # Remove whitespace
    date_string = trimws(date_string)
    
    # if it contains no numbers, then return NA
    if (!grepl("[0-9]", date_string)) {
        return(NA)
    }
    
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
        
        # if start date is greater than end date, then this probably means that this range spanned a year
        # for example "14 Dec – 9 Jan"
        # in this case its not clear which year to use, but lets assume that generally the end date is correct, and this is why the poll data ended up on this years table
        # And so we need to take one year off the start date
        if (start_date > end_date) {
            start_date = as.Date(paste(year-1, start_month, start_day, sep = "-"))
        }

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

getTableByYear <- function(page, year, party_cols, ignore_merged = TRUE) {
    # find the table for this year
    
    xpath = paste0("//h3[@id='", year, "']/ancestor::div/following-sibling::table[1]")
    this_table_raw = html_node(page, xpath = xpath) |> html_table(fill = TRUE)
    #
    if (ignore_merged == TRUE) {
        this_table = getCleanPollingTable(page, year)
        #dim(this_table)
        #dim(this_table_raw)
        if (ncol(this_table) != ncol(this_table_raw)) {
            stop("Something went wrong, tables are different sizes")
        }
        names(this_table) = names(this_table_raw)
    } else {
        this_table = this_table_raw
        # Drop the first row if it's a duplicate header
        if (this_table[1,1] == names(this_table)[1]) {
        # drop the first row
        this_table = this_table[-1, ]
        }
    }
    


    
    
    # print column names
    #print(names(this_table))

    #--- get a clean date
    names(this_table)[1] = "Dates_conducted"
    #suppressWarnings(as.Date(sapply(this_table$Dates_conducted[75:80], parse_poll_date, year = year)))


    # *** remove any obviously broken row:
    rows_to_drop = grep("%",this_table$Dates_conducted)

    if (length(rows_to_drop) > 0) {
        this_table = this_table[-rows_to_drop, ]
        print(paste0("...Dropping ", length(rows_to_drop), " rows"))
    }

    #suppressWarnings(as.Date(sapply(this_table$Dates_conducted[207], parse_poll_date, year = year)))

    # convert the date
    this_table$date = suppressWarnings(as.Date(sapply(this_table$Dates_conducted, parse_poll_date, year = year)))
    
    # drop any NA dates:
    print(paste0("Dropping ", sum(is.na(this_table$date)), " NA dates"))
    this_table = this_table[!is.na(this_table$date), ]




    #--- clean the sample size column
    names(this_table)[grep("sample",names(this_table),ignore.case = T)] = "Samplesize"


    # *** remove any obviously broken row:
    rows_to_drop = grep("%",this_table$Samplesize)

    if (length(rows_to_drop) > 0) {
        this_table = this_table[-rows_to_drop, ]
        print(paste0("...Dropping ", length(rows_to_drop), " rows"))
    }


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


    # *** remove any obviously broken row:
    rows_to_drop = grep("%",this_table$Pollster)

    if (length(rows_to_drop) > 0) {
        this_table = this_table[-rows_to_drop, ]
        print(paste0("...Dropping ", length(rows_to_drop), " rows"))
    }
    
    
    #--- deal with different party names:
    
    # sometimes partys ends with a ".", lets remove this
    names(this_table) = gsub("\\.$", "", names(this_table))
    #cc(this_table)
    
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
    #cc(this_table)
    avail_cols_index = names(this_table) %in% avail_party_cols
    this_table[,avail_cols_index] = suppressWarnings(sapply(this_table[,avail_cols_index], function(x) as.numeric(gsub("%", "", x))))
    
    # replace NAs with 0
    this_table[,avail_cols_index][is.na(this_table[,avail_cols_index])] = 0
    
    # there may now be some repeated party names e.g. if a part changed names and we've coded both columns under one name
    #this_col_names = gsub("\\.[0-9]$", "", names(this_table))
    
    this_col_names = names(this_table)

    raw_party_cols = names(this_table)[avail_cols_index]
    #raw_party_names = this_col_names[this_col_names %in% avail_party_cols]

    duplicated_parties = unique(raw_party_cols[duplicated(raw_party_cols)])

    if (length(duplicated_parties) > 0) {
        for (dup_party in duplicated_parties) {
            # dup_party = duplicated_parties[1]
            # dup_party = duplicated_parties[2]
            dup_index = which(this_col_names == dup_party)

            #this_group = this_table[,dup_index]

            this_total = rowSums(this_table[,dup_index], na.rm = TRUE)
            # drop the duplicate columns
            this_table = this_table[,-dup_index]
            this_col_names = this_col_names[-dup_index]
            # add in the total
            this_table[,dup_party] = this_total
        }
    }


    # scale:
    this_table[,avail_party_cols] = this_table[,avail_party_cols] / 100
    
    
    
    return(this_table)
}

getCleanPollingTable <- function(page, year) {
    # need to deal with the fact that sometimes there are merged cells
    # lets just drop these:

    xpath = paste0("//h3[@id='", year, "']/ancestor::div/following-sibling::table[1]")

    # Get the table node
    table_node = html_node(page, xpath = xpath)

    # Get all rows in the table body
    rows = html_nodes(table_node, "tr")

    # Identify which rows have merged cells
    has_merged = sapply(rows, function(row) {
        cells = html_nodes(row, "td, th")
        rowspans = html_attr(cells, "rowspan")
        colspans = html_attr(cells, "colspan")
        # Return TRUE if any cell has rowspan or colspan
        any(!is.na(rowspans) | !is.na(colspans))
    })

    # print the number and percent of merged rows:
    cat("Number of merged rows:", sum(has_merged), "\n")
    cat("Percentage of merged rows:", round(sum(has_merged) / length(rows) * 100, 2), "%\n")


    # Get rows without merged cells
    clean_rows = rows[!has_merged]

    # Now extract the table from only clean rows
    data_list = lapply(clean_rows, function(row) {
        cells = html_nodes(row, "td, th")
        this_result = sapply(cells, html_text, trim = TRUE)

        # remove anything after a new line character
        this_result = gsub("\n.*", "", this_result)

        # sometimes there are two percentages e.g. "4%5%", take the first one:
        this_result = gsub("%.*", "%", this_result)
    
        return(this_result)
    })
    
    row_lengths = lengths(data_list)
    max_row_length = max(row_lengths)
    data_list = lapply(data_list, function(row) {
        if (length(row) < max_row_length) {
            row = c(row, rep(NA, max_row_length - length(row)))
        }
        return(row)
    })

    # Convert to data frame
    this_table = as.data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE)

    # Set column names from first row if needed
    if (nrow(this_table) > 0) {
        colnames(this_table) = this_table[1, ]
        this_table = this_table[-1, ]
    }




    return(this_table)
}


getYears <- function(this_page) {
    # find all the h3 tags, and get there titles
    all_h3_tags = html_nodes(this_page, "h3")
    all_h3_titles = html_text(all_h3_tags)
    # get all the years
    all_years = as.numeric(grep("^[0-9]{4}", all_h3_titles,value = T))

    return(all_years)
}



# make sure each table has all the target columns, and then join them
joinPollTableList <- function(this_table_list, target_var_list) {
        this_clean_table_list = lapply(this_table_list, function(x) {
        miss_vars = setdiff(target_var_list, names(x))
        x[,miss_vars] = NA
        return(x[,target_var_list])
    })

    this_clean_table = do.call(rbind, this_clean_table_list)
  
    return(this_clean_table)
}


formatPollTable <- function(this_table_list, this_year){
    
  
    #---- join the tables in table lists:
    # define target columns:
    target_party_cols = names(party_cols)
    target_var_list = c("date","Pollster","Samplesize",target_party_cols)

    # make sure each table has all the target columns, and then join them
    this_GE_polls = joinPollTableList(this_table_list, target_var_list)

    # order by date
    this_GE_polls = this_GE_polls[order(this_GE_polls$date),]


    #--- split "other" into "right" and "left"

    #---- deal with missing parties:
    #cc(this_GE_polls)
    # work out which parties are missing...
    # Con, LD, and Lab, will always be there, but what about SNP, Green, Reform, PC
    # once we know whats missing we can look at the previous and following GE results
    # and work out the start and end of the "other right" and "other left" parties

    missing_cols = target_party_cols[which(colSums(this_GE_polls[,target_party_cols],na.rm = T) == 0)]

    this_GE_polls[,missing_cols] = 0

    missing_cols_left = missing_cols[which(missing_cols %in% left_party_cols)]
    missing_cols_right = missing_cols[which(missing_cols %in% right_party_cols)]



    #---- make sure the rows add to 100%
    this_total_excess = rowSums(this_GE_polls[,target_party_cols]) - 1
    # adjust Other to make it 100%
    this_GE_polls$Others = this_GE_polls$Others - this_total_excess





    #---- for this year, get the GE results for the start and end of this period
    # we will use this to decide how much "other" to assign to left vs right

    this_GE_index = which(GE_results$year == this_year)


    this_other_match = data.frame(
        "date" =GE_results[this_GE_index + -1:0,"date"], 
        "other_right" = rowSums(GE_results[this_GE_index + -1:0,c("Other_Right", missing_cols_right),drop = F],na.rm = T),
        "other_left" = rowSums(GE_results[this_GE_index + -1:0,
            c("Other_Left", missing_cols_left),drop = F],na.rm = T)
    )


    this_other_match$percent_left = this_other_match$other_left / (this_other_match$other_left + this_other_match$other_right)




    # now make a column in this_GE_polls for percent_left
    # use the dates and do a linear interpolation between the two dates in this_other_match

    this_GE_polls$percent_left = approx(this_other_match$date, this_other_match$percent_left, this_GE_polls$date, rule = 2)$y

    this_GE_polls$Other_Left = this_GE_polls$percent_left * this_GE_polls$Others

    this_GE_polls$Other_Right = this_GE_polls$Others - this_GE_polls$Other_Left
  
    return(this_GE_polls)
}
