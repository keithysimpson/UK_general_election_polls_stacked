# get long term historic data



#--- [1] csv data ----------------------------
# there are two versions of the file
# csv is easier to deal with, but has limited data (i.e. lots of parties grouped as "other")

historic_df = read.csv("1918-2019election_results.csv")

#----- filter some rows: -------

#--- filter ireland:
#table(historic_df$country.region)

historic_df = historic_df[historic_df$country.region %in% c("Ireland", "Northern Ireland") == FALSE,]

#---- negative votes:
#View(historic_df[historic_df$total_votes < 0,])
#table(historic_df$total_votes < 0)

historic_df = historic_df[historic_df$total_votes >= 0,]

#--- drop na elections
#View(historic_df[is.na(historic_df$election),])
historic_df = historic_df[!is.na(historic_df$election),]


#--- clean snp votes:
aggregate(!is.na(natSW_votes) ~ election, historic_df, sum) 

# names(historic_df)
# constituency_id
# seats
# constituency_name
# country.region
# electorate
# con_votes
# con_share
# lib_votes
# lib_share
# lab_votes
# lab_share
# natSW_votes
# natSW_share
# oth_votes
# oth_share
# total_votes
# turnout
# election
# boundary_set


#----- aggregate to each election ------------

total_votes_by_election = aggregate(cbind(con_votes, lab_votes, natSW_votes, lib_votes, oth_votes, total_votes) ~ election, historic_df, sum, na.rm = TRUE, na.action = na.pass)

total_votes_by_election = as.data.frame(total_votes_by_election)


total_votes_by_election$election[total_votes_by_election$election == "1974F"] = 1974 + (2/12)

total_votes_by_election$election[total_votes_by_election$election == "1974O"] = 1974 + (10/12)

total_votes_by_election$election = as.numeric(total_votes_by_election$election)

cc(total_votes_by_election)

party_cols = c("con_votes", "lib_votes", "lab_votes", "natSW_votes", "oth_votes")

#----- convert to percentage share -----
total_share_by_election = total_votes_by_election
total_share_by_election[,party_cols] = total_votes_by_election[,party_cols] / total_votes_by_election$total_votes
total_share_by_election$total_votes = NULL

# reorder columns
total_share_by_election = total_share_by_election[,c("election", party_cols)]

names(total_share_by_election) = c("election", "Con", "LD", "Lab", "SNP", "Others")


#----- plot result ---------------
# plot as a 100% stacked area chart

library(ggplot2)

ggplot_data = reshape2::melt(total_share_by_election, id.vars = "election", measure.vars = c("Con", "LD", "Lab", "SNP", "Others"),
    variable.name = "Party", value.name = "Percentage")

ggplot(ggplot_data, aes(x = election, y = Percentage, fill = Party)) + 
    geom_area() +
    labs(title = "UK General Election Polling Averages", x = "Date", y = "Polling Percentage") +
    scale_fill_manual(values = c(
                                 "Con" = "#6688ff",
                                 "LD" = "orange",
                                 "Lab" = "#ff7777",
                                 "SNP" = "yellow",
                                 "Others" = "gray")) +
    # add horizontal line at 0.5
    geom_hline(yintercept = 0.5, linetype = 5, color = "#111111") +
    geom_hline(yintercept = 0.45, linetype = 2, color = "#222222") +
    geom_hline(yintercept = 0.55, linetype = 2, color = "#222222") +
    geom_hline(yintercept = 0.25, linetype = 2, color = "#555555") +
    geom_hline(yintercept = 0.75, linetype = 2, color = "#555555") +
    
    theme_minimal()


#---- [2] xlsx file --------------

library(openxlsx)

# get all the sheet names
all_sheet_names = getSheetNames("1918-2019election_results_by_pcon.xlsx")

all_sheet_names = all_sheet_names[all_sheet_names != "University Seats"]

full_data_list = list()

#----- loop through each sheet --------
# progress bar initialise
pb = txtProgressBar(min = 0, max = length(all_sheet_names), style = 3)

for (sheet_name in all_sheet_names) {
    setTxtProgressBar(pb, which(all_sheet_names == sheet_name))
    #sheet_name = all_sheet_names[1]
    # read in the data
    #----- get the header names -----
    # header 1:
    h1 = read.xlsx("1918-2019election_results_by_pcon.xlsx", sheet = sheet_name, rows = 3,colNames = F)

    h1 = as.character(unlist(h1))

    

    # header 2:
    h2 = read.xlsx("1918-2019election_results_by_pcon.xlsx", sheet = sheet_name, rows = 4,colNames = F)

    h2 = as.character(unlist(h2))

    full_names = trimws(h2)
    votes_cols = which(full_names == "Votes")
    if (length(votes_cols) != length(h1)) {
        stop("votes_cols != length(h1)")
    }

    party_vote_cols = paste0(h1, " Votes")
    full_names[votes_cols] = party_vote_cols

    #---- read in the data ----------
    this_df = read.xlsx("1918-2019election_results_by_pcon.xlsx", sheet = sheet_name, startRow = 4, colNames = T)

    # if there are names not in h2, then drop:
    this_df = this_df[,gsub("[.][0-9]{1,2}$","",names(this_df)) %in% gsub(" ",".",trimws(h2))]

    if (ncol(this_df) != length(full_names)) {
        stop("ncol(this_df) != length(full_names)")
    }

    names(this_df) = full_names


    #--- rename to lowercase for consistency
    names(this_df) = tolower(names(this_df))
    party_vote_cols = tolower(party_vote_cols)

    #--- drop "vote share" columns
    this_df = this_df[,(names(this_df) == "vote share") == FALSE,]

    #----- filter some rows --------
    # drop based on country:
    #table(this_df[,"Country/Region"], useNA = "ifany")
    #...drop NA
    this_df = this_df[!is.na(this_df[,"country/region"]),]
    #...drop Ireland and Northern Ireland
    this_df = this_df[this_df[,"country/region"] %in% c("Ireland", "Northern Ireland") == FALSE,]

    #...drop University seats
    this_df = this_df[this_df[,"country/region"] %in% c("University") == FALSE,]

    #...drop negative votes
    this_df = this_df[this_df[,"total votes"] >= 0,]


    #--- make sure the vote cols are numeric
    this_df[,party_vote_cols] = lapply(this_df[,party_vote_cols], as.numeric)


    


    # of the remaining rows, drop party cols with no votes
    cols_with_no_votes = party_vote_cols[colSums(this_df[,party_vote_cols], na.rm = T) == 0]

    this_parties = h1[!(party_vote_cols %in% cols_with_no_votes)]      
  
    if (length(cols_with_no_votes) > 0) {
        # drop these:
        this_df = this_df[,!(names(this_df) %in% cols_with_no_votes)]
        this_parties = h1[!(party_vote_cols %in% cols_with_no_votes)]
        party_vote_cols = party_vote_cols[!(party_vote_cols %in% cols_with_no_votes)]
    }
    
    # save the data
    this_data = list(
        "election" = sheet_name,
        "data" = this_df,
        "all_parties" = h1,
        "parties" = this_parties,
        "party_cols" = party_vote_cols
    )

    full_data_list[[sheet_name]] = this_data
}


#saveRDS(full_data_list, "full_data_extract_1918_2019_RAW.RDS")

# Next steps:
# for each election need to deal with the various partys, assigning them to one of our categories:

# Other Right
# Reform_UKIP
# Conservative
# LD
# Labour
# Green Party
# SNP
# Other Left

#-- need to deal with the fact that sometimes SNP and PC are in the same column (so need to aggregate by country)
#--- are there any cases where a party appears twice for a particular year?

#--- need to deal with "coalition" parties, where parties agree to stand asside from each other

#--- need to deal with "Independent Conservative" parties

#-- To start, lets look at all the party names

all_party_names = unique(unlist(sapply(full_data_list, "[[", "parties")))
all_party_cols = unique(unlist(sapply(full_data_list, "[[", "party_cols")))


#--- for each year aggregate the data
full_raw_table = NULL
for (year in names(full_data_list)) {
    # year = names(full_data_list)[1]
    this_year = full_data_list[[year]]

    this_votes = this_year$data[,c(this_year$party_cols, "total votes")]

    # make sure they are all numeric
    this_votes = apply(this_votes, 2, as.numeric)

    this_total = colSums(this_votes,na.rm = T)

    missing_cols = all_party_names[!(all_party_names %in% names(this_total))]
    
    

    full_raw_table = rbind(full_raw_table, this_total[match(c(all_party_cols, "total votes"),names(this_total))])
}


colnames(full_raw_table) = c(all_party_cols, "total votes")
rownames(full_raw_table) = names(full_data_list)

cc(full_raw_table,includeRowNames = T)

# sense check why is there some years with non zero DUP names?


# sapply(full_data_list, function(x) {
#     sum(x$parties == "National")
# })

# length(full_data_list$`1918`$parties)

# full_data_list$`1945`$parties

# names(full_data_list$`1918`$data)
# ncol(full_data_list$`1979`$data)


# with(full_data_list$`1918`, data[!is.na(data$`sinn fein votes`),]) |> View()



# if we excluded university, what percentage of the vote is that?


# sapply(full_data_list, function(x) {
#     if ("University" %in% x$data$`country/region`) {
#         total_votes = as.numeric(x$data$`total votes`)
#         this_result = sum(total_votes[x$data$`country/region` == "University"],na.rm = T) / sum(total_votes,na.rm = T)
#     } else {
#         this_result = 0
#     }
#     return(this_result)
    
# })


party_groupings = list(
    "Other Right" = c("constitutionalist votes",
	                  "national liberal/ national liberal and conservative votes"),
    "Reform_UKIP" = c("ukip votes",
                      "brexit votes"),
    "Conservative" = c("conservative party votes",
                    "independent conservative votes",
                    "conservative votes"),
    "Liberal Democrats" = c("coalition liberal votes",
                "liberal votes",
                "independent liberal votes",
                "national liberal votes",
                "alliance votes",
                "liberal democrat votes",
                "liberal democrats votes"),
    "Labour" = c("coalition labour  votes",
                "labour votes",
                "independent labour  votes",
                "labour  votes"),
    "SNP" = c("snp votes"),
    "Other Left" = c("plaid cymru votes",
                "communist votes",
                "national labour votes",
                "independent labour party votes",
                "independent labour votes",
                "common wealth movement votes"),
    "Green Party" = c("green votes"),
    "Other Unknown" = c("other votes",
                "other  votes",
                "independent votes",
                "national votes",
                "national independent votes")

)

nat_partys_to_split =  c("snp/plaid cymru votes",
                        "pc/snp votes",
                        "nationalist (wales/ scotland) votes")



#---- aggregate the data, combining parties into groups
#---- loop through each year

full_aggregated_table = NULL
for (year in names(full_data_list)) {
    # year = names(full_data_list)[1]
    this_year = full_data_list[[year]]

    this_votes = this_year$data[,c(this_year$party_cols, "total votes")]

    # make sure they are all numeric
    this_votes = apply(this_votes, 2, as.numeric)

    this_result = NULL
    for (this_group in names(party_groupings)) {
        # this_group = names(party_groupings)[1]
        this_party_list = party_groupings[[this_group]]
      
        group_party_cols_in_data = intersect(this_party_list, this_year$party_cols)
        if (length(group_party_cols_in_data) >= 1) {
            this_total = sum(colSums(this_votes[,group_party_cols_in_data, drop = F],na.rm = T),na.rm = T)
        } else {
            this_total = 0
        }
        this_result = c(this_result, this_total)
        }
  
    # check for any nat parties
    if (any(nat_partys_to_split %in% this_year$party_cols)) {
        nat_party_cols_in_data = intersect(nat_partys_to_split, this_year$party_cols)
        is_scotland = this_year$data$`country/region` == "Scotland"
        snp_total = sum(colSums(this_votes[is_scotland,nat_party_cols_in_data, drop = F],na.rm = T),na.rm = T)
      
        is_wales = this_year$data$`country/region` == "Wales"
        pc_total = sum(colSums(this_votes[is_wales,nat_party_cols_in_data, drop = F],na.rm = T),na.rm = T)
      
        # add to the existing columns:
        this_result[names(party_groupings) == "SNP"] = this_result[names(party_groupings) == "SNP"] + snp_total
        this_result[names(party_groupings) == "Other Left"] = this_result[names(party_groupings) == "Other Left"] + pc_total
    }
  
    this_result_share = this_result / sum(this_votes[,"total votes"],na.rm = T)
  
    full_aggregated_table = rbind(full_aggregated_table, this_result_share)
    
}


rownames(full_aggregated_table) = names(full_data_list)
colnames(full_aggregated_table) = names(party_groupings)

#cc(full_aggregated_table, includeRowNames = T)
