library(rvest)
url = "https://en.wikipedia.org/wiki/2024_United_Kingdom_general_election"
url = "https://en.wikipedia.org/wiki/2019_United_Kingdom_general_election"
url = "https://en.wikipedia.org/wiki/2017_United_Kingdom_general_election"
url = "https://en.wikipedia.org/wiki/2015_United_Kingdom_general_election"
url = "https://en.wikipedia.org/wiki/2010_United_Kingdom_general_election"
url = "https://en.wikipedia.org/wiki/2005_United_Kingdom_general_election"
url = "https://en.wikipedia.org/wiki/2001_United_Kingdom_general_election"
url = "https://en.wikipedia.org/wiki/1997_United_Kingdom_general_election"
url = "https://en.wikipedia.org/wiki/1992_United_Kingdom_general_election"


page = read_html(url)

# Find the div containing the h3 with "Full_results"
# Then get the table that follows that div

# H3:
table = page |> html_element(xpath = "//div[.//h3[@id='Full_results']]/following-sibling::table[1]")

# H4
table = page |> html_element(xpath = "//div[.//h4[@id='Full_results']]/following-sibling::table[1]")

# h2
# first table:
table = page |> html_element(xpath = "//div[.//h2[@id='Results']]/following-sibling::table[1]")
# second table:
table = page |> html_element(xpath = "//div[.//h2[@id='Results']]/following-sibling::table[2]")


# To parse as data frame:
df = table |> html_table(fill = TRUE)


cc(df)

