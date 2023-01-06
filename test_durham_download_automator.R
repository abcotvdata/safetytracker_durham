# install and load the required packages
# install.packages("rvest")
library(rvest)
library(stringr)

# read the webpage
webpage <- read_html("https://www.durhamnc.gov/Archive.aspx?AMID=150")

# extract all the a tags
a_tags <- html_nodes(webpage, "a")

# extract the link content from each a tag
links <- html_attr(a_tags, "href")

# print the list of links
print(links)

# working now to be able to capture the latest link

# filter the list to include only those links that contain the text "aspx"
filtered_links <- grep("aspx", links, value = TRUE)

value <- str_extract(filtered_links[[2]], "=.*")
value <- sub("=", "", value)

# print the filtered list of links
print(filtered_links)

download.file(paste0("https://www.durhamnc.gov/ArchiveCenter/ViewFile/Item/",value),
              paste0("data/source/durham_weekly_",format(Sys.Date(), "%y%b%d"),".pdf"))

download.file(paste0("https://www.durhamnc.gov/ArchiveCenter/ViewFile/Item/",value),
              basename)




