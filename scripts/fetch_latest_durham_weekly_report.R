library(rvest)
library(stringr)

# read the webpage
webpage <- read_html("https://www.durhamnc.gov/Archive.aspx?AMID=172")
# extract all a tags
a_tags <- html_nodes(webpage, "a")
# extract all links from a tags
links <- html_attr(a_tags, "href")
# print the list of links
# print(links)
# pdf links contain aspx, so filtering to just those
filtered_links <- grep("aspx", links, value = TRUE)
# the second aspx link will always be the latest file
# cleaning to just get the file number to open the actual file
value <- str_extract(filtered_links[[2]], "=.*")
value <- sub("=", "", value)
# download the file to the source data directory for scraping
download.file(paste0("https://www.durhamnc.gov/ArchiveCenter/ViewFile/Item/",value),
              "data/source/period_to_date.pdf")



