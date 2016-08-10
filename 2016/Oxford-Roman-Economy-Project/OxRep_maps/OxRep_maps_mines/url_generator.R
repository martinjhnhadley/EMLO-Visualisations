

url_generator <- function(sitecountry = NA,
                          sitename = NA){
  paste0("http://oxrep.classics.ox.ac.uk/databases/sites/",
         gsub(" ","_",tolower(sitecountry)),
         "/",
         gsub(" ","_",tolower(sitename)),
         "_mine/")
}

## Apply to all
constructed_urls <-
  lapply(1:nrow(sites_df), function(x)
    url_generator(sitecountry = sites_df[x, "sitecountry"],
                  sitename = sites_df[x, "sitename"]))

constructed_urls_test <- lapply(
  constructed_urls,
  function(x){
    url_success(
      x,
      config(followlocation = 0L)
    )
  }
)

constructed_urls_test <- unlist(constructed_urls_test)

constructed_urls[which(constructed_urls_test == FALSE)]

## =============== Problem Characters ========================

this_url_works <- "http://oxrep.classics.ox.ac.uk/databases/sites/romania/petro%C5%9Fani_mine/"

this_url_fails <- "http://oxrep.classics.ox.ac.uk/databases/sites/spain/peña_de_hierro_mine/"



all_urls_as_string <- unlist(strsplit(paste0(constructed_urls,collapse = ""),""))

unique_s <- unique(all_urls_as_string)

unique_s[!grepl("[a-z]",unique_s)]


constructed_urls[grepl("ñ", constructed_urls)]

