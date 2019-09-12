library(devtools)
#install_github("tidyverse/rvest")
#install.packages("V8")
#install.packages("RCurl")
#install.packages("XML")
#install.packages("htm2txt")
install.packages("zipcode")
library(V8)
library(rvest)
library(stringr)
library(RSelenium)
library(RCurl)
library(XML)
library(magrittr)
library(stringr)
library(zipcode)

# declaring dataframe for Kijiji and initializing other vasriables

kijiji_base_url <- "https://kijiji.ca"
URL_kijiji <-
  "https://www.kijiji.ca/b-fitness-personal-trainer/canada/c83l0"

kijiji_df <- data.frame(
  TrainerName = character(),
  content = character(),
  location = character(),
  pictures = character(),
  email = character(),
  phoneNumber = character(),
  profileLinkOnKajiji = character(),
  stringsAsFactors = FALSE
)

#starting Selenium browser


#browser <- remoteDriver(port = 5556)
#browser$open()

# reading Kijiji search pages and getting Kijiji listings; initially the first 100 pages

for (i in 1:100)
{
  kijiji_searchpage_html <- read_html(URL_kijiji)
  kijiji_listings <-
    kijiji_searchpage_html %>%
    html_nodes(".title") %>%
    html_attr("href")
  
  #for loop to get all links in the searchpage
  
  for (i in 1:length(kijiji_listings))
  {
    if (all(toString(kijiji_listings[i])=="NA"))
    {
      next
    }
    
    phone <- "NA"
    emails <- "NA"
    
    kijiji_listings[i] <-
      paste(c(kijiji_base_url, toString(kijiji_listings[i])), collapse = "")
    
    print(paste("Kijiji profile link:", toString(kijiji_listings[i])))
    
    #Entering Listing and grabbing content
    
    kijiji_trainerDetails <-
      read_html(kijiji_listings[i]) %>%
      html_nodes("p") %>%
      html_text()
    
    kijiji_trainerDetails <-
      paste(kijiji_trainerDetails, collapse = "\n")
    
    print(paste("Kijiji Trainer Details", toString(kijiji_trainerDetails)))
    
    #getting Names
    
    p <- read_html(kijiji_listings[i]) %>% html_text()
    kijiji_Trainer_Name <- str_match_all(p,'"sellerName":"(.*?)"')[[1]][,2][1]
    print(kijiji_Trainer_Name)
    
    kijiji_Trainer_Loc <-
      read_html(kijiji_listings[i]) %>%
      html_nodes("span.address-3617944557") %>%
      html_text()
    
    #getting picture
    
    kijiji_Trainer_Image <-
      read_html(kijiji_listings[i]) %>%
    html_nodes("img") %>%
      #html_nodes(".link-4200870613")
      html_attr("src")
    
    kijiji_Trainer_Image <-
      paste(kijiji_Trainer_Image, collapse = "\n")
    
    print(paste(
      "Kijiji trainer images links",
      toString(kijiji_Trainer_Image)
    ))
    
    #getting phone number
    
    #phoneNumber = gsub("[^0-9/]","", kijiji_listing_details_text)   didn't use this code because it didn't cover all cases
    
    #gsub("[- .)(+]|[a-zA-Z]*:?","", kijiji_listing_details_text)
    
    #alternate code to match phone number
    
    phone <- regex(
      "
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [)- ]?   # optional closing parens, dash, or space
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{4}) # three more numbers
  ",
      comments = TRUE
    )
    
    phone <- str_match(kijiji_trainerDetails, phone)
    phone <- phone[1, 1]
    
    print(paste("Kijiji Trainer phone: ", toString(phone)))
    
    #detecting and getting email address if present
    
    
    emails = unlist(regmatches(
      kijiji_trainerDetails,
      gregexpr(
        "([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))",
        kijiji_trainerDetails
      )
    ))
    
    emails <- paste(emails, collapse = " ")
    
    print(paste("Kijiji Trainer email(s): ", toString(emails)))
    
    #populating Data Frame
    
    kijiji_df[nrow(kijiji_df) + 1, ] =
      c(
        toString(kijiji_Trainer_Name),
        toString(kijiji_trainerDetails),
        toString(kijiji_Trainer_Loc),
        toString(kijiji_Trainer_Image),
        toString(emails),
        toString(phone),
        toString(kijiji_listings[i])
      )
  }
  
  #Going to next Page
  
  nextPageLink <-
    read_html(URL_kijiji) %>%
    html_nodes("a[title=Next]") %>%
    html_attr("href")
  
  URL_kijiji <-
    paste(c(kijiji_base_url, nextPageLink), collapse = "")
  
  
}




#Testing


#link <- 
#  "https://www.kijiji.ca/v-fitness-personal-trainer/mississauga-peel-region/private-studio-personal-training-at-nashdfitness-free-sessions/1298817470"
#  kijiji_Trainer_Loc <-
#  read_html(link) %>%
#  html_nodes("span.address-3617944557") %>%
#  html_text()





