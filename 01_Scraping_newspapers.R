
install.packages("rvest")
install.packages("RSelenium")
library(rvest)
library(RSelenium)


driver <- rsDriver(browser = "chrome")
remDr <- driver[["client"]]


remDr$navigate("https://www.example.com")


Sys.sleep(5)


start_date <- as.Date(readline("Gib das Startdatum im Format YYYY-MM-DD ein: "))
end_date <- as.Date(readline("Gib das Enddatum im Format YYYY-MM-DD ein: "))


date_sequence <- seq(start_date, end_date, by = "days")


for (selected_date in date_sequence) {
  date_input <- remDr$findElement(using = "css", value = "your-date-selector")
  date_input$clickElement()
  date_input$clearElement()
  date_input$sendKeysToElement(list(as.character(selected_date)))
  

  Sys.sleep(5)
  
 
  link_elements <- remDr$findElements(using = "css", value = "your-link-selector")
  

  links <- sapply(link_elements, function(el) el$getElementAttribute("href")[[1]])
  

  for (link in links) {

    remDr$navigate(link)
    

    Sys.sleep(5)
    

    webpage <- remDr$getPageSource()[[1]]
    

    page_content <- read_html(webpage)
    

    titles <- page_content %>% html_nodes("h1") %>% html_text()
    

    print(titles)
    

    remDr$goBack()
    

    Sys.sleep(3)
  }
}


remDr$close()
driver$server$stop()
