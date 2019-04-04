  
#  tutorial
# 'https://shiny.rstudio.com/articles/shinyapps.html'
# 'https://www.shinyapps.io/admin/#/dashboard'
source('config.R')
rsconnect::setAccountInfo(name=shiny_name,
                          token=shiny_token,
                          secret=shiny_secret)

setwd(shiny_path)
rsconnect::deployApp()

