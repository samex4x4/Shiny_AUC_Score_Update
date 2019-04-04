options(shiny.maxRequestSize = 9*1024^2)

library(dplyr)
library(data.table)
library(googlesheets)
source('config.R')


## google Sheet name
table='rank'
## google token
## https://cran.r-project.org/web/packages/googlesheets/vignettes/managing-auth-tokens.html
gs_auth(token = google_token)

## correct answer data  
result=read.csv(answer_data, stringsAsFactors=FALSE)


# check submit data
check_data <- function(data){
  if (!all(names(data) == c('grup_no', 'target'))){
    return('Columns name not fitted. See example data.')}
  
  if(nrow(data) != 1000){
    return('Submit data rows not equal 1000. Please check data.')}
  
  if(!all(sort(result$grup_no) == sort(data$grup_no))){ 
    return("columns error:grup_no cannot fit all data. Please check data.")}
  
  return(NULL)
}



## https://github.com/ben519/mltools/blob/master/R/auc_roc.R
auc_roc <- function(preds, actuals, returnDT=FALSE){
  # Calculate area under the ROC curve
  # If returnDT = TRUE, a data.table is returned
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  Pred <- NULL
  Actual <- NULL
  CumulativeFPR <- NULL
  CountFalse <- NULL
  CumulativeTPR <- NULL
  CountTrue <- NULL
  AdditionalArea <- NULL
  CumulativeArea <- NULL
  
  #--------------------------------------------------
  
  # Check if every prediction is identical and if so, return 0.5
  if(length(unique(preds)) == 1L) return(0.5)
  
  # Convert actuals to numeric if it's an ordered factor
  if(is(actuals, "factor")){
    if(is.ordered(actuals) & length(levels(actuals)) == 2) actuals <- as.numeric(actuals) - 1 else stop("actuals is type factor, but is unordered. Make it an ordered factor.")
  }
  
  dt <- data.table(Pred=preds, Actual=actuals*1L)
  setorder(dt, -Pred)
  
  dt <- dt[ , {
    CountTrue = sum(Actual)
    list(CountFalse=.N - CountTrue, CountTrue=CountTrue)
  }, by=Pred]
  
  # Calculate the CumulativeFalsePositiveRate and CumulativeTruePositiveRate
  dt[, CumulativeFPR := cumsum(CountFalse)/sum(CountFalse)]
  dt[, CumulativeTPR := cumsum(CountTrue)/sum(CountTrue)]
  
  # Calculate AUC ROC
  dt[, AdditionalArea := c(head(CumulativeFPR, 1) * head(CumulativeTPR, 1)/2,
                           (tail(CumulativeFPR, -1) - head(CumulativeFPR, -1)) * (head(CumulativeTPR, -1) + (tail(CumulativeTPR, -1) - head(CumulativeTPR, -1))/2))]
  dt[, CumulativeArea := cumsum(AdditionalArea)]
  
  # Return the desired result
  if(returnDT) return(dt[]) else return(tail(dt$CumulativeArea, 1))
}



## Shiny
function(input, output, session) {
  output$check_data <- renderText({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    submmit_data=read.csv(inFile$datapath, stringsAsFactors=FALSE)
    return(check_data(submmit_data))
  })
  
  output$contents <- DT::renderDataTable({
    inFile <- input$file1
    
    if (is.null(inFile)){

      sheet <- gs_title(table)
      rank=gs_read_csv(sheet) %>% arrange(desc(submmit_time))
      
      return(DT::datatable(rank, options = list(paging = FALSE, searching = FALSE,dom = 'ft',ordering=TRUE)))
    }

    submmit_data=read.csv(inFile$datapath)
    
    data_type=check_data(submmit_data)
    
    if(is.null(data_type)){
      
      
      sheet <- gs_title(table)
      
      # merge submit data and answer data
      result_merge=result %>% inner_join(submmit_data, by='grup_no')
      auc=auc_roc(result_merge$target , result_merge$target_actual)
      
      gs_add_row(sheet, input=c(round(auc, 5), as.character(Sys.time())))
      
      rank=gs_read_csv(sheet) %>% arrange(desc(submmit_time))
      
      # session$reload()
      return(DT::datatable(rank, options = list(paging = FALSE, searching = FALSE,dom = 'ft',ordering=TRUE)))
      
    }else{

      sheet <- gs_title(table)
      rank=gs_read_csv(sheet) %>% arrange(desc(submmit_time))
      
     return(DT::datatable(rank, options = list(paging = FALSE, searching = FALSE,dom = 'ft',ordering=TRUE))) 
    }
  })
  
  output$example_table <- renderPrint({
    cat(
'grup_no,taget
GROUP010312,0.1
GROUP005390,0.26
GROUP009253,0.95
GROUP003473,0.61
GROUP010306,0.42')
  })
  
}