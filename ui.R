
fluidPage(
  titlePanel("Data Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      p('submit data只能包含grup_no, target兩個欄位, 以逗號分格資料。'),
      helpText('上傳範例：'),
      verbatimTextOutput('example_table')
    ),
    mainPanel(
      h2(textOutput('check_data')),
      tags$hr(),
      DT::dataTableOutput('contents')
      
    )
  )
)
