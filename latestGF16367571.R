
library(shiny)


ui <- fluidPage(
  titlePanel("Assignment 4"),
  
  #1 Inputs
  sidebarLayout(
    sidebarPanel(
      
      #Read in Csv
      fileInput("file1","Choose CSV file:", accept=c("text/csv","text/comma-separated-values,text/plain",".csv")),
      
      #Genes to display
      numericInput("genes","Number of genes to display:", value = 10),
      
      #P value
      sliderInput("adjp","Adj. P-Value:", min = 0.0001, max = .1, value = .050),
      
      #Log2FC
      sliderInput("log2","Log2FC:", min = 0, max = 8, value = 4),
      
      #Radio buttons to change abline colour (my feature)
      radioButtons("colours", "Choose abline colour:", choices = c("Red" = 'red', "Blue" = "blue", "Yellow"="yellow", "Hot pink"="hotpink"), selected = 'blue')
    ),
      
    #2 outputs
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("volcano")),
                  tabPanel("Table", tableOutput("gtable")
     # tableOutput("gtable"),
      #plotOutput("volcano")
    )
    )
  )
  )
)
      

#3 SERVER: set up our outputs with our inputs
server <- function(input, output, session) {

  #Set up reactive function for data loaded
     LoadData <- reactive({
       data <- input$file1
       if (is.null(data))
         return(NULL)
       read.csv(data$datapath, header=T,sep="\t")
     })
     
     #Table: make table based on rows selected from gene input slider
     output$gtable <-renderTable(head(LoadData(), input$genes))
    
     

   #Volcanoplot
   output$volcano <- renderPlot({
     #assign variable genes to hold reactive function data from csv
     genes <- LoadData()
     
     if (is.null(genes))
       return(NULL)
     
     #assign signifcance variable using sliders for adjusted p value and log2
     sig <- genes[(genes$adj.P.Val <= input$adjp & abs(genes$logFC)>= input$log2), ] 
     
     #Plot title that will change based on slider input
     Title <- c(paste("Volcano Plot:", "Adjusted P-value = ", as.character(input$adjp), "/Log2 Fold Change =" , as.character(input$log2)))
     
     #Plot the volcano plot using genes variable and log10 of the adjusted p value
     plot(genes$logFC,-log10(genes$adj.P.Val), pch="*",
          #Labels for axises and title
     xlab="Log2 Fold Change", ylab="-10log (adjusted p-value)", main = Title)
          #The signifcance cut off lines based on our inputs
     abline(h=-log10(input$adjp), v=c(-input$log2,input$log2), col= input$colours, lty=2)
     points(sig$logFC, -log10(sig$adj.P.Val), col="blue", pch="*")
   })
   
   
}




shinyApp(ui = ui, server = server)

