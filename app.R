library(shiny)
library(ggplot2)
library(visreg)

sales = read.csv("sales.csv")
set.seed(2015)
splitSale = caret::createDataPartition(sales[,1], p = 0.8, list=F, times=1)
training = sales[splitSale,]
testing = sales[!row.names(sales) %in% row.names(training),]
testing = sales[-splitSale,]


ui <- fluidPage(
    
    # App title ----
    titlePanel("Advertising Expenses and Sales"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            #Input: variables
            selectInput('variables','Select variables:',names(training)[c(1,2,3)],
                        selected=names(training)[[1]],),
            
            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 5),
            h3('Learn from data:'),
            p('We can use advertising expenses to predict sales. We have three x variables, 
              YouTube, Facebook and newspaper. The model of YouTube variable  is more accurate to predict sales.
              The correlation of YouTube and sales is 0.78 and indicates strong positive correlation. 
              To evaluate the model performance, the model of YouTube variable has the best performance. 
              The RMSEP value is relative low (3.3) and R2 is 0.7, the most close to 1 compared with other two. 
              Facebook variable model has medium performance with 5.55 RMSEP and 0.17 R2. 
              Newspaper variable model has the worst performance with 5.870668 RMSEP and 0.08 R2. 
              Therefore, predict sales by YouTube advertising expense is more accurate. ' ),
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            h5("The dataset show the interaction between advertising expenses of diffrent sourses 
            and sales (in thousand $)." ),
            h5( "There are total 171 observations in the dataset.The dataset is related to advertising expenses on different medias effect on sales. 
                Applied 80% to contribute on regresssion model and rest for testing"),
            h5("Source: kaggle Marketing_Data.csv"),
            h5("A linear regression model and model test are calculated for each type of advertising and shown below.You can swith x varialbe and check the data"),
            
            h4("Data Summary"),
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            
            # Output: HTML table with requested number of observations ----
            h4("Data review"),
            tableOutput("view"),
            h4("Regression Model"),
            plotOutput("plot1"),
            plotOutput("plot2"),
            
            h4("Correlation"),
            textOutput("cor1"),
            textOutput("cor2"),
            textOutput("cor3"),
            
            h4("Linear regression model fit"),
            verbatimTextOutput("summary1"),
            verbatimTextOutput("summary2"),
            verbatimTextOutput("summary3"),
            
            
            h4("Test Model: Predicted versus Reference"),
            plotOutput("plot3"),
            
            
            h4("PRESS"),
            textOutput("press1"),
            textOutput("press2"),
            textOutput("press3"),
            
            h4("RMSEP"),
            textOutput("rmsep1"),
            textOutput("rmsep2"),
            textOutput("rmsep3"),
            
            h4("SST"),
            textOutput("sst1"),
            textOutput("sst2"),
            textOutput("sst3"),
            
            h4("R2"),
            textOutput("r21"),
            textOutput("r22"),
            textOutput("r23"),
            
            h4("Test Summary"),
            verbatimTextOutput("p1"),
            verbatimTextOutput("p2"),
            verbatimTextOutput("p3")
            
        )
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    selectVariable <- reactive({
        training[,c(input$variables,"sales")]
        
    })
    
    
    
    model1 <- lm(sales ~ youtube, data = training)
    model2 <- lm(sales ~ facebook, data = training)
    model3 <- lm(sales ~ newspaper, data = training)
    
    pred1 = data.frame(predict(model1, newdata=testing))
    names(pred1)[1] = 'Predicted'
    pred1$Reference = testing[,c('sales')]
    pred1$lwr = predict(model1, newdata=testing, interval = "confidence")[,2]
    pred1$upr = predict(model1, newdata=testing, interval = "confidence")[,3]
    
    
    pred2 = data.frame(predict(model2, newdata=testing))
    names(pred2)[1] = 'Predicted'
    pred2$Reference = testing[,c('sales')]
    pred2$lwr = predict(model2, newdata=testing, interval = "confidence")[,2]
    pred2$upr = predict(model2, newdata=testing, interval = "confidence")[,3]
    
    pred3 = data.frame(predict(model3, newdata=testing))
    names(pred3)[1] = 'Predicted'
    pred3$Reference = testing[,c('sales')]
    pred3$lwr = predict(model3, newdata=testing, interval = "confidence")[,2]
    pred3$upr = predict(model3, newdata=testing, interval = "confidence")[,3]
    
    test1 = qplot(Reference, Predicted, data=pred1) + geom_point(colour = "#006600", size = 3) + 
        geom_errorbar(aes(ymin = lwr,ymax = upr))
    test2 = qplot(Reference, Predicted, data=pred2) + geom_point(colour = "#006600", size = 3) + 
        geom_errorbar(aes(ymin = lwr,ymax = upr))
    test3 = qplot(Reference, Predicted, data=pred3) + geom_point(colour = "#006600", size = 3) + 
        geom_errorbar(aes(ymin = lwr,ymax = upr))
    
    output$summary <- renderPrint({
        dataset <- sales
        summary(sales)
    })
    
    
    output$view <- renderTable({
        head(sales, n = input$obs)
    })
    
    output$plot1 <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectVariable())
        if(input$variables=='youtube'){abline(model1, col = "blue", lwd = 2)}
        if(input$variables=='facebook'){abline(model2, col = "green", lwd = 2)}
        if(input$variables=='newspaper'){abline(model3, col = "red", lwd = 2)}
    })
    
    output$plot2 <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))
        if(input$variables=='youtube')plot({visreg::visreg(model1)})
        if(input$variables=='facebook')plot({visreg::visreg(model2)})
        if(input$variables=='newspaper')plot({visreg::visreg(model3)})  
        
    })    
    
    output$cor1 <-renderText({if(input$variables=='youtube'){cor(sales$youtube, sales$sales)}})
    output$cor2 <-renderText({if(input$variables=='facebook'){cor(sales$facebook, sales$sales)}})
    output$cor3 <-renderText({if(input$variables=='newspaper'){cor(sales$newspaper, sales$sales)}})
    
    press1 = sum((pred1$Reference - pred1$Predicted)^2)
    press2 = sum((pred2$Reference - pred2$Predicted)^2)
    press3 = sum((pred3$Reference - pred3$Predicted)^2)
    
    sst1 = sum((pred1$Reference - mean(pred1$Reference))^2)
    sst2 = sum((pred2$Reference - mean(pred2$Reference))^2)
    sst3 = sum((pred3$Reference - mean(pred3$Reference))^2)
    
    output$press1 <- renderText({
        if(input$variables=='youtube'){press1}})
    output$press2 <- renderText({
        if(input$variables=='facebook'){press2}})
    output$press3 <- renderText({
        if(input$variables=='newspaper'){press3}})
    
    output$rmsep1 <- renderText({
        if(input$variables=='youtube'){sqrt(press1/ nrow(pred1))}})
    output$rmsep2 <- renderText({
        if(input$variables=='facebook'){sqrt(press2/ nrow(pred2))}})
    output$rmsep3 <- renderText({
        if(input$variables=='newspaper'){sqrt(press3/ nrow(pred3))}})
    
    output$sst1 <- renderText({
        if(input$variables=='youtube'){sst1}})
    output$sst2 <- renderText({
        if(input$variables=='facebook'){sst2}})
    output$sst3 <- renderText({
        if(input$variables=='newspaper'){sst3}})
    
    output$r21 <- renderText({
        if(input$variables=='youtube'){1 - (press1/sst1)}})
    output$r22 <- renderText({
        if(input$variables=='facebook'){1 - (press2/sst2)}})
    output$r23 <- renderText({
        if(input$variables=='newspaper'){1 - (press3/sst3)}})
    
    output$summary1 <- renderPrint({if(input$variables=='youtube'){summary(model1)}})
    output$summary2 <- renderPrint({if(input$variables=='facebook'){summary(model2)}})
    output$summary3 <- renderPrint({if(input$variables=='newspaper'){summary(model3)}})
    
    
    output$p1 <- renderPrint({if(input$variables=='youtube'){pred1}})
    output$p2 <- renderPrint({if(input$variables=='facebook'){pred2}})
    output$p3 <- renderPrint({if(input$variables=='newspaper'){pred3}})
    
    output$plot3 <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectVariable())
        
        if(input$variables=='youtube')plot({(test1)})
        if(input$variables=='facebook')plot({(test2)})
        if(input$variables=='newspaper')plot({(test3)})      
        
    })    
    
    
}

# Create Shiny app ----
shinyApp(ui, server)

