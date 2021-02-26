#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
GenDataReg <- function(n,ratio,xm,xsd,ym,ysd,xmstep,xsdstep,ymstep,ysdstep){
    newDat <- data.frame(n,ratio,xm,xsd,ym,ysd,xmstep,xsdstep,ymstep,ysdstep)
    return(newDat)
}

ui <- fluidPage(

    # Application title
    titlePanel("Residuals tool"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Sample Size",
                        min = 10,
                        max = 1000,
                        value = 30),
            sliderInput("ratio",
                        "Y Signal:Noise",
                        min = .1,
                        max = 10,
                        value = 1),
            fluidRow(
                column(width = 5,
                            sliderInput("xm",
                                        "Initial X Mean",
                                        min = 1,
                                        max = 100,
                                        value = 10)
                ),
                column(width=5,
                     sliderInput("xsd",
                                 "Initial X SD",
                                 min = 1,
                                 max = 100,
                                 value = 10)
            )),
            fluidRow(
                column(width = 5,
                       sliderInput("ym",
                                   "Initial Y Mean",
                                   min = 1,
                                   max = 100,
                                   value = 10)
                ),
                column(width=5,
                       sliderInput("ysd",
                                   "Initial Y SD",
                                   min = 1,
                                   max = 100,
                                   value = 10)
                )),
            fluidRow(
                column(width = 5,
                       sliderInput("xmstep",
                                   "X mean increment",
                                   min = 0,
                                   max = 100,
                                   value = 0)
                ),
                column(width=5,
                       sliderInput("xsdstep",
                                   "X SD increment",
                                   min = 0,
                                   max = 100,
                                   value = 0)
                )),
            fluidRow(
                column(width = 5,
                       sliderInput("ymstep",
                                   "Y Mean increment",
                                   min = 0,
                                   max = 100,
                                   value = 0)
                ),
                column(width=5,
                       sliderInput("ysdstep",
                                   "Y SD increment",
                                   min = 0,
                                   max = 100,
                                   value = 0)
                ))
            
            
        ),


        mainPanel(
            tabsetPanel(
                tabPanel("Model Summary", DT::dataTableOutput("summary"), htmlOutput("r2")),
                tabPanel("Data Plots", plotOutput("mainPlot")),
                tabPanel("Diagnostic Plots", plotOutput("distPlot")),
                tabPanel("Residual Plots", plotOutput("residPlot")))
            

        )
    )
)


server <- function(input, output) {
    myDat <- reactive({
        GenDataReg(input$n,input$ratio,input$xm,input$xsd,input$ym,input$ysd,input$xmstep,input$xsdstep,input$ymstep,input$ysdstep)
    })
    MyReg <- reactive({
        lm(y~x, data=myXY())
    })
    myXY <- reactive({
        newdat<- myDat()
        x<-c(
            rnorm((newdat$n)/10,newdat$xm,newdat$xsd),
            rnorm((newdat$n)/10,newdat$xm+(1*newdat$xmstep),newdat$xsd+(1*newdat$xsdstep)),
            rnorm((newdat$n)/10,newdat$xm+(2*newdat$xmstep),newdat$xsd+(2*newdat$xsdstep)),
            rnorm((newdat$n)/10,newdat$xm+(3*newdat$xmstep),newdat$xsd+(3*newdat$xsdstep)),
            rnorm((newdat$n)/10,newdat$xm+(4*newdat$xmstep),newdat$xsd+(4*newdat$xsdstep)),
            rnorm((newdat$n)/10,newdat$xm+(5*newdat$xmstep),newdat$xsd+(5*newdat$xsdstep)),
            rnorm((newdat$n)/10,newdat$xm+(6*newdat$xmstep),newdat$xsd+(6*newdat$xsdstep)),
            rnorm((newdat$n)/10,newdat$xm+(7*newdat$xmstep),newdat$xsd+(7*newdat$xsdstep)),
            rnorm((newdat$n)/10,newdat$xm+(8*newdat$xmstep),newdat$xsd+(8*newdat$xsdstep)),
            rnorm((newdat$n)/10,newdat$xm+(9*newdat$xmstep),newdat$xsd+(9*newdat$xsdstep))
        )
        y<-(newdat$ratio*x) + c(
            rnorm((newdat$n)/10,newdat$ym,newdat$ysd),
            rnorm((newdat$n)/10,newdat$ym+(1*newdat$ymstep),newdat$ysd+(1*newdat$ysdstep)),
            rnorm((newdat$n)/10,newdat$ym+(2*newdat$ymstep),newdat$ysd+(2*newdat$ysdstep)),
            rnorm((newdat$n)/10,newdat$ym+(3*newdat$ymstep),newdat$ysd+(3*newdat$ysdstep)),
            rnorm((newdat$n)/10,newdat$ym+(4*newdat$ymstep),newdat$ysd+(4*newdat$ysdstep)),
            rnorm((newdat$n)/10,newdat$ym+(5*newdat$ymstep),newdat$ysd+(5*newdat$ysdstep)),
            rnorm((newdat$n)/10,newdat$ym+(6*newdat$ymstep),newdat$ysd+(6*newdat$ysdstep)),
            rnorm((newdat$n)/10,newdat$ym+(7*newdat$ymstep),newdat$ysd+(7*newdat$ysdstep)),
            rnorm((newdat$n)/10,newdat$ym+(8*newdat$ymstep),newdat$ysd+(8*newdat$ysdstep)),
            rnorm((newdat$n)/10,newdat$ym+(9*newdat$ymstep),newdat$xsd+(9*newdat$ysdstep))
        )
        fin <- data.frame(x,y)
        return(fin)
    })
    output$distPlot <- renderPlot({
        xyData <- myXY()
        
        par(mfrow=c(2,2))
        plot(lm(xyData$y~xyData$x))
    })
    output$mainPlot <- renderPlot({
        xyData <- myXY()
        
        par(mfrow=c(1,3))
        plot(xyData$x)
        abline(h=0,lty=2)
        abline(v=0,lty=2)
        plot(xyData$y)
        abline(h=0,lty=2)
        abline(v=0,lty=2)
        plot(xyData$y~xyData$x)
        abline(h=0,lty=2)
        abline(v=0,lty=2)
        abline(lm(xyData$y~xyData$x),col="blue",lwd=2)
        
    })
    output$residPlot <- renderPlot({
        xyData <- myXY()
        mylm <- MyReg()
        par(mfrow=c(1,3))
        hist(residuals(mylm))
        abline(v=mean(residuals(mylm)),col="BLUE",lwd=2)
        plot(residuals(mylm)~xyData$x)
        abline(lm(residuals(mylm)~xyData$x),col="blue",lwd=2)
        abline(h=0,lty=2)
        abline(v=0,lty=2)
        plot(residuals(mylm)~xyData$y)
        abline(lm(residuals(mylm)~xyData$y),col="blue",lwd=2)
        abline(h=0,lty=2)
        abline(v=0,lty=2)
    })
    output$varText <- renderText({
        newdat<- myXY()
        paste("X variance",round(var(newdat$x),2),
              "<br>","Y Variance",round(var(newdat$y),2),
              "<br>","X/Y Variance",round(var(newdat$x)/var(newdat$y),2))
    })
    output$summary <- DT::renderDataTable({
      round(as.data.frame(summary(MyReg())$coefficients),3)

    }, options = list(dom = 't'))
    output$r2 <- renderText({
      paste("R-squared:",round(as.data.frame(summary(MyReg())$r.squared),3))

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
