library(readr)
library(shiny)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(viridis)
library(magick)
library(shinythemes)
library(plotly)

#read data in. adding original notes to help keep things straight

data_std <- read_csv("data/data_std.csv")
data_std <- data_std %>%
  mutate(Benign_Start = as.factor(Benign_Start),
         P53_Start = as.factor(P53_Start),
         Malig_Start = as.factor(Malig_Start))

ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("An Interactive Graphic to Visualize Changes in Various Mutation Rates"),
                h4("Henry Koelling and Andrew Vance"),#we can change this just a filler          
                tabsetPanel(
                  tabPanel("About",
                           h1("p53 Gene Expression in Cancer Evolution: Equilibrium of Expression Rate Dependent on Mutation Rate"),
                           h3("Abstract"),
                           mainPanel(p("One known contributor to increased cancer risk is the level of expression of the p53 gene which produces proteins that assist in proofreading replicated DNA during the cell cycle and can terminate the cell if a mutation is detected. As important as the p53 gene is in stabilizing the body system from acquiring potentially harmful mutations, it requires a significant amount of activation energy. Our study aims to replicate this biological process to gain insight on the relationship between p53 gene expression and energy cost. We have built an adapted simulation from the Empirical Software that emulates this process by creating a world of organisms prone to mutations in the p53 gene. We implemented our simulation by adjusting the rates of malignant and benign mutations, symbolic of cancerous and silent mutations, and the rates of p53 gene expression. After running 3500 simulations with varying starting p53 gene expression and malign and benign mutation rates and recording average p53 gene expression rates in the simulation, we observed that the organisms reached a lower equilibrium of average p53 rate when the amount of mutation rates, malignant or benign, were higher- a novel relationship between the p53 gene in cell behavior. Moving forward, we hope to improve our emulation of cancerous and silent mutations in cell replication to spotlight the relationship more effectively between overall cell mutation rates and p53 gene expression equilibrium.")),
                  ),
                  tabPanel("Mean P53 Gene Expression Rate",
                           h1("Starting Values: Impact on P53 Expression"),
                           sidebarPanel(
                             selectInput("p53",
                                         label = "Choose the p53 value",
                                         choices = c("0.0 P53 Rate", "0.05 P53 Rate", "0.06 P53 Rate", "0.08 P53 Rate", "0.13 P53 Rate", "0.15 P53 Rate")),
                             selectInput("benign",
                                         label = "Choose a Benign Mutation Rate",
                                         choices = c("0.0 Benig Rate", "0.25 Benig Rate", "0.5 Benig Rate", "0.75 Benig Rate", "0.9 Benig Rate")),
                             selectInput("malign",
                                         label = "Choose a Malignant Mutation Rate",
                                         choices = c("0.0 Malig Rate", "0.25 Malig Rate", "0.5 Malig Rate", "0.75 Malig Rate", "0.9 Malig Rate")),
                           ),
                           mainPanel(
                             plotOutput("mainplot"),
                             p("This plot allows the user to independently choose the starting values of each variable and see how they each impact p53 gene expression over a period of time. Note the equilibrium values of various combinations of starting values.", allign = "center")
                           )
                  ),
                  tabPanel("Group by",
                           h1("Comparitive Starting Value Visualization"),
                           varSelectInput("choice",
                                          label = "Select a Testing Variable",
                                          data = data_std[,2:4]),
                           
                           conditionalPanel(
                             condition = "input.choice == 'P53_Start'",
                             selectInput("benig_p53_partition",
                                         label = "Choose a Benign Mutation Rate",
                                         choices = c("0.0 Benig Rate", "0.25 Benig Rate", "0.5 Benig Rate", "0.75 Benig Rate", "0.9 Benig Rate")),
                             selectInput("malig_p53_partition",
                                         label = "Choose a Malignant Mutation Rate",
                                         choices = c("0.0 Malig Rate", "0.25 Malig Rate", "0.5 Malig Rate", "0.75 Malig Rate", "0.9 Malig Rate")),
                           ),
                           conditionalPanel(
                             condition = "input.choice == 'Malig_Start'",
                             selectInput("benig_malig_partition",
                                         label = "Choose a Benign Mutation Rate",
                                         choices = c("0.0 Benig Rate", "0.25 Benig Rate", "0.5 Benig Rate", "0.75 Benig Rate", "0.9 Benig Rate")),
                             selectInput("p53_malig_partition",
                                         label = "Choose the p53 value",
                                         choices = c("0.0 P53 Rate", "0.05 P53 Rate", "0.06 P53 Rate", "0.08 P53 Rate", "0.13 P53 Rate", "0.15 P53 Rate")),
                           ),
                           conditionalPanel(
                             condition = "input.choice == 'Benign_Start'",
                             selectInput("malig_benig_partition",
                                         label = "Choose a Malignant Mutation Rate",
                                         choices = c("0.0 Malig Rate", "0.25 Malig Rate", "0.5 Malig Rate", "0.75 Malig Rate", "0.9 Malig Rate")),
                             selectInput("p53_benig_partition",
                                         label = "Choose the p53 value",
                                         choices = c("0.0 P53 Rate", "0.05 P53 Rate", "0.06 P53 Rate", "0.08 P53 Rate", "0.13 P53 Rate", "0.15 P53 Rate")),
                           ),
                           p("This graph allows the user to view combinations of two starting variables across all possible values of the third. Highlighting portions of the graph with your cursor enables you to zoom in and view more closely."),
                           mainPanel(
                             plotlyOutput("sideplot")
                           ),        
                           
                  ),
                  tabPanel("Paper",
                           a("Link to paper",href="https://carletoncollegemn-my.sharepoint.com/personal/koellingh_carleton_edu/Documents/CS/Evo_Comp_and_Art_Life/final_paper_resources/final_paper.pdf")
                  )
                ))

server <- function(input, output, session) {
  output$text <- renderText({
    paste("sample text")
  })
  
  graph_main_data <- reactive({#filter starting values
    data_std %>%
      filter(Malig_Start == input$malign, Benign_Start == input$benign, P53_Start == input$p53)
  })
  
  graph_side_data <- reactive({
    if (input$choice == "P53_Start"){
      data_std %>%
        filter(Malig_Start == input$malig_p53_partition, Benign_Start == input$benig_p53_partition)
    } else if (input$choice == "Malig_Start"){
      data_std %>%
        filter(Benign_Start == input$benig_malig_partition, P53_Start == input$p53_malig_partition)
    } else {
      data_std %>%
        filter(Malig_Start == input$malig_benig_partition, P53_Start == input$p53_benig_partition)
    }  
  })  
  
  
  
  output$mainplot <- renderPlot({
    ggplot(graph_main_data(), aes(x = Update, y = avg_mean_p53)) + 
      geom_jitter() + 
      geom_smooth(aes(sd = sd_mean_p53)) +
      scale_color_viridis(discrete=TRUE) +
      theme(axis.text.x = element_text(angle=90, hjust=1))
    
  })
  
  output$sideplot <- renderPlotly({
    p <- ggplot(graph_side_data(), aes(x=Update, y=avg_mean_p53, color=!!input$choice)) + 
      geom_jitter() + 
      geom_smooth(aes(sd = sd_mean_p53)) +
      scale_color_viridis(discrete=TRUE) +
      theme(axis.text.x = element_text(angle=90, hjust=1))
    p <- ggplotly(p) %>% layout(autosize = FALSE)
    
    animation_opts(p, frame = 1000, transition = 500, easing = "linear", redraw = TRUE, mode = "afterall")
  })
}

shinyApp(ui = ui, server = server)















