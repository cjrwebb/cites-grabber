#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This application only works locally, it cannot be deployed as a shiny app due to
# ToS restrictions. Intended for personal use (e.g. CVs) only.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(patchwork)
library(scholar)

# Define UI for citation grabber
ui <- fluidPage(

    # Application title
    titlePanel("Google Scholar Citation Grabber"),

    # Sidebar with ID, submission button, and instructions
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "sch_id",
                      "Google Scholar ID",
                      value = "CNYHo0IAAAAJ"),
            submitButton("Grab Citations", icon("refresh")),
            HTML("<br>Enter your Google Scholar ID and press 'Grab Citations' to generate a table of your publications with citation numbers and a graph of cumulative and yearly citations.
                 <br><br>Your Google Scholar ID is the twelve character number between ?user= and &hl on your Google Scholar citations page.
                 <br><br>For example, my google scholar url is https://scholar.google.com/citations?user=<b>CNYHo0IAAAAJ</b>&hl=en. My 12 character ID is in bold.
                 <br><br>If you are a particularly eminent scholar, it can take a while to grab the citations and create the tables! Be patient! It takes over a minute for Albert Einstein's full bibliography. So if you are Albert Einstein please do not use my app.")
        ),

        # Application output
        mainPanel(
           htmlOutput("sch_name"),
           plotOutput("citesplots"),
           htmlOutput("refs_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    sch_id <- reactive({
        # Reactive object for scholar ID for updating
        sch_id <- as.character(input$sch_id)
    })
    
    
    sch_name <- reactive({
    # Scholar profile name
         sch_name <- scholar::get_profile(input$sch_id)$name
    })
    
    
    pubs_df <- reactive({
        
    # Reactive object for publications data pulled from scholar package
    pubs_df <- scholar::get_publications(sch_id()) %>%
        distinct(title, .keep_all = TRUE) %>%
        transmute(bibtype = "Article", author = as.character(author),
                  title = as.character(title),
                  journaltitle = as.character(journal), year, key = row_number(),
                  number = as.character(number), cites = as.numeric(cites),
                  pubid = as.character(pubid)
        )
        
    })
    
    
    # Reactive object for citation stats data, pulled using scholar package
    cite_stats <- reactive({
        cites_stats <- scholar::get_citation_history(sch_id())
    })
    
    
    # Outputs
    
    output$sch_name <- renderText({
    
    # Return scholar name with easter egg
    ifelse(
    
    sch_name() == "Nathan Hughes",
    yes = paste("<b> Glorious Leader", sch_name(), "</b>"),
    no = paste0("<b>", sch_name(), "</b>")
    
    )
    
    })
    
    
    output$citesplots <- renderPlot({
    
    # Get cite stats
    cite_stats <- cite_stats()
    
    # Create cumulative citations plot
    cumcite_plot <- cite_stats %>%
        ggplot() +
        geom_line(aes(x = year, y = cumsum(cites)), size = 1, col = "#C4EDC1") +
        geom_point(aes(x = year, y = cumsum(cites)), size = 2, col = "#8BDB86") +
        geom_bar(stat = "identity", aes(x = year, y = cites), 
                 size = 2, col = NA, fill = "#C4EDC1", alpha = 0.4) +
        xlab("Year") +
        ylab("Cumulative (line) Citations (bar)") +
        scale_x_continuous(breaks = seq(min(cite_stats$year), max(cite_stats$year), 1)) +
        theme_minimal() +
        ggeasy::easy_all_text_size(size = 7) +
        ggeasy::easy_all_text_color(color = "grey50")

    # Return cumulative citations plot
    cumcite_plot
})
    
    
    # Table of publications
    output$refs_table <- renderText({
        
        # Get pubs_df and cite stats
        pubs_df <- pubs_df()
        
        cite_stats <- cite_stats()
        
    # Tidy pubs_df into references table
    refs_table <- pubs_df %>% select(
        key, author, year, everything()
    ) %>%
        select(
            -bibtype, -pubid, -key
        ) %>%
        mutate(
            journal = paste(journaltitle, number, sep = ". ")
        ) %>%
        select(-journaltitle, -number) %>%
        select(author, year, title, journal, cites) %>%
        arrange(desc(year))
    
    # Create references kable
    refs_table <- knitr::kable(refs_table, "html", booktabs = TRUE,
                 col.names = c("Authors", "Year", "Title", "Journal", "Citations"),
                 longtable = FALSE) %>%
        kableExtra::column_spec(1, width = "5cm") %>%
        kableExtra::column_spec(2, width = "0.75cm") %>%
        kableExtra::column_spec(3, width = "6cm") %>%
        kableExtra::column_spec(4, width = "2.5cm") %>%
        kableExtra::column_spec(5, width = "1cm") %>%
        kableExtra::row_spec(0, bold = TRUE) %>%
        kableExtra::kable_styling(latex_options = c("scale_down", "repeat_header"),
                                  font_size = 10)
    
    # Return references table
    refs_table
    
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
