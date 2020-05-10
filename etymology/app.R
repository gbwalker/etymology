# Shiny app for visualizing etymology.
#

library(tidyverse)
library(shiny)
library(shinyjs)
library(visNetwork)
library(igraph)
library(utf8)

############################
# INITIAL FUNCTIONS AND DATA
############################

# Read in the necessary data.
df <- readRDS("df.rds")
edge_list <- readRDS("edge_list.rds")
nodes_all <- readRDS("nodes_all.rds")
root_list <- readRDS("roots.rds")

# Recursive edge finder.
get_edges <- function(id_list) {
    edges <- edge_list %>% 
        filter(from %in% c(id_list) | to %in% c(id_list))
    return (unique(c(unique(edges$from), unique(edges$to))))
}

unpack_roots <- function(root_row) {
    return (unlist(str_split(root_row, ",")))
}

# Escape all characters in strings for regex.
quotemeta <- function(string) {
    str_replace_all(string, "(\\W)", "\\\\\\1")
}

################
# USER INTERFACE
################

sidebar <- sidebarPanel(
    useShinyjs(),
    width = 2,
    column(12,
           tags$div(title="The graph will generate automatically when you enter an existing word.",
                    textInput(
                   "searched_word", 
                   h2("Search"),
                   placeholder = "Search for a word"
                    )
           ),
           hidden(textInput(
               "target_word",
               "hidden target word"
           )),
           actionButton(
               "random_word",
               label = "Random word",
           ),
           h2("Graph Settings"),
           tags$div(title="Choose how many levels of connections to show. WARNING: 2 may take a while to render.",
               radioButtons("tree_depth",
                            h5("Tree depth"),
                            choices = c(0:2),
                            selected = 0,
                            inline = TRUE)
               ),
           tags$div(title="Toggle this to explore the current graph. Otherwise, clicking a node will redraw it.",
               checkboxInput("locked",
                             h5("Lock graph"),
                             value = FALSE)
               ),
           tags$div(title="Display the graph with the most connected terms at the top.",
               checkboxInput("hierarchical",
                             h5("Hierarchical"),
                             value = FALSE)
               ),
           tags$div(title="Select which word roots to highlight. NOTE: This will redraw the graph.",
               selectInput("root",
                           h5("Highlight roots"),
                           choices = c("None", root_list$language),
                           select = "None",
                           selectize = TRUE)
           )
    ),
    helpText(HTML('&emsp;<em>All data from <a href="https://www.etymonline.com/" target="_blank">Online Etymology Dictionary</a>.</em>')),
    hr(),
    h2(htmlOutput("word")),
    htmlOutput("definition")
)

    
body <- mainPanel(
    fluidPage(
        visNetworkOutput("proxy_graph", width = "100%", height = "1000px")
    )
)

ui <- navbarPage(
    "Etymology Explorer",
    position = "static-top",
    tabPanel(
        "Graph",
        sidebarLayout(
            position = "left",
            sidebar,
            body
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ###########################
    # Create the network graph.
    ###########################
    output$proxy_graph <- renderVisNetwork({
        
        # If there is no input, print an empty graph.
        if (is.na(input$searched_word) | 
            input$searched_word == "" |
            (!(input$searched_word %in% nodes_all$label))) {
            NULL
        } else {
        
            # Set the searched word.
            searched_word <- input$searched_word
            
            # Filter to find the word of interest.
            row <- df %>% 
                filter(word == searched_word)
            def <- row %>% 
                select(def) %>% 
                first()
            date <- row %>% 
                select(date) %>% 
                first()
            
            # Recursively get links.
            id_list <- nodes_all$id[match(searched_word, nodes_all$label)]
            if (input$tree_depth > 0) {
                for (n in 1:input$tree_depth) {
                    id_list <- get_edges(id_list)
                }
            }
            
            # Get all the edges that contain the word.
            edges <- edge_list %>% 
                filter(from %in% c(id_list) | to %in% c(id_list)) %>% 
                mutate(color = "lightblue") %>% 
                select(-physics)
            
            if (nrow(edges) > 0) {
                # Get a list of all the adjacent words.
                adjacent_words <- c(unique(edges$from), unique(edges$to))
                
                # Make a list of all nodes.
                nodes <- nodes_all %>% 
                    filter(id %in% adjacent_words) %>% 
                    mutate(label = as_utf8(str_replace_all(label, "'", "\u2019")))
            } else {
                nodes <- nodes_all %>% 
                    filter(label == searched_word) %>% 
                    mutate(label = as_utf8(str_replace_all(label, "'", "\u2019")))
            }
            
            # Print no edges if they don't exist.
            if (nrow(edges) == 0) {
                from <- id_list
                to <- id_list
                edges <- tibble(to, from) %>% 
                    slice(0)
            }
            
            # Add a color if applicable.
            if (input$root %in% root_list$language) {
                nodes <- nodes %>% 
                    mutate(color = case_when(
                      str_detect(roots, input$root) ~ "gold",
                      label == searched_word ~ "lightgreen",
                      str_detect(roots, input$root) & label == searched_word ~ "lightgreen",
                      TRUE ~ "lightblue"
                        )
                    )
            } else {
                nodes <- nodes %>% 
                    mutate(color = case_when(
                        label == searched_word ~ "lightgreen",
                        TRUE ~ "lightblue")
                        )
            }
            
            # Print the graph and save it globally.
            visNetwork(nodes, edges) %>% 
                visLayout(hierarchical = input$hierarchical) %>% 
                visIgraphLayout(layout = "layout.davidson.harel", randomSeed = 1, physics = TRUE) %>% 
                visInteraction(dragNodes = FALSE, hover = TRUE) %>% 
                visEvents(
                    selectNode = "function(nodes) 
                        {Shiny.onInputChange('current_node_id', nodes);
                ;}")
        }
    })
    
    changeRoots <- reactive({
        list(input$searched_word, input$tree_depth)
    })
    
    # Display the option to only select languages of nodes on the graph.
    # This duplicates the process of creating a graph.
    observeEvent(changeRoots(), {
        
        # If there is no input, print an empty graph.
        if (is.na(input$searched_word) | input$searched_word == "" | (!(input$searched_word %in% nodes_all$label))) {
            NULL
        } else {
            
            searched_word <- input$searched_word
            
            # Filter to find the word of interest.
            row <- df %>% 
                filter(word == searched_word)
            
            # Recursively get links.
            id_list <- nodes_all$id[match(searched_word, nodes_all$label)]
            if (input$tree_depth > 0) {
                for (n in 1:input$tree_depth) {
                    id_list <- get_edges(id_list)
                }
            }
            
            # Get all the edges that contain the word.
            edges <- edge_list %>% 
                filter(from %in% c(id_list) | to %in% c(id_list))
            
            if (nrow(edges) > 0) {
                # Get a list of all the adjacent words.
                adjacent_words <- c(unique(edges$from), unique(edges$to))
                
                # Make a list of all nodes.
                nodes <- nodes_all %>% 
                    filter(id %in% adjacent_words) %>% 
                    mutate(label = as_utf8(str_replace_all(label, "'", "\u2019")))
            } else {
                nodes <- nodes_all %>% 
                    filter(label == searched_word) %>% 
                    mutate(label = as_utf8(str_replace_all(label, "'", "\u2019")))
            }
            
            # Get only the unique values in the roots list.
            root_list <- unique(unlist(lapply(c(nodes$roots[1:10]), unpack_roots)))
            root_list <- c("None", sort(root_list[root_list != "" & !is.na(root_list)]))
            
            # Use the currently selected root choice as a default. If it doesn't exist, choose none.
            if (input$root %in% root_list) {
                choice <- input$root
            } else {
                choice <- "None"
            }
            
            updateSelectInput(session,
                              "root", 
                              select = choice,
                              choices = root_list
            )
        }
    })
    
    ###########################################
    # Watch for changes in the graph selection.
    ###########################################
    observe({
        visNetworkProxy("proxy_graph")
    })
    
    # Capture URL input.
    observe({
        query <- parseQueryString(session$clientData$url_search)
        if (!is.null(query[["word"]])) {
            updateTextInput(session, 
                            "searched_word",
                            value = query[["word"]])
        }
    })
    
    # Print the date.
    output$word <- renderText({
        if (is.na(input$target_word) | 
            input$target_word == "" |
            (!(input$target_word %in% nodes_all$label))) {
            NULL
        } else {
            date <- df %>% 
                filter(word == input$target_word) %>% 
                select(date) %>% 
                first()
            
            if (!is.na(date)) {
                
                # Add a "circa" if the date is approximate.
                date <- ifelse(substr(date, 3, 4) %in% c("00", "25", "50", "75"), paste0("c. ", date), paste0(date))
                
                paste0(input$target_word, " (", date, ")")
                
            } else {
                paste0(input$target_word)
            }
        }
    })
    
    # Print the definition.
    output$definition <- renderText({
        if (is.na(input$target_word) | 
            input$target_word == "" |
            (!(input$target_word %in% nodes_all$label))) {
            NULL
        } else {
            df_word <- df %>% 
                filter(word == input$target_word) %>% 
                select(def, italics)
            
            # Replace longest list of italics first.
            italics <- unique(str_trim(unlist(str_split(df_word$italics, ","))))
            def <- df_word$def
            
            # Replace italics for HTML display.
            for (italic in italics) {
                def <- str_replace(def, paste0('([\\s\\(\\"])(', quotemeta(italic), ")", '([\\s\\"\\,\\.\\;\\)\\:])'), paste0("\\1<em>\\2</em>\\3"))
            }
            
            # Add in line breaks.
            def <- str_replace_all(def, "\\.\\s([A-Z])", ".<br /><br />\\1")
            
            # Replace some inconsistencies manually, including line breaks.
            def <- str_replace_all(def, ";;", "<br /><br />")
            
            no_breaks <- quotemeta(c("c.", "Col.", "ch.", "Ch.", paste0(LETTERS, ".")))
            
            for (entry in no_breaks) {
                def <- str_replace_all(def, paste0(entry, "<br /><br />"), paste0(entry, " "))
            }
            
            # Print the result.
            paste0(def)
        }
    })
    
    # Input a random word.
    observeEvent(input$random_word, {
        rm(.Random.seed, envir=globalenv())
        
        new_word <- sample_n(df, 1) %>% 
            select(word) %>% 
            first()
        
        updateTextInput(session, 
                        "searched_word",
                        value = new_word)
    })
    
    # Change the input value word when the user selects a node.
    observeEvent(input$current_node_id, {
        new_word <- nodes_all %>% 
            filter(unlist(input$current_node_id[1]) == id) %>% 
            select(label) %>% 
            first()
        
        # If the graph is locked, only update the target word (definition, date, etc.).
        if (!input$locked) {
            updateTextInput(session, 
                            "searched_word",
                            value = new_word)
            updateTextInput(session, 
                            "target_word",
                            value = new_word)
        } else {
            updateTextInput(session, 
                            "target_word",
                            value = new_word)
        }
    })
    
    # Change the target word if the searched word changes.
    observeEvent(input$searched_word, {
        updateTextInput(session,
                        "target_word",
                        value=input$searched_word)
    })
    
    # Redraw the graph if the user unselects "locked."
    observeEvent(input$locked, {
        if (!input$locked & input$searched_word != input$target_word) {
            updateTextInput(session,
                            "searched_word",
                            value=input$target_word)
        }
    })
}

# Run the application.
shinyApp(ui = ui, server = server)
