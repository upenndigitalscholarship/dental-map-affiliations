library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(igraph)
library(readxl)
library(geosphere)
library(ggplot2)
library(maps)
library(countrycode)
library(leaflet.extras)
library(tidygeocoder)
library(ggmap)

#df1 <- read.csv("Authors.csv", check.names = FALSE)
#df2 <- read.csv("Articles.csv", check.names = FALSE)
#df <- merge(df1, df2, by.x = "Study_ID", by.y = "ID")
#write.csv(df, "Merge.csv", row.names = FALSE)
df <- read.csv("merge.csv", check.names = FALSE)


#### Using Google API key to obtain the latitude and longitude data
#Register for a Google Maps API key:
#Go to the Google Cloud Console: https://console.cloud.google.com/
#Create or select a project (if you don't have one).
#Enable the Geocoding API for your project.
#Create an API key: Go to the Credentials page, click "Create Credentials," and select "API key."
#library(ggmap)
#register_google(key = "your_google_maps_api_key")
#locations <- geocode(paste(df$Affiliation, df$City, df$Country, sep = ", "))
#df <- cbind(df, locations)

# Using tidygeocoder to obtain the latitude and longitude data
#df <- df %>% 
#  tidygeocoder::geocode(
#    street = Affiliation,
#    city = City,
#    country = Country,
#    method = 'osm')

df_cat <- read_excel("merge.xlsx",skip = 1, col_names = TRUE)
#condition_frequency <- table(df_cat$`Primary condition`)
#sorted_frequency <- sort(condition_frequency, decreasing = TRUE)
#print(sorted_frequency)
#Count the number of occurrences where Primary condition has more than one unique value
count <- df_cat %>%
  group_by(TI) %>%
  filter(n_distinct(`Primary condition`) > 1)

df_cat <- df_cat %>%
  anti_join(count, by = "TI")


df <- df %>%
  left_join(df_cat %>% select(TI, `Primary condition`), by = "TI") %>% 
  distinct()


# Check for missing values in latitude and longitude columns
missing_lat <- is.na(df$lat)
missing_long <- is.na(df$long)


# Remove rows with missing coordinates
df <- df[!missing_lat & !missing_long, ]

# Convert countries to continents using countrycode
df$Continent <- countrycode(
  sourcevar = df$Country,
  origin = "country.name",
  destination = "continent",
  warn = TRUE
)

missing_cont <- is.na(df$Continent)
df <- df[!missing_cont,]

missing_cont <- is.na(df$Country)
df <- df[!missing_cont,]

df <- df[complete.cases(df[, c("TI", "Primary condition", "Country", "Continent")]), ]

catagory <- sort(unique(df$`Primary condition`))

# Define rename rules
rename_rules <- c("Antrhopometrics" = "Anthropometrics",
                  "Craniofacial anomalies" = "Craniofacial Anomalies",
                  "Dental anomalies" = "Dental Anomalies",
                  "Dental caries" = "Dental Caries",
                  "Dental trauma" = "Dental Trauma",
                  "Endodontics Conditions" = "Endodontic Conditions",
                  "Facial trauma" = "Facial Trauma",
                  "Maloclussion" = "Malocclusion",
                  "Maxillofacial trauma" = "Maxillofacial Trauma",
                  "Oral health care" = "Oral Health Care",
                  "Oral health Care" = "Oral Health Care",
                  "Oral health education" = "Oral Health Education",
                  "Oral health status" = "Oral Health Status",
                  "Oral health Status" = "Oral Health Status",
                  "Oral Health status" = "Oral Health Status",
                  "Oral lesions" = "Oral Lesions",
                  "oral lesions and ulcers" = "Oral Lesions and Ulcers",
                  "Periodonatal Diseases" = "Periodontal Diseases",
                  "Periodontal Disease" = "Periodontal Diseases",
                  "Periodontal diseases" = "Periodontal Diseases",
                  "TMJ Disorders" = "Temporomandibular Joint Disorders",
                  "Tempomandibular Joint disorders" = "Temporomandibular Joint Disorders",
                  "Tempomandibular Joint Disorders" = "Temporomandibular Joint Disorders")

# Apply renames

df$`Primary condition` <- ifelse(df$`Primary condition` %in% names(rename_rules), rename_rules[df$`Primary condition`], df$`Primary condition`)

catagory <- sort(unique(df$`Primary condition`))

ui <- function(req) {
  dashboardPage(
    dashboardHeader(title = "Collaboration Network"),
    dashboardSidebar(
      sidebarMenu(
        id = "Bar",
        menuItem("Country", tabName = "country", icon = icon("flag"), selected = TRUE, startExpanded = TRUE),
        menuItem("Continent", tabName = "continent", icon = icon("globe"), startExpanded = FALSE)
      )
    ),
    dashboardBody(
      tabItems(
        # Tab 1: Select a country
        tabItem(
          tabName = "country",
          fluidRow(
            box(
              title = "Select Primary Condition",
              selectInput("selected_primary_condition", "Primary Condition", choices = sort(unique(df$`Primary condition`)), selected = unique(df$`Primary condition`)[1])
            )
          ),
          fluidRow(
            box(
              title = "Select Country",
              selectInput("selected_country", "Country", choices = sort(unique(df$Country)), selected = "USA")
            )
          ),
          fluidRow(
            box(
              title = "Select Network Level",
              selectInput("network_level_country", "Network Level", choices = c("Country-level", "Continental-level", "Global-level"), selected = "Country-level")
            )
          ),
          conditionalPanel(
            condition = "input.network_level_country == 'Country-level'",
            fluidRow(
              box(
                title = "Select Country",
                selectInput("selected_element_country", "Country", choices = sort(unique(df$Country)), selected = "USA")
              )
            )
          ),
          conditionalPanel(
            condition = "input.network_level_country == 'Continental-level'",
            fluidRow(
              box(
                title = "Select Continent",
                selectInput("selected_element_continental", "Continent", choices = sort(unique(df$Continent)), selected = "North America")
              )
            )
          ),
          fluidRow(
            box(
              title = "Collaboration Map",
              leafletOutput("collab_map_country"),
              width = 18, height = "600px"
            )
          )
        ),
        # Tab 2: Select a continent
        tabItem(
          tabName = "continent",
          fluidRow(
            box(
              title = "Select Primary Condition",
              selectInput("selected_primary_condition_continent", "Primary Condition", choices = sort(unique(df$`Primary condition`)), selected = unique(df$`Primary condition`)[1])
            )
          ),
          fluidRow(
            box(
              title = "Select Continent",
              selectInput("selected_continent", "Continent", choices = sort(unique(df$Continent)), selected = "North America")
            )
          ),
          fluidRow(
            box(
              title = "Select Network Level",
              selectInput("network_level_continent", "Network Level", choices = c("Continental-level", "Global-level"), selected = "Continental-level")
            )
          ),
          conditionalPanel(
            condition = "input.network_level_continent == 'Continental-level'",
            fluidRow(
              box(
                title = "Select Continent",
                selectInput("selected_element_continent", "Continent", choices = sort(unique(df$Continent)), selected = "North America")
              )
            )
          ),
          fluidRow(
            box(
              title = "Collaboration Map",
              leafletOutput("collab_map_continent"),
              width = 18, height = "600px"
            )
          )
        )
      )
    )
  )
}


server <- function(input, output, session) {
  
  observe({

    if ((input$Bar == "country")) {
      primary_condition <- input$selected_primary_condition 
      filtered_df <- df[df$`Primary condition` == primary_condition, ]
      non_na_counts <- rowSums(!is.na(filtered_df[, -1]))
      filtered_df <- filtered_df[non_na_counts > 0, ]
      updateSelectInput(session, "selected_country", choices = sort(unique(filtered_df$Country)), selected = "Nigeria")
      updateSelectInput(session, "selected_element_country", choices = sort(unique(filtered_df$Country)), selected = "Nigeria")
      updateSelectInput(session, "selected_element_continental", choices = sort(unique(filtered_df$Continent)), selected = "Africa")
    }
  })
  
  observe({
    if ((input$Bar == "continent")) {
      primary_condition <- input$selected_primary_condition_continent
      filtered_df <- df[df$`Primary condition` == primary_condition, ]
      non_na_counts <- rowSums(!is.na(filtered_df[, -1]))
      filtered_df <- filtered_df[non_na_counts > 0, ]
      updateSelectInput(session, "selected_continent", choices = sort(unique(filtered_df$Continent)), selected = "Africa")
      updateSelectInput(session, "selected_element_continent", choices = sort(unique(filtered_df$Continent)), selected = unique(filtered_df$Continent)[1])
    }
  })
  
  # Filter data based on user selections
  filtered_df <- reactive({
    if ((input$Bar == "country")) {
      primary_condition <- input$selected_primary_condition }
    if ((input$Bar == "continent")) {
      primary_condition <- input$selected_primary_condition_continent }
    
    filtered_df <- df[df$`Primary condition` == primary_condition, ]
    non_na_counts <- rowSums(!is.na(filtered_df[, -1]))
    filtered_df <- filtered_df[non_na_counts > 0, ]
  
    if ((!is.null(input$selected_country)) & (input$Bar == "country")) {
      if (input$network_level_country == "Country-level") {
        selected_studies <- unique(filtered_df$Study_ID[filtered_df$Country == input$selected_country])
        filtered_df <- filtered_df[filtered_df$Study_ID %in% selected_studies, ]
        if ((!is.null(input$selected_element_country)) & (input$selected_element_country != input$selected_country) ){
          filtered_df <- filtered_df[filtered_df$Country %in% c(input$selected_element_country, input$selected_country), ]
          selected_studies <- unique(filtered_df$Study_ID[filtered_df$Country == input$selected_element_country])
          filtered_df <- filtered_df[filtered_df$Study_ID %in% selected_studies, ]
        }
        else if ((!is.null(input$selected_element_country)) & (input$selected_element_country == input$selected_country)) {
          filtered_df <- filtered_df %>%
            group_by(Study_ID) %>%
            filter(all(Country == input$selected_country)) %>%
            ungroup()
          
        }
      } else if (input$network_level_country == "Continental-level") {
        selected_studies <- unique(filtered_df$Study_ID[filtered_df$Country == input$selected_country])
        filtered_df <- filtered_df[filtered_df$Study_ID %in% selected_studies, ]
        Cont <- countrycode(
          sourcevar = input$selected_country,
          origin = "country.name",
          destination = "continent", 
          warn = TRUE
        )
        if ((!is.null(input$selected_element_continental)) & (input$selected_element_continental == Cont)){
          
          filtered_df <- filtered_df[filtered_df$Continent %in% c(input$selected_element_continental, Cont), ]
          studies_with_non_selected <- filtered_df$Study_ID[!filtered_df$Country %in% input$selected_country]
          studies_with_all_selected <- filtered_df$Study_ID[filtered_df$Country %in% input$selected_country]
          studies_with_all_selected <- studies_with_all_selected[!studies_with_all_selected %in% studies_with_non_selected]
          filtered_df <- filtered_df[!(filtered_df$Study_ID %in% studies_with_all_selected), ]
        }
        else if ((!is.null(input$selected_element_continental)) & (input$selected_element_continental != Cont)){
          filtered_df <- filtered_df[filtered_df$Continent %in% c(input$selected_element_continental, Cont), ]
          studies_with_non_selected <- filtered_df$Study_ID[!filtered_df$Country %in% input$selected_country]
          studies_with_all_selected <- filtered_df$Study_ID[filtered_df$Country %in% input$selected_country]
          studies_with_all_selected <- studies_with_all_selected[!studies_with_all_selected %in% studies_with_non_selected]
          filtered_df <- filtered_df[!(filtered_df$Study_ID %in% studies_with_all_selected), ]
          filtered_df <- filtered_df %>%
            group_by(Study_ID) %>%
            filter(!all(Continent == Cont)) %>%
            ungroup()
          filtered_df <- filtered_df %>%
            filter(!(Continent == Cont & Country != input$selected_country))
          
        }
      } else {
        selected_studies <- unique(filtered_df$Study_ID[filtered_df$Country == input$selected_country])
        filtered_df <- filtered_df[filtered_df$Study_ID %in% selected_studies, ]
        filtered_df <- filtered_df %>%
          group_by(Study_ID) %>%
          filter(!all(Country == input$selected_country)) %>%
          ungroup()
        
      }
    } else if ((!is.null(input$selected_continent)) & (input$Bar == "continent")) {
      
      if (input$network_level_continent == "Continental-level") {
        selected_studies <- unique(filtered_df$Study_ID[filtered_df$Continent == input$selected_continent])
        filtered_df <- filtered_df[filtered_df$Study_ID %in% selected_studies, ]
        if ((!is.null(input$selected_element_continent)) & (input$selected_element_continent != input$selected_continent) ){
          filtered_df <- filtered_df[filtered_df$Continent %in% c(input$selected_element_continent, input$selected_continent), ]
          selected_studies <- unique(filtered_df$Study_ID[filtered_df$Continent == input$selected_element_continent])
          filtered_df <- filtered_df[filtered_df$Study_ID %in% selected_studies, ]
        }
        else if ((!is.null(input$selected_element_continent)) & (input$selected_element_continent == input$selected_continent)) {
          
          filtered_df <- filtered_df %>%
            group_by(TI) %>%
            filter(all(Continent == input$selected_continent)) %>%
            ungroup() 
          
        }
      } else {
        selected_studies <- unique(filtered_df$Study_ID[filtered_df$Continent == input$selected_continent])
        filtered_df <- filtered_df[filtered_df$Study_ID %in% selected_studies, ]
        filtered_df <- filtered_df %>%
          group_by(Study_ID) %>%
          filter(!all(Continent == input$selected_continent)) %>%
          ungroup()
        
      }
    }
  })
  
  
  
  
  # Render the collaboration map
  output$collab_map_country <- renderLeaflet({
    collab_data <- filtered_df()  # Get the filtered data
    # Aggregate data to get collaboration counts between affiliations
    
    View(collab_data)
    
    collab_counts <- collab_data %>%
      group_by(Affiliation) %>%
      summarise(
        Collaborations  = n_distinct(df$Affiliation[!(df$Affiliation %in% unique(Affiliation)) & df$Study_ID %in% Study_ID]),
        Publications = n_distinct(Study_ID),
        NumFaculties = n_distinct(Author)
      )
    
    # Create a new data frame for edges (collaboration links) between affiliations
    edges_df <- collab_data %>%
      inner_join(collab_counts, by = "Affiliation") %>%
      select(-Collaborations) %>%
      distinct(Affiliation, Study_ID) %>%
      inner_join(collab_data, by = "Study_ID") %>%
      select(from = Affiliation.x, to = Affiliation.y, TI) %>%
      filter(from != to) %>%
      group_by(from, to) %>%
      summarize(TI = list(unique(TI))) %>%
      ungroup()
    
    # Get locations for from and to columns in edges_df
    edges_df <- edges_df %>%
      mutate(
        from_loc = df$Country[match(from, df$Affiliation)],
        to_loc = df$Country[match(to, df$Affiliation)]
      )
    
    # Drop edges which has nothing to do with the selected_country
    edges_df <- edges_df %>%
      filter(from_loc == input$selected_country | to_loc == input$selected_country)
    
    #### Drop edges of the collaborations within selected_country when selected_element_country and selected_country are different
    if (((input$network_level_country == "Country-level") & (input$selected_element_country != input$selected_country)) | (input$network_level_country == "Global-level") | (input$network_level_country == "Continental-level")) {
      edges_df <- edges_df %>%
        filter(!(from_loc == input$selected_country & to_loc == input$selected_country))
    }
    
    # Remove location columns
    edges_df <- edges_df %>%
      select(-from_loc, -to_loc)
    
    
    Cont <- countrycode(
      sourcevar = input$selected_country,
      origin = "country.name",
      destination = "continent", 
      warn = TRUE)
    
    #### Drop edges of the collaborations in the Continent of selected_country
    if ((input$network_level_country == "Continental-level") & (Cont != input$selected_element_continental))  {
      edges_df <- edges_df %>%
        mutate(
          from_loc = df$Continent[match(from, df$Affiliation)],
          to_loc = df$Continent[match(to, df$Affiliation)]
        )
      edges_df <- edges_df %>%
        filter(from_loc == input$selected_element_continental | to_loc == input$selected_element_continental)
      
      
      edges_df <- edges_df %>%
        filter(!(from_loc == Cont & to_loc == Cont))
      
      edges_df <- edges_df %>%
        select(-from_loc, -to_loc)
      
    }
    
    
    edges_df$len <- lengths(edges_df$TI)
    
    # Calculate centrality measures
    collab_graph <- graph_from_data_frame(edges_df, directed = FALSE)
    # Degree centrality
    #degree_centrality <- degree(collab_graph, mode = "all")
    # Betweenness centrality
    #betweenness_centrality <- betweenness(collab_graph)
    # Closeness centrality
    #closeness_centrality <- closeness(collab_graph, mode = "all")
    # Eigenvector centrality
    #eigen_centrality <- eigen_centrality(collab_graph)$vector
    
    # Get unique locations for each affiliation
    locations <- collab_data %>%
      group_by(Affiliation, lat, long) %>%
      slice(1) %>%
      ungroup()
    
    # Determine the color based on the number of collaborations
    max_collaborations <- max(collab_counts$Collaborations)
    color_scale <- colorRampPalette(c("lightblue", "darkblue"))(max_collaborations)
    
    # Create a leaflet map
    collab_map <- leaflet() %>%
      addTiles()
    
    # Add nodes (affiliations) to the map
    for (i in 1:nrow(locations)) {
      aff <- locations$Affiliation[i]
      num_collaborations <- collab_counts$Collaborations[match(aff, collab_counts$Affiliation)]
      node_color <- color_scale[num_collaborations]
      collab_map <- collab_map %>%
        addCircleMarkers(
          lng = locations$long[i],
          lat = locations$lat[i],
          radius = 8,
          fillColor = node_color,
          color = "white",
          fillOpacity = 1,
          weight = 0,   # Set the weight to 0 to remove the black margin
          popup = paste(
            "Affiliation:", aff,
            "<br>Total Collaborations:", num_collaborations,
            "<br>Number of Researchers:", collab_counts$NumFaculties[match(aff, collab_counts$Affiliation)]
            #"<br>Degree Centrality:", degree_centrality[match(aff, names(degree_centrality))],
            #"<br>Betweenness Centrality:", betweenness_centrality[match(aff, names(betweenness_centrality))],
            #"<br>Closeness Centrality:", closeness_centrality[match(aff, names(closeness_centrality))],
            #"<br>Eigenvector Centrality:", eigen_centrality[match(aff, names(eigen_centrality))]
          ),
          label = as.character(aff)
        )
    }
    
    # Add edges (links) to the map
    for (i in 1:nrow(edges_df)) {
      from_aff <- edges_df$from[i]
      to_aff <- edges_df$to[i]
      paper_titles <- edges_df$TI[[i]]
      num_collaborations <- edges_df$len[i]
      
      # Extract the coordinates of the two affiliations for the link
      from_coords <- locations %>% filter(Affiliation == from_aff) %>% select(lat, long) %>% unlist() %>% as.numeric()
      to_coords <- locations %>% filter(Affiliation == to_aff) %>% select(lat, long) %>% unlist() %>% as.numeric()
      
      # Set the weight for the link based on the number of collaborations
      link_weight <- num_collaborations
      
      # Add the link (polyline) to the map with popups for number of collaborations and paper titles
      collab_map <- collab_map %>%
        addPolylines(
          lng = c(from_coords[2], to_coords[2]),
          lat = c(from_coords[1], to_coords[1]),
          color = "blue",
          opacity = 0.8,           # Set opacity to 80%
          weight = link_weight, # Adjust thickness based on the number of collaborations
          popup = paste(
            "Number of Collaborations:", num_collaborations,
            "<br><br>Paper Titles:<br>", paste(paper_titles, collapse = "<br><br>")
          )
        )
    }
    
    collab_map
  })
  
  # Render the collaboration map
  output$collab_map_continent <- renderLeaflet({
    collab_data <- filtered_df()  # Get the filtered data
    # Aggregate data to get collaboration counts between affiliations
    
    View(collab_data)
    
    collab_counts <- collab_data %>%
      group_by(Affiliation) %>%
      summarise(
        Collaborations  = n_distinct(df$Affiliation[!(df$Affiliation %in% unique(Affiliation)) & df$Study_ID %in% Study_ID]),
        Publications = n_distinct(Study_ID),
        NumFaculties = n_distinct(Author)
      )
    
    # Create a new data frame for edges (collaboration links) between affiliations
    edges_df <- collab_data %>%
      inner_join(collab_counts, by = "Affiliation") %>%
      select(-Collaborations) %>%
      distinct(Affiliation, Study_ID) %>%
      inner_join(collab_data, by = "Study_ID") %>%
      select(from = Affiliation.x, to = Affiliation.y, TI) %>%
      filter(from != to) %>%
      group_by(from, to) %>%
      summarize(TI = list(unique(TI))) %>%
      ungroup()
    
    edges_df <- edges_df %>%
      mutate(
        from_loc = df$Continent[match(from, df$Affiliation)],
        to_loc = df$Continent[match(to, df$Affiliation)]
      )
    
    edges_df <- edges_df %>%
      filter(from_loc == input$selected_continent | to_loc == input$selected_continent)
    
    
    if (((input$selected_element_continent != input$selected_continent)) | (input$network_level_continent == "Global-level")) {
      edges_df <- edges_df %>%
        filter(!(from_loc == input$selected_continent & to_loc == input$selected_continent))
    }
    
    edges_df <- edges_df %>%
      select(-from_loc, -to_loc)
    
    
    edges_df$len <- lengths(edges_df$TI)
    
    # Calculate centrality measures
    collab_graph <- graph_from_data_frame(edges_df, directed = FALSE)
    # Degree centrality
    #degree_centrality <- degree(collab_graph, mode = "all")
    # Betweenness centrality
    #betweenness_centrality <- betweenness(collab_graph)
    # Closeness centrality
    #closeness_centrality <- closeness(collab_graph, mode = "all")
    # Eigenvector centrality
    #eigen_centrality <- eigen_centrality(collab_graph)$vector
    
    # Get unique locations for each affiliation
    locations <- collab_data %>%
      group_by(Affiliation, lat, long) %>%
      slice(1) %>%
      ungroup()
    
    # Determine the color based on the number of collaborations
    max_collaborations <- max(collab_counts$Collaborations)
    color_scale <- colorRampPalette(c("lightblue", "darkblue"))(max_collaborations)
    
    # Create a leaflet map
    collab_map <- leaflet() %>%
      addTiles()
    
    # Add nodes (affiliations) to the map
    for (i in 1:nrow(locations)) {
      aff <- locations$Affiliation[i]
      num_collaborations <- collab_counts$Collaborations[match(aff, collab_counts$Affiliation)]
      node_color <- color_scale[num_collaborations]
      collab_map <- collab_map %>%
        addCircleMarkers(
          lng = locations$long[i],
          lat = locations$lat[i],
          radius = 8,
          fillColor = node_color,
          color = "white",
          fillOpacity = 1,
          weight = 0,   # Set the weight to 0 to remove the black margin
          popup = paste(
            "Affiliation:", aff,
            "<br>Total Collaborations:", num_collaborations,
            "<br>Number of Researchers:", collab_counts$NumFaculties[match(aff, collab_counts$Affiliation)]
            #"<br>Degree Centrality:", degree_centrality[match(aff, names(degree_centrality))],
            #"<br>Betweenness Centrality:", betweenness_centrality[match(aff, names(betweenness_centrality))],
            #"<br>Closeness Centrality:", closeness_centrality[match(aff, names(closeness_centrality))],
            #"<br>Eigenvector Centrality:", eigen_centrality[match(aff, names(eigen_centrality))]
          ),
          label = as.character(aff)
        )
    }
    
    # Add edges (links) to the map
    for (i in 1:nrow(edges_df)) {
      from_aff <- edges_df$from[i]
      to_aff <- edges_df$to[i]
      paper_titles <- edges_df$TI[[i]]
      num_collaborations <- edges_df$len[i]
      
      # Extract the coordinates of the two affiliations for the link
      from_coords <- locations %>% filter(Affiliation == from_aff) %>% select(lat, long) %>% unlist() %>% as.numeric()
      to_coords <- locations %>% filter(Affiliation == to_aff) %>% select(lat, long) %>% unlist() %>% as.numeric()
      
      # Set the weight for the link based on the number of collaborations
      link_weight <- num_collaborations
      
      # Add the link (polyline) to the map with popups for number of collaborations and paper titles
      collab_map <- collab_map %>%
        addPolylines(
          lng = c(from_coords[2], to_coords[2]),
          lat = c(from_coords[1], to_coords[1]),
          color = "blue",
          opacity = 0.8,           # Set opacity to 80%
          weight = link_weight, # Adjust thickness based on the number of collaborations
          popup = paste(
            "Number of Collaborations:", num_collaborations,
            "<br><br>Paper Titles:<br>", paste(paper_titles, collapse = "<br><br>")
          )
        )
    }
    
    collab_map
  })
}



# Run the shiny app
shinyApp(ui, server)

