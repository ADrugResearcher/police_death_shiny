# Currently I just download the csv file from trackinginjustice.ca
# https://trackinginjustice.ca/explore-the-data/

library(readr)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

`%notin%` <- Negate(`%in%`)
police_k <- read_csv("Trackinginjustice Website Data Sheet.csv", 
                     col_types = cols(DATE = col_date(format = "%d/%m/%Y")))
pop <- data.frame(Province = c("ON", "QC", "BC", "AB", 
                               "SK" ,"MB", "NS",
                               "NB", "NL", "PE",
                               "NT", "YK", "NU"),
                  Pop = c(14223942,8501833, 5000879, 
                          4262635,1132505,1342153, 
                          969383,775610,510550,154331,
                          41070, 40232,36858))
colnames(police_k) <- str_to_title(colnames(police_k) )

police_k <- janitor::clean_names(police_k)
colnames(police_k) <- str_to_title(colnames(police_k) )

cats <- c("Province","Gender", "Race", "Highest_level_force", "Age_group")
thirdfilter <- c("None","Gender", "Race",  "Highest_level_force", "Age_group")
police_k <- police_k %>%
  as_tibble() %>%
  mutate(across(where(is.character),~ str_to_title(.x)),
         Province = str_to_upper(Province)) %>%
  select(Age:Highest_level_force) %>%
  mutate(Year = year(Date),
         Gender = str_replace_all(Gender, c("Male" = "Men",
                                            "Female" = "Women"))) %>%
  left_join(pop)

police_k <- police_k %>%
  mutate(Age_group = case_when(
    Age == 0 ~ "Unknown",
    Age <= 19 ~ "1-19",
    Age >= 20 & Age <= 30 ~ "20-30",
    Age >= 31 & Age <= 40 ~ "31-40",
    Age >= 41 & Age <=50 ~ "41-50",
    Age >= 51 & Age <= 61 ~ "51-61",
    Age >= 62 ~ "62+"
  )) 

police_k$Age_group <- factor(police_k$Age_group, levels = c( "1-19",
                                                             "20-30",
                                                             "31-40",
                                                             "41-50",
                                                             "51-61",
                                                             "62+",
                                                             "Unknown"),
                             ordered = TRUE)
police_k <- police_k %>%
  mutate(Province = "Canada",
         Pop = sum(pop$Pop, na.rm=T)) %>%
  bind_rows(police_k)
police_k <- police_k %>%
  select(-Date, -Age)
all_names <- colnames(police_k)
library(shinythemes)
library(shiny)

ui <- navbarPage(title = "Police-involved Death in Canada - Tracking Justice Data",
####----------------------Main Page of UI----------------------------------####
                 theme = shinytheme("flatly"),
                 tabPanel("Overview of Stats",
                          sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput("the_cols",
                                                     "Main Variable",
                                                     choices =cats,
                                                     selected = "Province"),
                                         selectInput("the_filter",
                                                     "Filter By",
                                                     choices = c("Cross-Provincial\nComparison", unique(police_k$Province)),
                                                     selected = "Cross-Provincial\nComparison",
                                                     multiple = T),
                                         selectInput("third", "Breakdown by Group",
                                                     choices = thirdfilter,
                                                     selected = "None"),
                                         h5("Data graciously provided by: "),
                                            h5(a("Tracking (In)Justice", href ="https://trackinginjustice.ca/")), 
                                         h5("I've written this mostly as a handy tool for exploring the data in a bit finer detail. It is very much a WIP - all mistakes as always are my own"),
                                         h5("In honour of Sarah Speight. Researcher, activist & friend that without whom this would not have been possible to make"),
                                         h5("If there are other visualizations you think would be useful to add to this, I'm all ears. Shoot me a message on twitter @ADrugResearcher"),
                                         h6("Data last updated:Mar 15 2023")
                                         
                                         
                                         
                                         
                                         
                            ),
                            
                            mainPanel(width = 9,
                                      fluidRow(
                                        sliderInput("Year_by",
                                                    label = "",
                                                    min = min(police_k$Year),
                                                    max = max(police_k$Year),
                                                    value= c(min(police_k$Year),
                                                             max(police_k$Year)),
                                                    dragRange = T,
                                                    sep = "",
                                                    width = "600px"
                                        )),
                                      fluidRow(
                                        plotOutput("poli", width = "100%",
                                                   height = "600px",
                                                   brush = "plot_brush"))
                            ))
                 ),
####-------------------------Date Page--------------------------------------####
                 tabPanel("Deaths by Year",
                          sidebarLayout(sidebarPanel(width =2,
                                                     selectInput("var", "Main Variable",
                                                                 choices = cats,
                                                                 selected = "Race"),
                                                     selectInput("fillf", "Filter",
                                                                 choices = c("Province",
                                                                             "Highest_level_force",
                                                                             "Gender", "Age_group"),
                                                                 selected = "Province"),
                                                     checkboxGroupInput("check", "Filter here",
                                                                        choices = c(unique(police_k$Province)),
                                                                        selected = c(unique(police_k$Province)),
                                                                        
                                                                        inline = T
                                                     ),
                                                     checkboxInput("allnone",
                                                                  "(Un)select All", 
                                                                   value = T),
                                                     radioButtons("radio", "Per Capita (only for Provinces)", 
                                                                  choices = c("Yes", "No"),
                                                                  selected = "No"
                                                     )
                          ),
                          mainPanel(width = 10,
                                    plotOutput("ttime",  
                                               width = "100%",
                                               height = "750px",
                                               brush = "plot_brush"))
                          ))
                 
)                   
####-----------------------END UI-------------------------------------------####

server <- function(input, output, session) {
  
####---------------------------Comparative Bar Chart------------------------####
#first create reactive variables
#input$third = grouping variable for bar graph
  third <- reactive({input$third})
  re_cols <- reactive({input$the_cols})
#tcols is a grouping variable of the 1st & third inputs
  t_cols <- reactive({

    if(third() != "None"){
      return(c(re_cols(), third()))
    }else{
      return(re_cols())
    }
    
  })  

#The observe filter is meant to remove the cross-provincial comparison when
#it is inappropriate, i.e., basically whenever another province is selected
  observeEvent(input$the_filter, {
    infill <- input$the_filter
    intrue <- infill[infill == "Cross-Provincial\nComparison"]
    
    
    if(length(infill) >=2 & "Cross-Provincial\nComparison" %in% infill){
      infill <- infill[infill %notin% intrue]
      updateSelectInput(session, "the_filter",
                        choices =  c("Cross-Provincial\nComparison", 
                                     unique(police_k$Province)),
                        selected = infill)
      
    } else{
      updateSelectInput(session, "the_filter",
                        choices =  c("Cross-Provincial\nComparison",
                                     unique(police_k$Province)),
                        selected = infill)
    }
  })
  #Now we pass on input the provincial filter to reactive variable
  t_filter <- reactive({input$the_filter})

  
# pk() is going to be the reactive df
# the police1 ifelse statement is a bit janky, but otherwise ifelse
# returns a list, which will not work :'(
  pk <- reactive({

    the_filt <- paste0(t_filter(), collapse = ", ")

    if(isTRUE(str_detect(the_filt, "Cross-Provincial\nComparison"))){
      police1 <- police_k %>%
        filter(Year>= input$Year_by[1] &
                 Year  <=  input$Year_by[2])
    } else{
      police1 <- police_k %>%
        filter(Province %in% t_filter() &
                 Year>= input$Year_by[1] &
                 Year  <=  input$Year_by[2])
    }


    police1 <- police1 %>%
      select(contains(t_cols())) %>%
      group_by_all() %>%
      count(name = "tcount") %>%
      ungroup()

    return(police1)
  })
  
  
  output$poli <- renderPlot({
    pg <- pk()
    val_test <- ifelse(third() == re_cols(), 1,0)
    validate(
      need(val_test == 0, "Grouping Variable & Pick Variable can't be the same")
    )

    tlength <- length(t_cols())
    title_val <- gsub("_", " ", str_to_title(colnames(pg)[1]))


    ifelse( tlength ==1|t_cols()[1] == t_cols()[2],colnames(pg) <-  c( "cat", "tcount"),
           colnames(pg) <-  c("cat", "fvar", "tcount"))

    pg <- pg %>%
        arrange(-tcount)
    if(!is.ordered(pg$cat)){
    pg$cat <- ordered(pg$cat, levels = pg$cat, labels = pg$cat)
    }
    ifelse("Cross-Provincial\nComparison" %in% t_filter(),
           the_place <- "All Provinces",
           the_place <- paste0(t_filter(), collapse = ", "))

    the_title <- paste("Police-related Deaths in ",the_place , " by ", title_val, " (",
                       input$Year_by[1],
                       "-", input$Year_by[2], ")", sep = "")
    x_lab <- t_cols()
    
    my_coul <- c(brewer.pal(8, "Dark2"),  "#000000", "#ffffff")
    my_colors <- my_coul[as.numeric(as.factor(unique(pg$fvar)))]
    names(my_colors) <- unique(pg$fvar)

    if(third() != "None"){

      t2 <- pg%>%
        select(-tcount) %>%
        distinct(.keep_all = T) %>%
        expand.grid() %>%
        distinct() %>%
        unite("filler_name", cat:fvar, sep = "_") %>%
        mutate(tcount = 0)
      
      tallest_bar <- pg %>%
        unite("filler_name", cat:fvar, sep = "_") %>%
        full_join(t2) %>%
        separate(filler_name, into = c("cat", "fvar"),
                 remove = T, sep = "_") %>%
        group_by(cat) %>%
        add_count(name = "max_count", wt = tcount) %>%
        ungroup()
     
    max_count <- max(tallest_bar$max_count)+round(median(tallest_bar$max_count), digits = -1)

    rm(tallest_bar, t2)
    } else{
      max_count <- max(pg$tcount,na.rm = T)
    }


    if(tlength == 1){
      g <- ggplot(pg, aes(x = cat, y = tcount))+
        geom_col(fill = "Dark Blue") 
    } else{
      fill_name <- str_to_title(gsub("_", " ", third()))

      g <- ggplot(pg, aes(x = cat, y = tcount, group = fvar, fill = fvar))+
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(name = fill_name, values = my_colors) 
        
      
    }
    
    g<- g+ scale_y_continuous(expand =c(0,0), limits = c(0, max_count))+
      labs(title = the_title, 
           x = x_lab,
           y = "Count") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 30),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 20),
            plot.caption = element_text(size = 12),
            legend.text = element_text(size=14),
            legend.title =  element_text(size=16))
    g
  })
  
####--------------------------Line Graph------------------------------------####  
  observeEvent(input$fillf,{
    
    new_ch = police_k %>%
      select(contains(input$fillf)) %>%
      distinct()
    
    new_ch <- unlist(as.vector(new_ch[,1]), use.names = F)
    
    updateCheckboxGroupInput(
      session = session,
      inputId = "check",
      choices = new_ch,
      selected = new_ch,
      inline = T)

  }, ignoreInit = T)
  observeEvent(input$allnone,{
    
    if(input$allnone == T){

      updateCheckboxGroupInput(
        session = session,
        inputId = "check",
        choices = c(unique(police_k$Province)),
        selected = c(unique(police_k$Province)),
        inline = T)
    } else if(input$allnone ==F){
      updateCheckboxGroupInput(
        session = session,
        inputId = "check",
        choices = c(unique(police_k$Province)),
        selected = NA,
        inline = T)

    }
    
  }, ignoreInit = T)


  pkt <- reactive({
    
    police_k %>%
      filter(if_any(matches(input$fillf), ~ . %in% input$check)) %>%
      filter(Province != "Canada")

  })
  
 
  output$ttime <- renderPlot({

    inputs <- c("Year", input$var)
    pt <- pkt()
    req(nrow(pt)>1)
    t_name <- str_to_title(gsub("_", " ", input$var))
    if(input$var == "Province" & input$radio == "Yes"){
     
      pt <- pt %>%
        select(contains(inputs)) %>%
        filter(Province %notin% c("YT", "YK", "NT", "NU")) %>%
        group_by_all() %>%
        count(name = "tcount") %>%
        ungroup() %>%
        left_join(pop) %>%
        mutate(tcount = tcount/Pop*100000) %>%
        select(-Pop) %>%
        complete(Province, Year= seq(2000,2022, by=1)) %>%
        select(Year, Province, tcount)
        pt$tcount[is.na(pt$tcount)] <- 0.00
     

      title1 <- paste("Police-related deaths from 2000-2022 by ", input$var, "(", 
                      "Per 100 000)",
                      sep ="")
      y_lab <- "Per 100 000"
      
    } else if(input$var == "Province" & input$radio == "No"){
      pt <- pt %>%
        select(contains(inputs)) %>%
        group_by_all() %>%
        count(name = "tcount") %>%
        ungroup()  %>%
        complete(Province, Year= seq(2000,2022, by=1)) %>%
        select(Year, Province, tcount)
      pt$tcount[is.na(pt$tcount)] <- 0.00
      
      y_lab <- "Count"
      
      title1 <- paste("Police-related deaths from 2000-2022 by ", t_name,
                      sep ="")
      
    }else if(input$var != "Province" & input$radio == "Yes"){
      pt <- pt %>%
        select(contains(inputs)) %>%
        group_by_all() %>%
        count(name = "tcount") %>%
        ungroup() 
      
      y_lab <- "Count\n(Per Capita Unavailble)"
      title1 <- paste("Police-related deaths from 2000-2022 by ", last(inputs),
                      sep ="")
      
    } else{
      pt <- pt %>%
        select(contains(inputs)) %>%
        group_by_all() %>%
        count(name = "tcount") %>%
        ungroup() 
      
      y_lab <- "Count"
      
      title1 <- paste("Police-related deaths from 2000-2022 by ", t_name,
                      sep ="")
    }

    colnames(pt) <- c("Year", "val", "tcount")
    my_coul <- c(brewer.pal(12, "Set3"), "#808080", "#000000")
    my_colors <- my_coul[as.numeric(as.factor(unique(pt$val)))]
    names(my_colors) <- unique(pt$val)
    
    fill_name <- str_to_title(gsub("_", " ", input$var))
    ggplot(pt, aes(x =Year, y = tcount,group = val, colour = val))+
      geom_line(linewidth = 2.5) +
      geom_point(size = 4) +
      scale_y_continuous(expand =c(0,0), limits = c(0,max(pt$tcount)+0.4))+
      scale_colour_manual(name = fill_name,values= my_colors, na.value ="grey50",
                          guide= guide_legend(order = 2, 
                                       nrow = 7,
                                       ncol =3))+
      labs(x= "Year", y = y_lab,title = title1 ) +
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, size = 30),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 20),
            plot.caption = element_text(size = 12),
            legend.text = element_text(size=17),
            legend.title =  element_text(size=18),
            legend.key.size = unit(1.5, 'cm')) +
      guides()
    
  })
  
}
shinyApp(ui = ui, server = server)
