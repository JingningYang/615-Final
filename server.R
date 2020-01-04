library(leaflet)
library(RColorBrewer)
library(lattice)
library(dplyr)
library(scales)
library(shiny)
library(tidyverse)
library(leaflet.extras)
library(DT)
library(shiny)
library(viridisLite)
library(wordcloud)
library(wordcloud2)
library(textdata)
library(tidytext)

business <- read.csv("business.csv")
att <- read.csv("attribute.csv")
att1 <- select(att, -state)
review <- read.csv("review.csv")

shinyServer(function(input, output){
    ###create the map
    pal <- colorFactor(
        palette = viridis(100),
        domain = business$city)
    output$map <- renderLeaflet({
        leaflet(business)%>% 
            addTiles() %>% 
            setView(lng = -80.85, lat = 39.5, zoom = 7) %>%
            addCircles(data = business, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = 100, 
                       label = ~as.character(paste0("Restaurant: ", sep = " ", name)), labelOptions = labelOptions(textsize = "20px"), color = ~pal(city), fillOpacity = 0.5)
    })
    
    observeEvent(input$Res, {
        leafletProxy("map")%>%
            clearGroup("Markers") %>%
            addMarkers(data = business[business$name == input$Res, ], ~longitude, ~latitude, group = "Markers")
    })
    observeEvent(input$state1, {
        leafletProxy("map")%>%
            clearGroup("Markers") %>%
            addMarkers(data = business[business$state == input$state1, ], ~longitude, ~latitude, group = "Markers")
    })
    observeEvent(input$star, {
        leafletProxy("map", data = business[business$stars == input$star, ])%>%
            clearGroup("Markers") %>%
            addMarkers( ~longitude, ~latitude, group = "Markers")
    })
    
    ###### business exploration
    output$inf <- DT::renderDataTable(DT::datatable({
        data <- business
        if(input$Re != "All"){
            data <- data[data$name == input$Re, ]
        }
        if(input$stat != "All"){
            data <- data[data$state == input$stat, ]
        }
        if(input$sta != "All"){
            data <- data[data$stars == input$sta, ]
        }
        data
    }))
    #####EDA
    # output$eda <- renderPlotly({
    #     ggplotly(
    #         ggplot(att) +
    #             geom_point(aes(x=stars, y=reviewcount, color=state), stat = "identity",alpha=0.3) +
    #             facet_grid(.~state) +scale_y_log10()
    #     )
    # })
    ####PCA
    pc3 <- reactive({
        principal(att1, nfactors = input$factor, rotate = "varimax")
    })
    output$result <- renderPlot({
        fa.diagram(pc3(),simple=TRUE)
    })
    ##### Explore preference
    output$explore <- renderPlotly({
        a <- business %>% filter(str_detect(categories, "Restaurant")) %>%
            unnest(categories) %>%
            filter(categories != "Restaurants") %>%
            count(state, categories) %>%
            filter(n > 10) %>%
            group_by(state) %>%
            top_n(1, n)
        a$categories[a$categories == "Restaurants, Pizza"] <- "Pizza, Restaurants"
        ggplotly(ggplot(a, aes(x=state, y=n, fill=categories)) +
                     geom_bar(stat = "identity") +
                     labs(y="Number of restaurants"))
    })
    ######Wordcloud
    PA <- subset(review, stars > 4 & state == 'PA')
    PAr <- PA %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
    seperate2 <- PAr %>% 
        count(bigram, sort = TRUE) %>% 
        separate(bigram, c("word1", "word2"), sep = " ") 
    
    OH <- subset(review, stars > 4 & state == 'OH')
    OHr <- OH %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
    seperate3 <- OHr %>% 
        count(bigram, sort = TRUE) %>% 
        separate(bigram, c("word1", "word2"), sep = " ")
    
    
    output$wordcloud <- renderWordcloud2({
        if(input$pa){
            PA <- subset(review, stars > 4 & state == 'PA')
            PAr <- PA %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
            seperate2 <- PAr %>% 
                count(bigram, sort = TRUE) %>% 
                separate(bigram, c("word1", "word2"), sep = " ") 
            unite2 <- seperate2 %>% 
                filter(!word1 %in% stop_words$word) %>% #remove cases where either is a stop-word.
                filter(!word2 %in% stop_words$word) %>% 
                unite(bigram, word1, word2, sep = " ") %>% 
                head(input$fre)
            wordcloud2(unite2, shape = 'circle' ,color = "random-light", size = 0.3, backgroundColor = "white")
        }
        else if(input$oh){
            OH <- subset(review, stars > 4 & state == 'OH')
            OHr <- OH %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
            seperate3 <- OHr %>% 
                count(bigram, sort = TRUE) %>% 
                separate(bigram, c("word1", "word2"), sep = " ")
            unite3 <- seperate3 %>% 
                filter(!word1 %in% stop_words$word) %>% #remove cases where either is a stop-word.
                filter(!word2 %in% stop_words$word) %>% 
                unite(bigram, word1, word2, sep = " ") %>% 
                head(input$fre)
            wordcloud2(unite3, shape = "circle",color = "random-light", size = 0.3, backgroundColor = "white")
        }
    })
    ######Sentiment Analysis
    output$sentiment <- renderPlot({
        Afinn <- get_sentiments("afinn")
        negation_words <- c("not", "no", "never", "without","none","bad")
        if(input$state == "PA"){
            # PA <- subset(review, stars > 4 & state == 'PA')
            # PAr <- PA %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
            # seperate2 <- PAr %>% 
            #     count(bigram, sort = TRUE) %>% 
            #     separate(bigram, c("word1", "word2"), sep = " ")
            not_words <- seperate2 %>%
                filter(word1 %in% negation_words) %>%
                inner_join(Afinn, by = c(word2 = "word")) %>%
                count(word1,word2, value, sort = TRUE)
            
            not_words %>%
                mutate(contribution = n * value) %>%
                arrange(desc(abs(contribution))) %>%
                head(20) %>%
                mutate(word2 = reorder(word2, contribution)) %>%
                ggplot(aes(word2, n * value, fill = n * value > 0)) +
                geom_col(show.legend = FALSE) +
                xlab("Words preceded by \"not,no,never,without,none and bad\"") +
                ylab("Sentiment value * number of occurrences in state PA") +
                coord_flip()
        }
        else if(input$state == "OH"){
            # OH <- subset(review, stars > 4 & state == 'OH')
            # OHr <- OH %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
            # seperate3 <- OHr %>% 
            #     count(bigram, sort = TRUE) %>% 
            #     separate(bigram, c("word1", "word2"), sep = " ")
            not_words1 <- seperate3 %>%
                filter(word1 %in% negation_words) %>%
                inner_join(Afinn, by = c(word2 = "word")) %>%
                count(word1,word2, value, sort = TRUE)
            
            not_words1 %>%
                mutate(contribution = n * value) %>%
                arrange(desc(abs(contribution))) %>%
                head(20) %>%
                mutate(word2 = reorder(word2, contribution)) %>%
                ggplot(aes(word2, n * value, fill = n * value > 0)) +
                geom_col(show.legend = FALSE) +
                xlab("Words preceded by \"not,no,never,without,none and bad\"") +
                ylab("Sentiment value * number of occurrences in state OH") +
                coord_flip()
        }
    })
    ###### sentiment compare
    output$compare <- renderPlot({
        if(input$state2 == "PA"){
           
            seperate2 %>%
                inner_join(get_sentiments("bing"), by=c(word2="word")) %>%
                count(word1, word2, sentiment, sort = TRUE) %>%
                acast(word2 ~ sentiment, value.var = "n", fill = 0) %>%
                comparison.cloud(colors = c("blue", "red"),
                                 max.words = 50)
        }
        else if(input$state2 == "OH"){
            # OH <- subset(review, stars > 4 & state == 'OH')
            # OHr <- OH %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
            # seperate3 <- OHr %>% 
            #     count(bigram, sort = TRUE) %>% 
            #     separate(bigram, c("word1", "word2"), sep = " ")
            seperate3 %>%
                inner_join(get_sentiments("bing"), by=c(word2="word")) %>%
                count(word1, word2, sentiment, sort = TRUE) %>%
                acast(word2 ~ sentiment, value.var = "n", fill = 0) %>%
                comparison.cloud(colors = c("blue", "red"),
                                 max.words = 50)
        }
    })
    
    
})
