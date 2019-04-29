## app.R ##

# Package and Data Load-In
# ========================

library(tidyverse)
library(plyr)
library(shiny)
library(rsconnect)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(png)
library(OpenImageR)

# Horoscope Template - Text
data = read.csv("horoscopes.csv")

# `cal` dataframe for days of each month
cal = matrix(nrow = 31, ncol = 12)
cal = as.data.frame(cal)
colnames(cal) = month.name
cal[,c("January", "March", "May", "July", "August", "October", "December")] = 1:31
cal[,c("April", "June", "September", "November")] = c(1:30, NA)
cal[,"February"] = c(1:29, NA, NA)

# `bday.to.sign` function to take birthday and return a sign
bday.to.sign = function (day, month) {
  sign = "Capricorn"
  if (month == "January" & day > 20) {sign = "Aquarius"}
  if (month == "February" & day < 20) {sign = "Aquarius"}
  if (month == "February" & day > 19) {sign = "Pisces"}
  if (month == "March" & day < 21) {sign = "Pisces"}
  if (month == "March" & day > 20) {sign = "Aries"}
  if (month == "April" & day < 21) {sign = "Aries"}
  if (month == "April" & day > 20) {sign = "Taurus"}
  if (month == "May" & day < 22) {sign = "Taurus"}
  if (month == "May" & day > 21) {sign = "Gemini"}
  if (month == "June" & day < 22) {sign = "Gemini"}
  if (month == "June" & day > 21) {sign = "Cancer"}
  if (month == "July" & day < 23) {sign = "Cancer"}
  if (month == "July" & day > 22) {sign = "Leo"}
  if (month == "August" & day < 23) {sign = "Leo"}
  if (month == "August" & day > 22) {sign = "Virgo"}
  if (month == "September" & day < 24) {sign = "Virgo"}
  if (month == "September" & day > 23) {sign = "Libra"}
  if (month == "October" & day < 24) {sign = "Libra"}
  if (month == "October" & day > 20) {sign = "Scorpio"}
  if (month == "November" & day < 23) {sign = "Scorpio"}
  if (month == "November" & day > 22) {sign = "Sagittarius"}
  if (month == "December" & day < 22) {sign = "Sagittarius"}
  return(sign)
}

# `generate` function to choose a random horoscope given a sign
generate = function (data, sign, weekday) {
  horoscopes = filter(data, sign == sign) %>% 
    filter(day_of_week == weekday)
  i = sample(1:nrow(horoscopes), 1)
  return(list(as.character(horoscopes$horoscope[i]),
              as.character(horoscopes$img_path[i])))
}

# `recombine` function to split a horoscope by periods and paste with line breaks and images in between
recombine = function (list) {
  
  elements = unlist(strsplit(list[[1]], "(?<![^!?.])\\s+", perl = TRUE))
  
  paths = vector(length = 3)
  for (i in 1:3) {paths[i] = paste(list[[2]], ".", as.character(i), ".png", sep = "")}
  
  if (length(elements) == 1) {elements = c(elements, "", "", "")}
  if (length(elements) == 2) {elements = c(elements, "", "")}
  if (length(elements) == 3) {elements = c(elements, "")}
  
  group = round(length(elements)/4)
  
  texts = vector(length = 4)
  if (length(elements) == 4) {texts = elements} else {
    for (i in 1:3) {texts[i] = paste(elements[seq(1,length(elements),group)[i]:i+group-1], collapse = "<br/>")}
  }
  
  texts[4] = ifelse(length(elements) > group*3, paste(tail(elements, length(elements) - group*3), collapse = "<br/>"), "")
  
  return(list(texts, paths))
  
}


# SHINY APP CODE
# ==============

ui = dashboardPage(
  skin = "black",
  dashboardHeader(title = "AI Horoscope"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info")),
      menuItem("My Horoscope", tabName = "horoscope", icon = icon("star"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.content-wrapper, .right-side {
                                background-color: #ffffff;'))),
    tabItems(
      
      tabItem(tabName = "introduction",
              h2("Introduction"),
              br(),
              p("Horoscopes are a forecast of one’s future, based on the 
                relative position of the stars and planets at one’s birth. There 
                are 12 horoscopes that align somewhat similarly to the months of 
                a year: Aries, Taurus, Gemini, Cancer, Leo, Virgo, Libra, 
                Scorpio, Sagittarius, Capricorn, Aquarius, and Pisces"),
              p("Generally, these 12 horoscopes are described as having 
                different personality types. Although horoscope forecasts differ 
                depending on the astrologist reading the stars, there exists a 
                strong belief system behind this system. This method of 
                fortune-telling has become very trendy, with daily horoscope 
                features on platforms such as newspapers, Facebook, and 
                Snapchat."),
              p("Our goal is to model horoscope data in order to generate a 
                series of fortunes. Regardless of whether the outcome is 
                perceived to be comical or poetic, we believe that a piece that 
                spurs self-introspection is very valuable. Our project advocates 
                for this form of thought in our audience."),
              br(),
              p("By: Jenn Choi, Julie Kim, Sophia Yoo")),
      
      # First Tab Content
      tabItem(tabName = "horoscope",
              h2("My Horoscope"),
              p("Please select your birthday and click `Reveal Horoscope`"),
              
              fluidPage(
                fluidRow(
                  column(5,
                         selectInput("month", label = HTML("Birthday<br/>Month"),
                                     choices = month.name,
                                     selectize = FALSE),
                         selectInput("day", label = "Day",
                                     choices = NULL,
                                     selectize = FALSE)),
                  column(5, br(),
                         dateInput("weekday", label = "Which day's horoscope would you like to see?", 
                                   value = NULL, min = NULL, max = NULL, 
                                   format = "mm-dd-yyyy", 
                                   startview = "month", weekstart = 0, 
                                   language = "en", width = NULL),
                         br(), 
                         actionButton("do", "Reveal Horoscope"))),
                htmlOutput("greeting"),
                htmlOutput("text1")),
              fluidRow(align = "center", imageOutput("image1", inline = TRUE),
                       htmlOutput("text2")),
              fluidRow(align = "center", imageOutput("image2", inline = TRUE),
                       htmlOutput("text3")),
              fluidRow(align = "center", imageOutput("image3", inline = TRUE),
                       htmlOutput("text4"))))))


server <- function(input, output, session) {
  
  options(warn = -1)
  observe({updateSelectInput(session, "day", label = "Day", 
                             choices = na.omit(cal[,input$month]))})
  
  # First Tab Output - Text
  # Reactive Button - Expression
  do = observeEvent(input$do, {
    
    month = as.character(input$month)
    day = as.numeric(input$day)
    weekday = as.character(weekdays(as.Date(input$weekday)))
    sign = bday.to.sign(day, month)
    data = recombine(generate(data, sign, weekday))
    
    output$greeting = renderText({
      paste("<br/>", "<center>", "<i>", "Your horoscope sign is: ", sign, "<i/>", "</center>", "<br/><br/>", sep = "")
    })
    
    output$text1 = renderText({paste("<center>", data[[1]][1], "</center>", sep = "")})
    output$image1 = renderImage({list(src = data[[2]][1], contentType = "png",
                                      width = 390, height = 120)}, 
                                deleteFile = FALSE)
    output$text2 = renderText({paste("<center>", data[[1]][2], "</center>", sep = "")})
    output$image2 = renderImage({list(src = data[[2]][2], contentType = "png",
                                      width = 390, height = 120)}, 
                                deleteFile = FALSE)
    output$text3 = renderText({paste("<center>", data[[1]][3], "</center>", sep = "")})
    output$image3 = renderImage({list(src = data[[2]][3], contentType = "png",
                                      width = 390, height = 120)}, 
                                deleteFile = FALSE)
    output$text4 = renderText({paste("<center>", data[[1]][4], "</center>", sep = "")})
    
  })

}

shinyApp(ui, server)