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
  return(as.character(horoscopes$horoscope[i]))
}

# `recombine` function to split a horoscope by periods and paste with line breaks
recombine = function (text) {
  elements = unlist(strsplit(as.character(text), "\\. "))
  return(paste(elements, collapse = ".<br/><br/>"))
}


# SHINY APP CODE
# ==============

ui = dashboardPage(
  skin = "black",
  dashboardHeader(title = "AI Horoscope"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info")),
      menuItem("My Horoscope", tabName = "horoscope", icon = icon("star")),
      menuItem("Process", tabName = "process", icon = icon("cog")),
      menuItem("Sources", tabName = "sources", icon = icon("link"))
    )
  ),
  dashboardBody(
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
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
                htmlOutput("my_horoscope"),
                imageOutput("my_image")
              )),
      
      # Second Tab Content
      tabItem(tabName = "process",
              h2("Process & Techniques"),
              p("Hello"),
              p("")),
      
      # Second Tab Content
      tabItem(tabName = "sources",
              h2("Sources"),
              p("Hello"),
              p("")))))


server <- function(input, output, session) {
  
  options(warn = -1)
  observe({updateSelectInput(session, "day", label = "Day", 
                             choices = na.omit(cal[,input$month]))})
  
  # First Tab Output - Text
  # Reactive Button - Expression
  do = observeEvent(input$do, {
    
    output$my_horoscope = renderText({
      month = as.character(input$month)
      day = as.numeric(input$day)
      weekday = as.character(weekdays(as.Date(input$weekday)))
      sign = bday.to.sign(day, month)
      greeting = paste("Your horoscope sign is:", sign)
      fortune = generate(data, sign, weekday)
      output = paste(greeting, recombine(fortune), sep = "<br/><br/><br/>")
      HTML(output)
    })
    
  })
  
  # First Tab Output - Image
  output$my_image = renderImage({list(src = "imgs/img_1.png",
                                      contentType = "png",
                                      width = 400, height = 400)}, 
                                deleteFile = FALSE)

}

shinyApp(ui, server)