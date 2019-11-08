#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)

#' Cleans the phrase using the same algorithm as was used for preparing the trigrams
#' 
#' @param phrase String with phrase to clean.
#' @return String with cleaned phrase.
cleanPhrase <- function(phrase) {
    phrase <- tolower(phrase)
    
    # replace the non-alphanumeric characters with spaces
    # but only when at the beginning/end of the word
    phrase <- gsub("\\s['.,:;!?(){}<>]+(\\w)", " \\1", phrase)
    phrase <- gsub("(\\w)['.,:;!?(){}<>]+\\s", "\\1 ", phrase)
    
    # remove all words made of pure numbers and special characters
    phrase <- gsub("\\s+[0-9'.,:;!?(){}<>]+\\s+", " ", phrase)
    phrase <- gsub("^[0-9'.,:;!?(){}<>]+\\s+", " ", phrase)
    phrase <- gsub("\\s+[0-9'.,:;!?(){}<>]+$", " ", phrase)
    
    # replace spaces at the beginning of the line
    phrase <- gsub("^\\s+", "", phrase)
    
    # replace spaces at the end of the line
    phrase <- gsub("\\s+$", "", phrase)
    
    # replace all multiple spaces to one
    phrase <- gsub("\\s+", " ", phrase)
    
    # convert the `i` to the capital one `I`
    # when it's a separate word
    phrase <- gsub("(^|\\s)i($|\\s)", "\\1I\\2", phrase)
    
    phrase
}

#' Splits the phrase into words.
#' 
#' @param phrase String to split into words.
#' @return Vector of strings with words.
splitPhrase <- function(phrase){unlist(strsplit(phrase, " "))}

#' Read file with trigrams
data <- read_csv("data.csv")

#' Weights for the 0WordSearch, 1WordSearch, 2WordSearch.
#' This is needed for further tweaking the results.
zeroWordWeight <- 1
oneWordWeight <- 1
twoWordWeight <- 1
weights <- list(zeroWordWeight, oneWordWeight, twoWordWeight)
names(weights) <- c("0", "1", "2")

#' The number of items we will show in the UI.
show_top_n_items <- 8

#' Finds the best phrases to show based on the trigrams, for two words phrase.
#' 
#' @param data Data frame with the trigrams, with columns:
#'             word1, word2, word3, n
#' @param count The maximum number of phrases to return.
#' @param weight Integer to multiply the `n` param by.
#' @return Vector with no more than show_top_n_items strings.
#'         Each of them can have more than one word.
findWithTwoWords <- function(data, count, weight, searchWord1, searchWord2) {
    data %>%
        filter(word1 == searchWord1 & word2 == searchWord2) %>%
        select(word3, n) %>%
        rename(nextPhrase = word3) %>%
        mutate(n = n * twoWordWeight) %>%
        top_n(count)
}

#' Finds the best phrases to show based on the trigrams, for one word any phrase.
#' 
#' @param data Data frame with the trigrams, with columns:
#'             word1, word2, word3, n
#' @param count The maximum number of phrases to return.
#' @param weight Integer to multiply the `n` param by.
#' @return Vector with no more than show_top_n_items strings.
#'         Each of them can have more than one word.
findWithOneWord <- function(data, count, weight, searchWord) {
    data %>%
        filter(word1 == searchWord) %>%
        unite("nextPhrase", c(word2, word3), sep = " ") %>%
        select("nextPhrase", n) %>%
        mutate(n = n * oneWordWeight) %>%
        top_n(count)
    
}

#' Finds the best phrases to show based on the trigrams, without any phrase.
#' 
#' @param data Data frame with the trigrams, with columns:
#'             word1, word2, word3, n
#' @param count The maximum number of phrases to return.
#' @param weight Integer to multiply the `n` param by.
#' @return Vector with no more than show_top_n_items strings.
#'         Each of them can have more than one word.
findWithoutWord <- function(data, count, weight) {
    data %>%
        top_n(1000) %>% 
        select(word1) %>%
        dplyr::count(word1, sort = TRUE) %>%
        rename(nextPhrase = word1) %>%
        mutate(n = n * zeroWordWeight) %>%
        top_n(count)
}

#' Combines the data frames to prepare final results
#' 
#' @param dataFrames List of dataframes.
#' @param count The number of items in the result.
#' @result Vector with no more than show_top_n_items strings.
#'         Each of them can have more than one word.
prepareResults <- function(dataFrames, count) {
    bind_rows(dataFrames) %>% 
        arrange(desc(n)) %>% 
        top_n(count) %>%
        .$nextPhrase
}

#' Tries to find the next word for the passed phrase.
#' 
#' @param phrase String with the whole phrase to find the next phrase for.
#' @param data Data frame with the trigrams, with columns:
#'             word1, word2, word3, n
#' @param count The maximum number of phrases to return.
#' @param weights List of weights for the 0WordSearch, 1WordSearch, 2WordSearch
#' @return Vector with no more than show_top_n_items strings.
#'         Each of them can have more than one word.
findNextWord <- function(phrase, data, count, weights) {

    cleanedPhrase <- cleanPhrase(phrase)
    words <- splitPhrase(cleanedPhrase)
    
    noWord <- findWithoutWord(data, count, weights$"0")
    
    # words can be empty, what now?
    if (length(words) == 0) {
        return(prepareResults(list(noWord), count))
    }
    
    # can be just one word
    if (length(words) == 1) {
        oneWord <- findWithOneWord(data, count, weights$"1", words[1])
        
        return(prepareResults(list(noWord, oneWord), count))
    }
    
    # in case of two words or more:
    lastWords <- tail(words, 2)
    oneWord <- findWithOneWord(data, count, weights$"1", lastWords[2])
    twoWords <- findWithTwoWords(data, count, weights$"2", lastWords[1], lastWords[2])
    
    return(prepareResults(list(noWord, oneWord, twoWords), count))
}

updateText <- function(input, session, buttonNumber) {
    word <- findNextWord(input$phrase, data, show_top_n_items, weights)[[buttonNumber]]
    updateTextInput(
        session,
        "phrase",
        value = paste(input$phrase, word)
    )
}

# Main Shiny server function.
shinyServer(function(input, output, session) {
 
    # This could be nicely refactores
    observeEvent(input$button1, {
        updateText(input, session, 1)
    })
    
    observeEvent(input$button2, {
        updateText(input, session, 2)
    })
    
    observeEvent(input$button3, {
        updateText(input, session, 3)
    })
    
    observeEvent(input$button4, {
        updateText(input, session, 4)
    })
    
    observeEvent(input$button5, {
        updateText(input, session, 5)
    })
    
    observeEvent(input$button6, {
        updateText(input, session, 6)
    })
    
    observeEvent(input$button7, {
        updateText(input, session, 7)
    })
    
    observeEvent(input$button8, {
        updateText(input, session, 8)
    })
    
    observe({
        # phrase read from the input field
        phrase <- input$phrase
        foundWords <- findNextWord(phrase, data, show_top_n_items, weights)

        for (i in 1:show_top_n_items) {
            
            updateActionButton(
                session,
                paste("button", i, sep = ""),
                label = foundWords[[i]]
            )
        }
        
    })

})
