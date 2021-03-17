
library(shiny)
library(dplyr)
library(ggplot2); theme_set(theme_classic())
library(xtable)
library(magrittr)
library(rmarkdown)


shinyServer(function(input, output, session) {

  ### update default digits for rounding based on dist/family selected
  ###################################################################
  output$digits_input <- renderUI({
    
    initial_round_digits <- switch(input$dist,
      norm = 4, t = 3, chisq = 3, f = 2, binomial = 4, poisson = 4
    )
    
    numericInput("round_digits", "Number of Digits to Round to", 
      value = initial_round_digits
    )
  })
  

  
  ### create pdf of table
  ###################################################################
  output$download_table <- downloadHandler(

    filename = function() {
      
      if (input$dist == "norm" & input$norm_area == "left") {
        "Standard Normal Table - Left.pdf"
      } else if (input$dist == "norm" & input$norm_area == "middle") {
        "Standard Normal Table - Middle.pdf"
      } else if (input$dist == "norm" & input$norm_area == "right") {
        "Standard Normal Table - Right.pdf"
      } else if (input$dist == "t") {
        "t Table.pdf"
      } else if (input$dist == "chisq") {
        "Chi Square Table.pdf"
      } else if (input$dist == "f") {
        "F Table.pdf"
      } else if (input$dist == "binomial" & input$binom_area == "binom_pmf") {
        "Binomial PMF Table.pdf"
      } else if (input$dist == "binomial" & input$binom_area == "binom_cdf") {
        "Binomial CDF Table.pdf"
      } else if (input$dist == "poisson" & input$pois_area == "pois_pmf_2") {
        "Poisson PMF Table.pdf"
      } else if (input$dist == "poisson" & input$pois_area == "pois_pmf_5") {
        "Poisson PMF Table.pdf"
      } else if (input$dist == "poisson" & input$pois_area == "pois_cdf_2") {
        "Poisson CDF Table.pdf"
      } else if (input$dist == "poisson" & input$pois_area == "pois_cdf_5") {
        "Poisson CDF Table.pdf"
      }
    },

    content = function(file) {
      
      markdown_file <- if (input$dist == "norm" & input$norm_area == "left") {
        "std_norm_table_left.Rmd"
      } else if (input$dist == "norm" & input$norm_area == "middle") {
        "std_norm_table_middle.Rmd"
      } else if (input$dist == "norm" & input$norm_area == "right") {
        "std_norm_table_right.Rmd"
      } else if (input$dist == "t" & input$t_area == "right") {
        "t_table_right.Rmd"
      } else if (input$dist == "t" & input$t_area == "middle") {
        "t_table_middle.Rmd"
      } else if (input$dist == "chisq") {
        "chisq_table.Rmd"
      } else if (input$dist == "f" & input$alpha_f == "a10") {
        "f_table_10.Rmd"
      } else if (input$dist == "f" & input$alpha_f == "a05") {
        "f_table_05.Rmd"
      } else if (input$dist == "f" & input$alpha_f == "a025") {
        "f_table_025.Rmd"
      } else if (input$dist == "f" & input$alpha_f == "a01") {
        "f_table_01.Rmd"
      } else if (input$dist == "f" & input$alpha_f == "a001") {
        "f_table_001.Rmd"
      } else if (input$dist == "binomial" & input$binom_area == "binom_pmf") {
        if (input$binom_custom_table == FALSE) {
          "binom_table_pmf.Rmd"
        } else if (input$binom_custom_table == TRUE) {
          "binom_table_pmf_custom.Rmd"
        }
      } else if (input$dist == "binomial" & input$binom_area == "binom_cdf") {
        if (input$binom_custom_table == FALSE) {
          "binom_table_cdf.Rmd"
        } else if (input$binom_custom_table == TRUE) {
          "binom_table_cdf_custom.Rmd"
        }
      } else if (input$dist == "binomial" & input$binom_area == "binom_pmf") {
        "binom_table_pmf.Rmd"
      } else if (input$dist == "binomial" & input$binom_area == "binom_cdf") {
        "binom_table_cdf.Rmd"
      } else if (input$dist == "poisson" & input$pois_area == "pois_pmf_2") {
        "poisson_table_pmf_2.Rmd"
      } else if (input$dist == "poisson" & input$pois_area == "pois_pmf_5") {
        "poisson_table_pmf_5.Rmd"
      } else if (input$dist == "poisson" & input$pois_area == "pois_cdf_2") {
        "poisson_table_cdf_2.Rmd"
      } else if (input$dist == "poisson" & input$pois_area == "pois_cdf_5") {
        "poisson_table_cdf_5.Rmd"
      }
      
      
      src <- normalizePath(markdown_file)
      
      # temporarily switch to the temp directory, in case user doesn't have write
      #  permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, markdown_file, overwrite = TRUE)
      
      out <- render(markdown_file, pdf_document())
      
      file.rename(out, file)
    }
  )
})



