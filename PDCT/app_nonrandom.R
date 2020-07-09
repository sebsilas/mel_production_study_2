library(shiny)
library(psychTestR)
library(psychTestRCAT)
library(htmltools)

item.bank <- read.csv("ItemBank.csv")


show.item <- function(item, state, ...) {

  item_number <- psychTestRCAT::get_item_number(item)
  
  tones <- sprintf("playTones([%s, %s, %s]);", item["tone.1"], item["tone.2"], item["tone.3"])
  

  NAFC_page(label = paste0("q", item_number), 
             prompt = div(includeScript("www/js/Tone.js"),
                          includeScript("www/js/main.js"),
                          shiny::tags$script(tones),
                          shiny::tags$p("Click the odd one out.")),
             choices = c("1","2","3"),
            arrange_vertically = FALSE
             )
  
}

timeline <- psychTestR::join(
        one_button_page(shiny::tags$h4("Hello, welcome to the Pitch Discrimination Task!")),
        one_button_page(div(shiny::tags$p("In this task, on each trial you will hear three tones."),
                        shiny::tags$p("One will be higher than the other two."),
                        shiny::tags$p("You must click which tone you think is the odd one out (the highest tone)."))),
        one_button_page(shiny::tags$h4("Let's begin!")),
        
        
        psychTestRCAT::adapt_test(label = "PDCT", 
        item_bank = item.bank,
        show_item = show.item,
        stopping_rule = stopping_rule.num_items(n = 10),
        opt = adapt_test_options(next_item.criterion = "bOpt",
                               next_item.estimator = "BM",
                                final_ability.estimator = "WL",
                                  eligible_first_items = c(97,98,99)
                               )),
        # save results
        elt_save_results_to_disk(complete = TRUE),
        # final page
        final_page("The End!")
        )


PDCT.test <- make_test(elts = timeline)

runApp(PDCT.test)

#rsconnect::deployApp('/Users/sebsilas/Desktop/Melody Production Dissertation/Pitch-Discrimination-Task/task')

# c(next_item.prior_dist = "norm",  
# next_item.prior_par = c(0, 1), 
# constrain_answers = FALSE, 
# avoid_duplicates = NULL,
# cb_control = NULL, cb_group = NULL,
# notify_duration = 5)


