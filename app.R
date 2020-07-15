# imports

library(psychTestR)
library(htmltools)
library(shiny)
library(shinyBS)
library(shinyjs)
library(tuneR)
library(seewave)
library(hrep)
library(rjson)
library(readxl)
library(dplyr)
library(ggplot2)

## other tests
library(psychTestRCAT)
library(mpt)
library(JAJ)
library(mdt)
library(BDS)
library(piat)
library(psyquest)

# constants

simple_intervals <- c(-12:24)
test.format <- as.data.frame(read_excel("test_formats/test.xlsx"))
intro_debrief <- as.data.frame(read_excel("test_formats/intro_debrief.xlsx"))
vocal.range.factor <- 2 # i.e the number of semitones to clip each range from
intro.text <- intro_debrief[intro_debrief$label == "information_sheet", ]$text
debrief.text <- intro_debrief[intro_debrief$label == "debrief", ]$text


source("html.R")

soprano <- 60:84
alto <- 53:77
tenor <- 48:72
baritone <- 45:69
bass <- 40:64
ranges <- list("Soprano" = soprano, "Alto" = alto, "Tenor" = tenor, "Baritone" = baritone, "Bass" = bass)


# import stimuli
stimuli.abs <- readRDS("Berkowitz_Absolute.RDS")
stimuli <- lapply(stimuli.abs, diff)
berkowitz.item.bank <- readRDS("Berkowitz_Item_Bank.RDS")
berkowitz.item.bank[, c("Freq", "N", "rel.freq", "log.freq")] <- mutate_all(berkowitz.item.bank[, c("Freq", "N", "rel.freq", "log.freq")], function(x) as.numeric(as.character(x)))


# list of page types that require a user-specific starting pitch

user.starting.range.pages <- list("play_long_tone_record_audio_page", "play_interval_record_audio_page", "play_midi_file_record_audio_page", "play_melody_from_list_record_audio_page", "play_melody_record_audio_page")


# core functions

rel.to.abs.mel <- function(start_note, list_of_rel_notes) {
  # convert a relative representation of a melody to an absolute one, given a starting note
  new.mel <- cumsum(c(start_note, as.numeric(unlist(list_of_rel_notes))))
  return(new.mel)
}


get.vocal.range <- function(string_of_range) {
  
  return(ranges[[string_of_range]])
}


generate.user.range <- function(note) {
  # given a starting note, create a range for the user to present stimuli in
  range <- c(-3:3) + note
  return(range)
}

create.p.id <- function(state, ...) {
  p_id <- sprintf("p_%s",paste(base::sample(1:9, 26, replace = TRUE), collapse = ''))
  set_global("p_id", p_id, state)
  cat("p_id is", p_id, sep = "\n")
}

get.p.id <- function(state, ...) {
  p_id <- get_global("p_id", state) 
  return(p_id)
}

tidy.melody.from.corpus <- function(mel) {
  mel <- as.numeric(unlist(strsplit(mel, ",")))  
}

mean.of.stimuli <- function(rel_melody) {
  res <- round(mean(rel.to.abs.mel(0, rel_melody)))
  res
}

random.melody.from.corpus <- function() {
  mel <- tidy.melody.from.corpus(berkowitz.item.bank[sample(1:nrow(berkowitz.item.bank), 1), 1])
  mel
}


rel.to.abs.mel.mean.centred <- function(rel_melody, user_mean_note, range = NULL) {
  # produce a melody which is centered on the user's range. 
  # NB: the "mean stimuli note" could/should be sampled from around the user's mean range i.e +/- 3 semitones
  
  mean_of_stimuli <- mean.of.stimuli(rel_melody)
  
  min.range <- range[1]
  max.range <- range[length(range)]
  
  user_mean_corrected_to_stimuli <- user_mean_note - mean_of_stimuli
  stimuli_centred_to_user_mean <- rel.to.abs.mel(user_mean_corrected_to_stimuli, rel_melody)
  
  
  # the rel melody should be the same when converted back
  #print(diff(stimuli_centred_to_user_mean))
  #print(rel_melody)
  
  
  # data <- data.frame("x"=1:length(stimuli_centred_to_user_mean), "y"=stimuli_centred_to_user_mean)
  # 
  # # Plot
  # print(plot_gg <- data %>%
  #   ggplot( aes(x=x, y=y)) +
  #   geom_line() +
  #   geom_point() +
  #   geom_hline(yintercept = user_mean_note, color = "blue") +
  #   geom_hline(yintercept = user_mean_corrected_to_stimuli, color = "red", linetype="dotted") +
  #   geom_hline(yintercept = min.range, color = "green") +
  #   geom_hline(yintercept = max.range, color = "green"))
  # 
  return(stimuli_centred_to_user_mean)
  
}




rangeTest <- function() {
  
  # get random range
  vocal_range <- sample(ranges, 1)[[1]]
  print(vocal_range)
  # get random melody
  rel_melody <- random.melody.from.corpus()
  
  sampled_mean_note <- mean(vocal_range) + sample(-3:3, 1)
  
  centred.mel <- rel.to.abs.mel.mean.centred(rel_melody, sampled_mean_note, vocal_range)
  print(centred.mel)
  res <- centred.mel[centred.mel %in% intersect(centred.mel,  vocal_range)] # NB this is necessary because of duplicates
  print(res)
  no.present <- length(res)
  
  print_res <- paste0(no.present, " out of ", length(centred.mel), " present in range")
  print(print_res)
}




compute.SNR <- function(signal, noise) {
  # nice interpretation: https://reviseomatic.org/help/e-misc/Decibels.php
  signal <- env(signal, f = 44100)
  noise <- env(noise, f = 44100)
  SNR <- 20*log10(abs(rms(signal)-rms(noise))/rms(noise))
  return(SNR)
}



get_user_session_dir <- function (state) {
  
  session_dir <- paste0("output/sessions/",get_session_info(state, complete = FALSE)$p_id,"/")
  
  return (session_dir)
  
}

need.headphones <- function(answer, ...) {
  res <- suppressWarnings(answer)
  if (!is.na(res) && res == "Yes, I am wearing headphones.") TRUE
  else display_error("Sorry, you cannot complete the test unless you are using headphones.")
}

need.quiet <- function(answer, ...) {
    res <- suppressWarnings(answer)
    if (!is.na(res) && res == "Yes") TRUE
    else display_error("Sorry, you cannot complete the test unless you are in a quiet environment.")
}

need.consent <- function(answer, ...) {
  res <- suppressWarnings(answer)
  if (!is.na(res) && res == "Yes") TRUE
  else display_error("Sorry, you cannot complete the test unless you give consent to this.")
}

user_info_check <- function(input, state, ...)  {
  
  # check the info and save it including participant ID
  if (input$browser_capable == "FALSE") {
    display_error("Sorry, your browser does not have the have requirements to complete the test. Please download the latest version of Google Chrome to complete the experiment.")
  }
  
  else {
    list("user_info" = fromJSON(input$user_info),
         "p_id" = get.p.id(state),
         "sound_out_id" = get_global("soundout_id", state)
         )
  }
  
}


get.timecode <- function(input, state, getStimuli, getRhythms, ...) {
  
  # if getStimuliData == TRUE, get stimuli data
  # if getRhythms == TRUE, also get rhythm data
  
  page_answer <- list(trial.timecode = input$timecode)
  
  if (getStimuli == TRUE) {
    page_answer$playback.count <- input$playback_count
    page_answer$stimuli.pitch <- input$stimuli_pitch
    page_answer$playback.times <- input$playback_times
  }

  if (getRhythms == TRUE) {
    page_answer$stimuli.ticks <- fromJSON(input$stimuli_ticks)
    page_answer$stimuli.duration <- fromJSON(input$stimuli_duration)
    page_answer$stimuli.durationTicks <- fromJSON(input$stimuli_durationTicks)
  }
  
  return(page_answer)
  
  
}


button.text.to.choices <- function(button_text) {
  # where multiple choices are given in data file in the form "Yes/No",
  # separate into a character vector of the choiecs
  
  choices <- unlist(strsplit(button_text, "/"))

  return(choices)
}


save.range <- function(answer, state, ...) {
  set_global("user_range",answer,state)
}

get.range <- function(state, ...) {
  range <- get_global("user_range",state)
  range
}


page.builder <- function(page_type, argus) {
  
  # the first argument should be the "function" (page)
  half.built.page.list <- append(get(page_type), argus, after = 1)
  
  new.page.builder.function <- function(state, answer, ...) {
    
    # get the args generated at runtime and add them to argument list
    
    if (any(grepl(page_type, user.starting.range.pages))) {
      
      if (page_type == "play_long_tone_record_audio_page") {
        sampled_note <- get_global("sampled_note", state)
        half.built.page.list$sampled_note <- sampled_note
      }
      
      else {
        sampled_mean_note <- get_global("sampled_mean_note", state)
        half.built.page.list$sampled_mean_note <- sampled_mean_note
        
        user_range <- get.range(state)
        half.built.page.list$user_range <- user_range
      }
      
    }
    
    else { ## 
    }
    
    p.id <- get.p.id(state)
    half.built.page.list$p_id <- p.id
    
    full_fun <- as.call(half.built.page.list)
    eval(full_fun)
    
  } # end page builder function
  
  return(new.page.builder.function)
  
}


random.note.from.user.range <- function(pageb, sample_mean_note) {
  
  # prepend the page with a codeblock which does the dirty work
  
  cb <- code_block(function(state, answer, ...) {
    # a page wrapper for generating the stimuli from a random starting note (within the participants calculated range)
    
    # retrieve user range
    saved_user_range <- get_global("user_range", state)   # user_range: a range of absolute "starting" midi values
    
    # https://www.musicnotes.com/now/tips/determine-vocal-range/
    
    if (saved_user_range == "Soprano") {
      print("Soprano")
      lowest_freq <- 60 + vocal.range.factor
      highest_freq <- 84 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }
    
    else if (saved_user_range == "Alto") {
      print("Alto")
      lowest_freq <- 53 + vocal.range.factor
      highest_freq <- 77 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }
    
    else if (saved_user_range == "Tenor") {
      print("Tenor")
      lowest_freq <- 48 + vocal.range.factor
      highest_freq <- 72 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }
    
    else if (saved_user_range == "Baritone") {
      print("Baritone")
      lowest_freq <- 45 + vocal.range.factor
      highest_freq <- 69 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }
    
    else {
      print("Bass")
      lowest_freq <- 40 + vocal.range.factor
      highest_freq <- 64 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }
    
    if (sample_mean_note == TRUE) {
      sampled_mean_note <- mean(range) + sample(-3:3, 1)
      set_global("sampled_mean_note",sampled_mean_note, state)
    }
    
    else {
      sampled_note <- sample(range, 1)
      set_global("sampled_note",sampled_note, state)
    }
      
  }) # end code block
  
  # then wrap in a reactive page
  page <- reactive_page(pageb) # end reactive page

  both <- list(cb,page)
  
  return(both)
  
  
} # end main function


item.sampler <- function(item_bank, no_samples) {
  
  no_samples <- as.numeric(no_samples)
  max.N <- max(item_bank$N)
  
  sample <- item_bank[1, ]
  
  count <- 0
  
  while(nrow(sample)-2 < no_samples) {
      
    N.subset <- item_bank[item_bank[, "N"] == count, ]
    rand.samp.i <- sample(1:nrow(N.subset), 1, replace = FALSE)
    rand.samp <- N.subset[rand.samp.i, ]
    sample <- rbind(sample, rand.samp)
    
    count <- count + 1

    if (count == max.N) { 
      count <- 0
      }
  }
  
  sample <- sample[-1, ] # remove the first dummy row
  sample <- sample[order(sample$N), ] # order by N
  sample <- sample[complete.cases(sample), ]
  row.names(sample) <- 1:nrow(sample)
  
  sample
  
}



create.pages <- function(items, page_type, page_pars) {
  
  # create a bunch of psychTestR pages from a list of items
  
  tl <- c()

  for (i in 1:nrow(items)) {
    
    rel_melody <- as.numeric(as.vector(unlist(strsplit(items[i, "melody"], ","))))
    
    page_pars$label <- paste0(page_type,"_", i)
    page_pars$stimuli_no <- NULL
    
    if (i == 1) {
      page_pars$body <- page_pars$text
      page_pars$text <- NULL
    }
    
    else {
      ##
    }
    
 
    
    if (any(grepl(page_type, user.starting.range.pages))) {
      
      if (page_type == "play_long_tone_record_audio_page") {
        page.builder.fun <- page.builder(page_type, page_pars)
        page <- random.note.from.user.range(page.builder.fun, FALSE)
      }
      
      else {
        page_pars$rel_melody <- rel_melody
        page.builder.fun <- page.builder(page_type, page_pars)
        page <- random.note.from.user.range(page.builder.fun, TRUE)
      }
    }
      
    else { 
      ##  
    }
    
    tl <- append(tl, page, after = length(tl)) 
    tl <- append(tl, eval(parse(text="elt_save_results_to_disk(complete = FALSE)")), after = length(tl))
    
  } # end for loop
  
  tl
}

sample.random.stimuli.no <- function(page_type, no_in_corpus, no_to_select, page_pars) {
  
  range_of_lengths <- seq(4, 4+no_to_select) # 4 notes minimum

  samp <- sample(1:no_in_corpus, no_to_select)
  
  tl <- c()
  
  for (i in 1:no_to_select) {
    
    page_pars$label <- paste0(page_type,"_", i)
    page_pars$stimuli_no <- samp[[i]]
    page_pars$note_no <- range_of_lengths[i]
    
    if (i == 1) {
      page_pars$body <- page_pars$text
      page_pars$text <- NULL
    } else { }
    
    page.builder.fun <- page.builder(page_type, page_pars)
    page <- random.note.from.user.range(page.builder.fun, TRUE)
    tl <- append(tl, page, after = length(tl)) 
    tl <- append(tl, eval(parse(text="elt_save_results_to_disk(complete = FALSE)")), after = length(tl))
    
  } # end for loop
  
  tl
  
}



create.test <- function(test_format, item_bank) {
  
  # main test builder from excel file
  # test_format: excel file specifying test format
  # with_final: should there be a final page? TRUE if so.
  
  # change NULLS to NAs (better for R to handle)
  test_format <- test_format %>% replace(.=="NULL", NA) # replace with NA
  
  tl <- c() # init empty list
  
  count <- 1
  
  for (row in 1:nrow(test_format)) {
    
    page_info <- as.list(test_format[row, ])
    
    # remove from list if no value
    page_info <- page_info[!is.na(page_info)]
    
    page_type <- page_info$page_type
    
    
    # these pages just do behind the scenes work. no processing needed other than making them pages
    if (page_type == "calculate.SNR.page") {
      page <- calculate.SNR.page
      tl <- append(tl, page, after = length(tl))
    } 
    
    else if (page_type == "calculate.range.page") {
      page <- calculate.range.page
      tl <- append(tl, page, after = length(tl))
    }
    
    else if (page_type == "sound_out_p_id") {
      page.fun <- get("page", asNamespace("psychTestR"))
      page <- do.call(page.fun, list(on_complete = as.name(page_info$on_complete), 
                                 ui = div(htmltools::HTML(page_info$text),
                                          trigger_button("next", page_info$button_text)
                                          )
                                 )
                      )
      
      tl <- append(tl, psychTestR::code_block(function(state, ...) {
        soundout_id <- psychTestR::get_url_params(state)$soundout_id
        if (!is.null(soundout_id)) {
          set_global("soundout_id", soundout_id, state)
        }
      }) , after = length(tl))
      tl <- append(tl, page, after = length(tl))
    }
    
    else {  # pages that need parameters
      
      page_pars <- page_info
      page_pars$page_type <- NULL # delete page_type from the page_pars list
      page_pars$text <- htmltools::HTML(page_pars$text)
      
      
      # if _GENERATE.ITEMS_ keyword found then generate a bunch of trials according to the page time 
      
      if (page_info$label == "_GENERATE.ITEMS_") {
        if (page_type == "play_melody_record_audio_page") {
          user.sample <- item.sampler(item_bank, page_info$stimuli_no)
          pages <- create.pages(user.sample, page_type, page_pars)
          tl <- append(tl, pages, after = length(tl))
        }
        else if (page_type == "play_midi_file_record_audio_page") {
          pages <- sample.random.stimuli.no(page_type, 629, page_info$stimuli_no, page_pars)
          tl <- append(tl, pages, after = length(tl))
        }
        
        else if (page_type == "play_long_tone_record_audio_page") {
          user.sample <- item.sampler(item_bank, page_info$stimuli_no)
          pages <- create.pages(user.sample, page_type, page_pars)
          tl <- append(tl, pages, after = length(tl))
        }
        else {}
        
      } 
      else {
        # i.e manually create each page as per the row in the excel file
        if (page_type == "final_page") {  # final page has a different pattern with regards to storing results
          page_pars$label <- NULL
          page_pars$save_answer <- NULL
          tl <- append(tl, eval(parse(text="elt_save_results_to_disk(complete = TRUE)")), after = length(tl)) # BEFORE final page
          page <- do.call(final_page, list(body = page_pars$text))
          tl <- append(tl, page, after = length(tl))
        } 
        
        else {
          
          # if the on_complete answer is present, make sure to render the string as a function
          
          if (!is.null(page_pars$on_complete)) {
            page_pars$on_complete <- get(page_pars$on_complete)
          } else { } # do nothing
          
          
          
          # change name of "text" argument correspondingly (different pages have different argument names for body text)
          
          if (page_type == "NAFC_page" || page_type == "volume_calibration_page" || page_type == "text_input_page") {
            # rename "text" argument to prompt
            page_pars$prompt <- page_pars$text
            page_pars$text <- NULL 
          }
          
          else {
            # rename "text" argument to body
            page_pars$body <- page_pars$text
            page_pars$text <- NULL 
          }
          
          # remove the label and save_answer arguments (these pages do not accept them)
          
          if (page_type == "one_button_page" || page_type == "volume_calibration_page") {
            
            page_pars$label <- NULL
            page_pars$save_answer <- NULL
            
          }  else { } # do nothing 
          
          
          # for NAFC_page, convert button text to choices
          # and update the argument name
          
          if (page_type == "NAFC_page") {
            page_pars$choices <- button.text.to.choices(page_pars$button_text)
            page_pars$button_text <- NULL
            page_pars$arrange_vertically <- FALSE
          } else {} # do nothing
          
          # generate the pages
          if (any(grepl(page_type, user.starting.range.pages))) { # this condition finds which pages require a start_note argument
            
            page.builder.fun <- page.builder(page_type, page_pars)
            
            if (page_type == "play_long_tone_record_audio_page") {
              page <- random.note.from.user.range(page.builder.fun, FALSE)
            }
            
            else {
              page <- random.note.from.user.range(page.builder.fun, TRUE)
            }
            
            tl <- append(tl, page, after = length(tl)) # add to timeline. 
            tl <- append(tl, eval(parse(text="elt_save_results_to_disk(complete = FALSE)")), after = length(tl)) # AFTER page
          }
          
          else if (any(grepl(page_type, c("record_background_page", "record_5_second_hum_page")))) { # or another page that collects audio, but doesn't need a start note (i.e these pages need a p_id)
            
            page.builder.fun <- page.builder(page_type, page_pars)
            
            page <- reactive_page(page.builder.fun) # end reactive page
            
            tl <- append(tl, page, after = length(tl)) # add to timeline. these pages have the results message already appended
            tl <- append(tl, eval(parse(text="elt_save_results_to_disk(complete = FALSE)")), after = length(tl)) # AFTER page
          }
          
          else {
            
            # evaluate the pages that don't need a sampled_note argument
            page <- do.call(page_type, page_pars)
            
            if (!is.null(page_pars$save_answer)) {
              
              if (page_pars$save_answer == TRUE) {
                
                tl <- append(tl, page, after = length(tl))
                tl <- append(tl, eval(parse(text="elt_save_results_to_disk(complete = FALSE)")), after = length(tl)) # AFTER page
              }
              
              else { # if it's false, still append, but without the save to results message
                tl <- append(tl, page, after = length(tl))
              }
              
            } # end if !is.null(page_pars) 
            else {
              tl <- append(tl, page, after = length(tl)) # also still append the pages that had no save_answer arg at all
            } # the respective else of is.null!(page_pars)
            
          } # end else for any(grepl(page_type, user.starting.range.pages))
          
        } # end else for if (page_type == "final_page")
        
      } # end else of if page_info$label == "_GENERATE.ITEMS_"
      
    } # end pages that need parameters else
    
  } # end main for loop
  
  tl <- psychTestR::module("MST", as.list(tl))
  
  return(tl)
  
}
## main test pages

#### setup pages ####


microphone_calibration_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, button_text = "Next", save_answer = FALSE) {
  
  ui <- div(
    shiny::tags$style('._hidden { display: none;}'), # to hide textInputs
    includeScript("www/js/Tone.js"),
    includeScript("www/js/audio_display.js"),
    includeScript("www/js/speech.js"),

    # start body
    
    
    body,
    
    
    img(id = "record",
        src = "img/mic128.png",
        onclick = "toggleRecording(this);getMedia();",
        style = "display:block; margin:1px auto;", width = "100px", height = "100px"),
    
    # 
    # helpText("Click on the microphone to test"),
     hr(),
    # helpText("Make sure your microphone levels are not too high when you speak or sing. Turn your microphone volume down if so."),
    # helpText("If you see that the levels are moving a lot when you are sitting quietly, your room may be too noisy to complete the test."),
    # hr(),
    div(id = "viz",
        tags$canvas(id = "analyser"),
        tags$canvas(id = "wavedisplay")
    ),
    br(),
    trigger_button("next", button_text),
    hr()
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = FALSE)
}


get_user_info_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, button_text = "Give Browser Info", save_answer = TRUE) {
  
  
  ui <- div(
    
    html.head, # end head
    
    # start body
    
    body,
    
    div(shiny::tags$input(id = "user_info"), class="_hidden"
    ),
    
    shiny::tags$button(button_text, id="getUserInfoButton", onclick="getUserInfo();testFeatureCapability();next_page();"),
    #br(),
    #hr(),
    #helpText("Click here to view more information about your privacy"),
    #hr()
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = user_info_check)
}


record_background_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, save_answer = TRUE, body = NULL, button_text = "Next", p_id) {
  
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  
  print(p_id)
  
  ui <- div(
    
    html.head
    
    , # end head
    
    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton",
                                       onclick="recordAndStop(5000, false, true, this.id, 'piano');")
                    
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = FALSE, getRhythms =  FALSE) } )
  
}


record_5_second_hum_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, save_answer = TRUE, button_text = "Next", p_id) {
  
  # a page type for recording a 5-second user hum to compute signal-to-noise ratio (SNR)
  
  
  ui <- div(
    
    html.head,
    
    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick="recordAndStop(5000, false, true, this.id, 'piano');")
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer2
  
  ) # end main div
  
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = FALSE, getRhythms =  FALSE) } )
  
}


video_page <- function(admin_ui = NULL, on_complete = NULL, label = NULL, url = NULL, body = NULL, button_text = "Next", save_answer = FALSE, video = NULL) {
  
  # page for presenting popup videos. TBC later
  
  #video_html <- htmltools::HTML(paste0(body,video.popup.html.1,src,video.popup.html.2))
  
  vid <- htmltools::HTML(paste0("<video controls width=\"640px\", height=\"350px\">
  <source muted=\"false\", src=\"", url,"\" type = \"video/mp4\">
  Sorry, your browser doesn't support embedded videos.
  </video>"))
  
  # NB to autoplay videos, there should be NO autoplay argument present and also the muted att is required for chrome
  
    ui <- div(
    
    html.head, # end head
    
    # start body
    
    body,
    
    vid,
    
    
    br(),
    
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = FALSE)
}




#### main test pages ####



play_long_tone_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
                                             save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
                                             note_no = "max", interval = NULL, sampled_note = NULL, p_id, ...) {
  
  # a page type for playing a 5-second tone and recording a user singing with it
  
  # The arguments must be in this order: 
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, sampled_note, ...
  
  
  # user_range_index: which index of the user's stored range should be used for the long tone

  
  tone.for.js <- sampled_note
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##
    
    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("console.log(this.id);playTone(%s, 5, this.id, 'tone');", tone.for.js))
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer2
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  FALSE) })
  
} 







play_interval_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
                                            save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
                                            note_no = 2, sampled_mean_note = NULL, p_id, ...) {
  
  # The arguments must be in this order: label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  #save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, sampled_mean_note
  
  # a page type for playing a single interval, recording user audio response and saving as a file

  interval <- rel.to.abs.mel.mean.centred(simple_intervals[stimuli_no], sampled_mean_note)
  
  interval.for.js <- toString(interval)
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##
    
    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], true, this.id, 'piano');", interval.for.js))
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer2
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  FALSE) })
  
}



# create a page type for playing back midi files

play_midi_file_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
                      save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
                      note_no = "max", interval = NULL, sampled_mean_note = NULL, p_id, ...) {
  
  # The first arguments must be in this order: 
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, sampled_mean_note, ...
  
  # note_no. optionally limit number of notes
  
  # work out the difference between the first note of the stimuli and the user starting note to calculate no. of semitones to transpose
  
  melody <- rel.to.abs.mel.mean.centred(stimuli[stimuli_no], sampled_mean_note)
  
  transpose <- diff(c(melody[1], stimuli.abs[[stimuli_no]][1]))
  
  #cat("transpose by: ", -(transpose), sep="\n")
  
  if (is.null(note_no) == TRUE) {
    note_no <- "\"max\""
  }
  
  dir_of_midi <- "berkowitz_midi_rhythmic/"
  
  url <- paste0(dir_of_midi,"Berkowitz",stimuli_no,".mid")
  
  ui <- div(
    
    html.head,
    
    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##
    
    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),
    
    # start body
    body,
    
    shiny::tags$div(id="button_area",
          shiny::tags$button(button_text, id="playButton", 
          onclick=shiny::HTML(paste0("playMidiFileAndRecordAfter(\"",url,"\", true, ",note_no,", true, this.id, ",transpose,", 'piano')")))
    ),
    
    shiny::tags$div(id="loading_area"),
    html.footer2
  )
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  TRUE) })
}




play_melody_from_list_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
          save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
          note_no = "max", interval = NULL, sampled_mean_note = NULL, p_id, ...) {
 
  # The arguments must be in this order: 
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, sampled_mean_note, ...
  
  # a page type for playing a melody, recording user audio response and saving as a file

  if (note_no == "max") {
    note_no <- length(stimuli[[stimuli_no]])
  }
  
  rel_melody <- stimuli[[stimuli_no]][0:note_no]
  
  melody <- rel.to.abs.mel.mean.centred(rel_melody, sampled_mean_note)
  
  mel.for.js <- toString(melody)
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##
    
    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], true, this.id, 'piano');", mel.for.js))
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer2
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  TRUE) })
  
}



play_melody_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
                                                    save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no = NULL, 
                                                    note_no = "max", interval = NULL, sampled_mean_note = NULL, p_id, rel_melody = NULL, user_range = NULL, ...) {
  
  # The arguments must be in this order: 
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, sampled_mean_note, ...
  
  # a page type for playing a melody, recording user audio response and saving as a file
  
  cat("sampled_mean_note in play_melody_record_audio_page", sampled_mean_note)
 
  
  melody <- rel.to.abs.mel.mean.centred(rel_melody, sampled_mean_note, get.vocal.range(user_range))
  cat("melody in play_melody_record_audio_page", melody)
  
  mel.for.js <- toString(melody)
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##
    
    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], false, this.id, 'piano');", mel.for.js))
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer2
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  FALSE) })
  
}


# create the test based on the excel file
  
MST <- create.test(test.format, berkowitz.item.bank)

# constants for all tests

test.password <- "ilikecheesepie432"
num_items <- 10


###

# import PDCT
source("PDCT/app.R")

### Study 2. Full Battery ###

deploy.battery <- function(password) {
  
test <- psychTestR::join(one_button_page(body = htmltools::HTML(intro.text)),
                           NAFC_page(label = "consent_1", prompt = htmltools::HTML(intro_debrief[intro_debrief$label == "consent_1", ]$text), choices = c("Yes", "No"), on_complete = need.consent),
                           NAFC_page(label = "consent_2", prompt = htmltools::HTML(intro_debrief[intro_debrief$label == "consent_2", ]$text), choices = c("Yes", "No"), on_complete = need.consent),
                           NAFC_page(label = "consent_3", prompt = htmltools::HTML(intro_debrief[intro_debrief$label == "consent_3", ]$text), choices = c("Yes", "No"), on_complete = need.consent),
                           NAFC_page(label = "consent_4", prompt = htmltools::HTML(intro_debrief[intro_debrief$label == "consent_4", ]$text), choices = c("Yes", "No"), on_complete = need.consent),
                           NAFC_page(label = "consent_5", prompt = htmltools::HTML(intro_debrief[intro_debrief$label == "consent_5", ]$text), choices = c("Yes", "No"), on_complete = need.consent),
                           one_button_page("In this study you will complete 6 short tests and 1 questionnaire. 
                                           Please make sure you complete all the tasks before you close your browser!"),
                           MST,
                          PDCT(label = "PDCT", num_items = 15L),
                           piat(label = "PIAT", num_items = 15L),
                           mdt(label = "MDT", num_items = 11L),
                           mpt(label = "MPT", num_items = 15L),
                          JAJ(label = "JAJ", num_items = 8L), 
                           GMS(admin_password = password, 
                            subscales = c("Musical Training", "Singing Abilities")),
                           elt_save_results_to_disk(complete = TRUE), 
                           one_button_page(body = htmltools::HTML(debrief.text)),
                           final_page("The End.")
  )
}

study.2.battery <- deploy.battery(password = test.password)

# # # run the test
test <- make_test(
  elts = study.2.battery,
  opt = test_options("Melody Singing", test.password,
                     display = display_options(
                       css = "style.css")
  )
)

#shiny::runApp(".")
#rsconnect::deployApp('/Users/sebsilas/mel_production_aws_study_2')