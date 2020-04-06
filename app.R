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

# constants

midi_notes <- c(40:84)
freq_notes <- lapply(midi_notes, midi_to_freq)
lowest_freq <- midi_to_freq(midi_notes[1])
highest_freq <- midi_to_freq(midi_notes[length(midi_notes)])
freq_range <- c(lowest_freq, highest_freq)
simple_intervals <- c(-12:24)
test_data <- as.data.frame(read_excel("test.xlsx"))


# import stimuli as relative midi notes
stimuli <- readRDS("Berkowitz_midi_relative.RDS")


# list of page types that require a user-specific starting pitch

user.starting.range.pages <- list("play_long_tone_record_audio_page", "play_interval_record_audio_page", "play_midi_file_record_audio_page", "play_melody_from_list_record_audio_page")

# list of page types that don't return audio

non.audio.pages <- list("get_user_info", "microphone_test", "present_files", "quiet_question", "headphones_question")



# html header

html.head <- shiny::tags$head(shiny::tags$script(htmltools::HTML(
  '// Create the XHR object.
function createCORSRequest(method, url) {
var xhr = new XMLHttpRequest();
if ("withCredentials" in xhr) {
// XHR for Chrome/Firefox/Opera/Safari.
xhr.open(method, url, true);
} else if (typeof XDomainRequest != "undefined") {
// XDomainRequest for IE.
xhr = new XDomainRequest();
xhr.open(method, url);
} else {
// CORS not supported.
xhr = null;
}
return xhr;
}
// Helper method to parse the title tag from the response.
function getTitle(text) {
return text.match(\'<title>(.*)?</title>\')[1];
}
// Make the actual CORS request.
function makeCorsRequest() {
// This is a sample server that supports CORS.
var url = \'https://www.midijs.net/lib/midi.js\';
var xhr = createCORSRequest(\'GET\', url);
if (!xhr) {
alert(\'CORS not supported\');
return;
}
// Response handlers.
xhr.onload = function() {
var text = xhr.responseText;
var title = getTitle(text);
alert(\'Response from CORS request to \' + url + \': \' + title);
};
xhr.onerror = function() {
alert(\'Woops, there was an error making the request.\');
};
xhr.send();
}')),
  shiny::tags$script(htmltools::HTML(
    '// Create the XHR object.
function createCORSRequest(method, url) {
var xhr = new XMLHttpRequest();
if ("withCredentials" in xhr) {
// XHR for Chrome/Firefox/Opera/Safari.
xhr.open(method, url, true);
} else if (typeof XDomainRequest != "undefined") {
// XDomainRequest for IE.
xhr = new XDomainRequest();
xhr.open(method, url);
} else {
// CORS not supported.
xhr = null;
}
return xhr;
}
// Helper method to parse the title tag from the response.
function getTitle(text) {
return text.match(\'<title>(.*)?</title>\')[1];
}
// Make the actual CORS request.
function makeCorsRequest() {
// This is a sample server that supports CORS.
var url = \'https://eartrainer.app\';
var xhr = createCORSRequest(\'GET\', url);
if (!xhr) {
alert(\'CORS not supported\');
return;
}
// Response handlers.
xhr.onload = function() {
var text = xhr.responseText;
var title = getTitle(text);
alert(\'Response from CORS request to \' + url + \': \' + title);
};
xhr.onerror = function() {
alert(\'Woops, there was an error making the request.\');
};
xhr.send();
}
')),
  shiny::tags$style('._hidden { display: none;}'), # to hide textInputs
  #includeScript("www/js/midi.js"),
  shiny::tags$script(src="https://www.midijs.net/lib/midi.js"),
  shiny::tags$script(src="https://unpkg.com/@tonejs/midi"),
  includeScript("www/js/Tone.js"), # not this
  includeScript("www/js/main.js"), # haven't checked yet
  includeScript("www/js/speech.js"), # here
  includeScript("www/js/audiodisplay.js"),
  includeScript("www/js/modernizr-custom.js"),
  shiny::tags$script(htmltools::HTML('initAudio();// get audio context going'))
  )



# potentially for pop-up video later
video.popup.html.1 <- NULL
video.popup.html.2 <- NULL


# core functions

rel.to.abs.mel <- function(start_note, list_of_rel_notes) {
  # convert a relative representation of a melody to an absolute one, given a starting note
  new.mel <- cumsum(c(start_note, as.numeric(unlist(list_of_rel_notes))))
  return(new.mel)
}




generate.user.range <- function(note) {
  # given a starting note, create a range for the user to present stimuli in
  range <- c(-5:5) + note
  return(range)
}



compute.SNR <- function(signal, noise) {
  # nice interpretation: https://reviseomatic.org/help/e-misc/Decibels.php
  signal <- env(signal, f = 44100)
  noise <- env(noise, f = 44100)
  SNR <- 20*log10(abs(rms(signal)-rms(noise))/rms(noise))
  return(SNR)
}




get_user_session_dir <- function (state) {
  
  #local_path <- getwd() # remove eventually
  
  session_dir <- paste0("output/sessions/",get_session_info(state, complete = FALSE)$p_id,"/")
  
  #session_dir <- paste0(local_path, "/", session_dir)
  
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

prepare.then.write.audio <- function(audio_data, file_name, state) {
  
  ## split two channel audio
  audio_split <- length(audio_data)/2
  
  a1 <- audio_data[1:audio_split]
  
  a2 <- audio_data[(audio_split+1):length(audio_data)]
  
  # construct wav object that the API likes
  Wobj <- Wave(a1, a2, samp.rate = 44100, bit = 16)
  Wobj <- normalize(Wobj, unit = "16", pcm = TRUE)
  Wobj <- mono(Wobj)
  
  # writing the file to the www directory
  
  wav_www_dir <- paste0("www/trial_audio/",file_name)
  writeWave(Wobj, wav_www_dir, extensible = FALSE)
  
  # session-specific writing
  session_dir <- get_user_session_dir(state)
  
  wav_name_sess_dir <- paste0(session_dir,file_name)
  
  
  writeWave(Wobj, wav_name_sess_dir, extensible = FALSE)
  
}


user_info_check <- function(input, ...)  {
  
  if (input$browser_capable == "FALSE") {
    display_error("Sorry, your browser does not have the have requirements to complete the test. Please download the latest version of Google Chrome to complete the experiment.")
  }
  
  else {
    list(user_info = fromJSON(input$user_info))
  }
  
}


get.answer.grab.audio <- function(input, state, ...) {
  
  # audio
  audio.data <- input$audio
  
  # get timecode
  tc = Sys.time()
  
  # create filename for the audio file based on time code
  trial.filename =  paste0(gsub("[^0-9]","",tc), ".wav")
  
  # write audio
  prepare.then.write.audio(audio.data, trial.filename, state = state)
  
  # playback count
  playback_count <- input$playbackCount
  
  list(trial.id = NULL, # should be page label
       trial.timecode = tc,
       trial.filename =  trial.filename,
       playback.count = playback_count
  )
  
}


button.text.to.choices <- function(button_text) {
  # where multiple choices are given in data file in the form "Yes/No",
  # separate into a character vector of the choiecs
  
  choices <- unlist(strsplit(button_text, "/"))
  
  str_c <- ""
  count <- 0
  
  for (w in choices) {
    
    if (count == 0) {
      str_c <- paste0(str_c,"\"",w,"\", ")
    }
    
    else {
      str_c <- paste0(str_c,"\"",w,"\"")
    }
    count <- count + 1
  }
  
  str_c <- paste0("c(",str_c,")")
  
  res <- eval(parse(text=str_c))

  return(res)
}


page.builder <- function(page_type, argus) {
  
  com <-  append(page_type, argus, after = 1)
  
  half.built.page <<- as.call(com)
  
  half.built.page.list <- com
  
  new.page.builder.function <- function(state, answer, ...) {
    
    start_note <- get_global("start_note", state)
    
    half.built.page.list$start_note <- start_note
    
    full_fun <- as.call(half.built.page.list)
    
    eval(full_fun)
    
  } 
  
  return(new.page.builder.function)
  
}


random.note.from.user.range <- function(pageb) {
  
  # prepend the page with a codeblock which does the dirty work
  
  cb <- code_block(function(state, answer, ...) {
    # a page wrapper for generating the stimuli from a random starting note (within the participants calculated range)
    
    # retrieve user range
    saved_user_range <- get_global("user_range", state)   # user_range: a range of absolute "starting" midi values
    next_start_note <- sample(saved_user_range, 1) # sample one
    set_global("start_note",next_start_note, state)
    
    
  }) # end code block
  
  
  cat("pageb", str(pageb))
  cat("pageb class", class(pageb))
  cat("pageb class eval", class(eval(pageb)))
  
  # then wrap in a reactive page
  page <- reactive_page(pageb) # end reactive page

  both <- list(cb,page)
  
  return(both)
  
  
} # end main function



calculate.range.page <- reactive_page(function(state, answer, ...) {
  
  # a "hidden" page

  res <- as.list(results(state))
  
  # get user session directory
  session_dir <- get_user_session_dir(state)
  
  user_singing_calibration_file_name <- res$results$user_singing_calibration$trial.filename
  user_singing_calibration <- readWave(paste0(session_dir,user_singing_calibration_file_name))
  
  
  # calculating periodograms of sections each consisting of 1024 observations,
  # overlapping by 512 observations:
  WspecObject <- periodogram(user_singing_calibration, width = 1024, overlap = 512)
  
  # calculate the fundamental frequency:
  ff <- tuneR::FF(WspecObject, peakheight=0.015) #tuneR solution: issue, no bandpass try to get below working
  
  #ff <- seewave::autoc(user_singing_calibration, f = 44100, fmin = round(lowest_freq), fmax = round(highest_freq), plot = FALSE, xlab = "Time (s)", ylab = "Frequency (kHz)", ylim = c(0, f/44100), threshold = 2) # NB, also threshold argument
  #df <- seewave::dfreq(user_singing_calibration , clip = 0.1, threshold = 10, wl=87.5, bandpass = c(round(lowest_freq), round(highest_freq)))
  #print(ff)
  
  # mean ff
  user.mean.FF <- round(mean(ff, na.rm = TRUE), 2)
  user.mean.midi <- round(freq_to_midi(user.mean.FF))
  
  # mean df
  #user.mean.DF <- round(mean(df, na.rm = TRUE), 2) * 1000
  #user.mean.DF.midi <- round(freq_to_midi(user.mean.DF))
  
  # define a user range
  
  user.range <- generate.user.range(user.mean.midi)
  
  set_global("user_mean_midi", user.mean.midi, state)
  
  user.range <- generate.user.range(user.mean.midi)
  set_global("user_range", user.range, state)
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    
    #renderPlot({plot(ff)}), # optional: plotenergy = FALSE for tuneR
    
    #renderPlot({plot(df)}), # optional: plotenergy = FALSE
    
    renderText({sprintf("The mean FF was %.2fHZ / MIDI note: %i", user.mean.FF, user.mean.midi)}), # mean FF
    
    renderText({"This page will not be presented to the user"}), # mean midi note
    
    
    #renderText({sprintf("The mean DF was %.2f", user.mean.DF)}), # mean DF
    
    #renderText({sprintf("The mean MIDI note was %i", user.mean.DF.midi)}), # mean midi note
    
    
    # next page
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, get_answer = function(input, ...) toString(input$user.range))
  
})



### diagnostic pages for admin testing ###


present_files_page <- function(state, admin_ui = NULL, on_complete = NULL, label= NULL) {
  
  # NB this page is only used locally for testing because you can only playback audio files in the /www/ directory. 
  # In the real test, online, we would only store files in the user session directory
  
  # get list of (all) page titles
  
  res <- as.list(results(state))$results
  
  page_titles <- names(as.list(results(state))$results)
  
  
  # create a list of pages that return audio
  
  audio_pages <- c()
  
  for (t in page_titles) {
    
    if (t %in% non.audio.pages) {
      # do nothing
    }
    else {
      audio_pages <- c(audio_pages, t)
    }
    
  }
  
  # now get list of file names
  
  html.file.list <- list() # instantiate empty string
  
  count <- 1
  count_4 <- 1
  
  for (page in audio_pages) {
    
    
    file_name <- eval(parse(text=paste0("res$",page,"$trial.filename")))
    path <- paste0("trial_audio/",file_name)
    
    new.title <- tags$p(page)
    
    new.tag <- tags$audio(src = path, type = "audio/wav", autoplay = FALSE, controls = TRUE)
    
    html.file.list[[count_4]] <- new.title
    html.file.list[[count_4+1]] <- br()
    html.file.list[[count_4+2]] <- new.tag
    html.file.list[[count_4+3]] <- br()
    
    count <- count+1
    count_4 <- count_4+4
    
  }
  
  html.file.list <- tagList(html.file.list)
  
  
  ui <- div(
    
    html.head, # end head
    
    # start body
    
    html.file.list,
    
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = FALSE)
}


calculate.SNR.page <- reactive_page(function(state, ...) {
  
  # a "hidden" page
  
  res <- as.list(results(state))
  
  # get user session directory
  session_dir <- get_user_session_dir(state)
  
  user_background_file_name <- res$results$user_background$trial.filename
  user_hum_file_name <- res$results$user_hum$trial.filename
  
  # first for user background
  user_background <- readWave(paste0(session_dir,user_background_file_name))
  # same thing for note sing
  user_hum <- readWave(paste0(session_dir,user_hum_file_name))
  
  # compute signal-to-noise ratio
  SNR <- compute.SNR(user_hum,user_background)
  
  print(SNR)
  
  if (as.numeric(SNR) < 4) {
    display_error("Sorry, your signal is too noisy. Please try making your environment less noisy and/or your microphone signal better to complete the test.")
  }
  
  #periodograms
  userbgWspecObject <- tuneR::periodogram(user_background, width = 1024, overlap = 512, fastdisp = TRUE)
  userhumWspecObject <- tuneR::periodogram(user_hum, width = 1024, overlap = 512, fastdisp = TRUE)
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    renderText({paste0("Your signal-to-noise ratio was: ",round(SNR,2))}),
    
    renderText({"This page will not be presented to the participant."}),
    renderText({"Currently the SNR must be > 4 to proceed."}),
    
    
    #renderText({"User Background"}), # optional: plotenergy = FALSE
    
    #renderPlot({plot(userbgWspecObject)}, width = 100, height = 50), # optional: plotenergy = FALSE
    
    #renderText({"User Hum"}), # optional: plotenergy = FALSE
    
    #renderPlot({plot(userhumWspecObject)}, width = 100, height = 50), # optional: plotenergy = FALSE
    
    
    # next page
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui)
  
})


create.test <- function(data) {
  
  # main test builder from excel file
  # data: excel file
  
  # change NULLS to NAs (better for R to handle)
  data <- data %>% replace(.=="NULL", NA) # replace with NA
  
  tl <- c() # init empty list
  
  count <- 1
  
  for (row in 1:nrow(data)) {
    
    page_info <- as.list(data[row, ])
    
    # remove from list if no value
    page_info <- page_info[!is.na(page_info)]
    
    page_type <- page_info$page_type
    
    #print(page_pars)
    
    # these pages just do behind the scenes work. no processing needed other than making them pages
    if (page_type == "calculate.SNR.page") {
      page <- calculate.SNR.page
      tl <- append(tl, page, after = length(tl))
    } 
    
    else if (page_type == "calculate.range.page") {
      page <- calculate.range.page
      tl <- append(tl, page, after = length(tl))
    }
    
    
    else {  # pages that need parameters
      
      page_pars <- page_info
      page_pars$page_type <- NULL # delete page_type from the page_pars list
      page_pars$text <- htmltools::HTML(page_pars$text)
      
    # final page has a different pattern with regards to storing results
    
      if (page_type == "final_page") {
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
        } else {} # do nothing
        
        
        
        
        # generate the pages
        if (any(grepl(page_type, user.starting.range.pages))) { # this condition finds which pages require a start_note argument
          
          page.builder.fun <- page.builder(get(page_type), page_pars)
          
          
          page <- random.note.from.user.range(page.builder.fun)
          
          tl <- append(tl, page, after = length(tl)) # add to timeline. these pages have the results message already appended
        }
        
        else {
          
          # evaluate the pages that don't need a start_note argument
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
      
        } # end else forany(grepl(page_type, user.starting.range.pages))
      
      } # end else for if (page_type == "final_page")
      
    } # end pages that need parameters else
    
  } # end main for loop
  
  tl <- as.list(tl)
  
  return(tl)
  
}
## main test pages

#### setup pages ####


microphone_calibration_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, button_text = "Next", save_answer = FALSE) {
  
  # NB, this page needs its own unique head because we don't want to initAudio()
  
  ui <- div(
    shiny::tags$style('._hidden { display: none;}'), # to hide textInputs
    includeScript("www/js/Tone.js"),
    includeScript("www/js/main.js"),
    includeScript("www/js/speech.js"),
    includeScript("www/js/audiodisplay.js"),
    
    # start body
    
    
    body,
    
    
    img(id = "record",
        src = "/img/mic128.png",
        onclick = "console.log(\"Pushed Record\");console.log(this);initAudio();toggleRecording(this);",
        style = "display:block; margin:1px auto;", width = "100px", height = "100px"),
    
    
    helpText("Click on the microphone to test"),
    hr(),
    helpText("Make sure your microphone levels are not too high when you speak or sing. Turn your microphone volume down if so."),
    helpText("If you see that the levels are moving a lot when you are sitting quietly, your room may be too noisy to complete the test."),
    hr(),
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


get_user_info_page<- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, button_text = "Give Browser Info", save_answer = TRUE) {
  
  
  ui <- div(
    
    html.head, # end head
    
    # start body
    
    body,
    
    div(shiny::tags$input(id = "user_info"), class="_hidden"
    )
    ,
    br(),
    shiny::tags$button(button_text, id="getUserInfoButton", onclick="getUserInfo();showHiddenButton(next);hideButton(this);"),
    br(),
    trigger_button("next", "Next", class="_hidden"),
    
    hr(),
    helpText("Click here to view more information about your privacy"),
    hr()
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = user_info_check)
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


record_background_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, save_answer = TRUE, body = NULL, button_text = "Next") {
  
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  
  
  ui <- div(
    
    html.head
    
    
    
    
    
    , # end headr
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton",
                                       onclick="recordAndStop(5000, false, true);")
                    
    ),
    
    shiny::tags$div(id="loading_area")
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}


record_5_second_hum_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, save_answer = TRUE, button_text = "Next") {
  
  # a page type for recording a 5-second user hum to compute signal-to-noise ratio (SNR)
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick="recordAndStop(5000, false, true);")
    ),
    
    shiny::tags$div(id="loading_area")
    
    
  ) # end main div
  
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}



singing_calibration_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, save_answer = TRUE, button_text = "Next") {
  
  
  # ask the user to sing a well-known song
  
  ui <- div(
    
    
    html.head,
    
    
    # start body
    
    body,
    
    
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick="recordAndStop(null,true, true);")
    ),
    shiny::tags$div(id="loading_area")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}



#### main test pages ####



play_long_tone_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
                                             save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
                                             note_no = "max", interval = NULL, start_note = NULL, ...) {
  
  # The arguments must be in this order: 
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, start_note, ...
  
  
  # a page type for playing a 5-second tone and recording a user singing with it
  
  # user_range_index: which index of the user's stored range should be used for the long tone
  
  
  tone.for.js <- start_note
  
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playTone(%s, 5)", tone.for.js))
    ),
    
    shiny::tags$div(id="loading_area")
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
} 







play_interval_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
                                            save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
                                            note_no = 2, start_note = NULL, ...) {
  
  # The arguments must be in this order: label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  #save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, start_note
  
  # a page type for playing a single interval, recording user audio response and saving as a file
 
  interval <- rel.to.abs.mel(start_note,  simple_intervals[stimuli_no])
  
  interval.for.js <- toString(interval)
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], true)", interval.for.js))
    ),
    
    shiny::tags$div(id="loading_area")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}



# create a page type for playing back midi files

play_midi_file_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
                      save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
                      note_no = "max", interval = NULL, start_note = NULL, ...) {
  
  # The first arguments must be in this order: 
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, start_note, ...
  
  # note_no. optionally limit number of notes
  
  if (is.null(note_no) == TRUE) {
    note_no <- "\"max\""
  }
  
  #dir_of_midi <- "/berkowitz_midi_rhythmic/Berkowitz"
  
  dir_of_midi <- "https://eartrainer.app/melodic-production/stimuli/"
  
  url <- paste0(dir_of_midi,"Berkowitz",stimuli_no,".mid")
  
  ui <- div(
    
    html.head,
    
    # start body
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=paste0("playMidiFileAndRecordAfter(\"",url,"\",true, ",note_no,", true)"))
    ),
    
    shiny::tags$div(id="loading_area")
    
  )
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
}




play_melody_from_list_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
          save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
          note_no = "max", interval = NULL, start_note = NULL, ...) {
 
  # The arguments must be in this order: 
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, start_note, ...
  
  # a page type for playing a melody, recording user audio response and saving as a file
  
  if (note_no == "max") {
    note_no <- length(stimuli[[stimuli_no]])
  }
  
  rel_melody <- stimuli[[stimuli_no]][0:note_no]
  
  melody <- rel.to.abs.mel(start_note, rel_melody)
  
  mel.for.js <- toString(melody)
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], false)", mel.for.js))
    ),
    
    shiny::tags$div(id="loading_area")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}







# create the test based on the excel file
  
test_v1 <- create.test(test_data)



# run the test
test <- make_test(
  elts = test_v1,
  opt = test_options("Melody Singing", "demo",
                     display = display_options(
                       css = "style.css")
  )
)


#shiny::runApp(".")

