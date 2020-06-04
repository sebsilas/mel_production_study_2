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

simple_intervals <- c(-12:24)
test_data <- as.data.frame(read_excel("test.xlsx"))
vocal.range.factor <- 2 # i.e the number of semitones to clip each range from


# import stimuli
stimuli.abs <- readRDS("Berkowitz_Absolute.RDS")
stimuli <- lapply(stimuli.abs, diff)


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
  includeScript("www/js/Tone.js"),
  includeScript("www/js/modernizr-custom.js"),
  shiny::tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js"),
  shiny::tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"),
  includeScript("www/js/main.js")
  )





# footer
html.footer <- div(
  htmltools::includeScript("https://sdk.amazonaws.com/js/aws-sdk-2.2.32.min.js"),
  includeScript("www/js/record.js"),
  shiny::tags$script('
  var wRegion = "us-east-1";
  var poolid = "us-east-1:c74a7565-ecd3-4abb-9dba-3d02b483e795";
  var s3bucketName = "melody-singing-task";
  console.log(p_id);
  var audioPath = `/${p_id}/audio-files`;
  function audio(wRegion,poolid,path) {
    AStream = new AudioStream(wRegion,poolid,path);
    return AStream;
  }
  var NewAudio = audio(wRegion,poolid,s3bucketName+audioPath);
  NewAudio.audioStreamInitialize();
  '))

html.footer2 <- div(
  htmltools::includeScript("https://sdk.amazonaws.com/js/aws-sdk-2.2.32.min.js"),
  #includeScript("www/js/record.js"),
  shiny::tags$script('
  var wRegion = "us-east-1";
  var poolid = "us-east-1:c74a7565-ecd3-4abb-9dba-3d02b483e795";
  var s3bucketName = "melody-singing-task";
  console.log(p_id);
  var audioPath = `/${p_id}/audio-files`;
  function AudioBuild(wRegion,poolid,path) {
    console.log("AudioBuild called");
    console.log(typeof AudioStream);
    AudioS = new AudioStream(wRegion,poolid,path);
    return AudioS;
  }
  var NewAudio = AudioBuild(wRegion,poolid,s3bucketName+audioPath);
  NewAudio.audioStreamInitialize();
  '))


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


rel.to.abs.mel.mean.centred <- function(rel_melody, user_mean_note) {
  # produce a melody which is centered on the user's range. 
  # NB: the "mean stimuli note" could/should be sampled from around the user's mean range i.e +/- 3 semitones
  
  cat("rel melody: ", str(rel_melody))
  
  mean_of_stimuli <- round(mean(rel.to.abs.mel(0, rel_melody)))
  
  cat("mean of stimuli: ", mean_of_stimuli)
  
  start_note <-  user_mean_note - mean_of_stimuli
    
  cat("start note: ", start_note)
  
  stimuli_centred_to_user_mean <- rel.to.abs.mel(start_note, rel_melody)
  
  return(stimuli_centred_to_user_mean)
  
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



user_info_check <- function(input, ...)  {
  
  if (input$browser_capable == "FALSE") {
    display_error("Sorry, your browser does not have the have requirements to complete the test. Please download the latest version of Google Chrome to complete the experiment.")
  }
  
  else {
    list(user_info = fromJSON(input$user_info))
  }
  
}


get.timecode <- function(input, state, getRhythms, ...) {
  
  # if getRhythms == TRUE, also get rhythm data
  
  print("getting timecode")
  

  if (getRhythms == TRUE) {
    page_answer <- list(trial.timecode = input$timecode,
                        playback.count = input$playback_count,
                        stimuli.pitch = fromJSON(input$stimuli_pitch),
                        stimuli.ticks = fromJSON(input$stimuli_ticks),
                        stimuli.duration = fromJSON(input$stimuli_duration),
                        stimuli.durationTicks = fromJSON(input$stimuli_durationTicks))
  }
  
  else {
  
    page_answer <- list(trial.timecode = input$timecode,
         playback.count = input$playback_count,
         stimuli.pitch = input$stimuli_pitch)
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


page.builder <- function(page_type, argus) {
  
  # the first argument should be the "function" (page)
  half.built.page.list <- append(get(page_type), argus, after = 1)
  
  new.page.builder.function <- function(state, answer, ...) {
    
    #cat("half built page list", str(half.built.page.list))
    
    # get the args generated at runtime and add them to argument list
    
    print(page_type)
    
    if (any(grepl(page_type, user.starting.range.pages))) {
      sampled_mean_note <- get_global("sampled_mean_note", state)
      half.built.page.list$sampled_mean_note <- sampled_mean_note
    }
    
    p.id <- get.p.id(state)
    half.built.page.list$p_id <- p.id
    
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
    
    sampled_mean_note <- mean(range) + sample(-3:3, 1)
    cat("Sampled next mean range note:",sampled_mean_note, sep="\n")
    
    set_global("sampled_mean_note",sampled_mean_note, state)
      
  }) # end code block
  
  # then wrap in a reactive page
  page <- reactive_page(pageb) # end reactive page

  both <- list(cb,page)
  
  return(both)
  
  
} # end main function





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
          page_pars$arrange_vertically <- FALSE
        } else {} # do nothing
        
        
        
        
        # generate the pages
        if (any(grepl(page_type, user.starting.range.pages))) { # this condition finds which pages require a start_note argument
          
          page.builder.fun <- page.builder(page_type, page_pars)
          
          page <- random.note.from.user.range(page.builder.fun)
          
          tl <- append(tl, page, after = length(tl)) # add to timeline. these pages have the results message already appended
        }
        
        else if (any(grepl(page_type, c("record_background_page", "record_5_second_hum_page")))) { # or another page that collects audio, but doesn't need a start note (i.e these pages need a p_id)
          
          page.builder.fun <- page.builder(page_type, page_pars)
          
          page <- reactive_page(page.builder.fun) # end reactive page
          
          tl <- append(tl, page, after = length(tl)) # add to timeline. these pages have the results message already appended
        }
        
        else {
          
          # evaluate the pages that don't need a sampled_mean_note argument
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
  
  ui <- div(
    shiny::tags$style('._hidden { display: none;}'), # to hide textInputs
    includeScript("www/js/Tone.js"),
    includeScript("www/js/audio_display.js"),
    includeScript("www/js/speech.js"),
    includeScript("www/js/audio_display.js"),

    # start body
    
    
    body,
    
    
    img(id = "record",
        src = "img/mic128.png",
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

record_background_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, save_answer = TRUE, body = NULL, button_text = "Next", p_id = NULL) {
  
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  
  
  ui <- div(
    
    html.head
    
    , # end head
    
    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton",
                                       onclick="recordAndStop(5000, false, true, this.id);")
                    
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, FALSE) } )
  
}


record_5_second_hum_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, save_answer = TRUE, button_text = "Next", p_id = NULL) {
  
  # a page type for recording a 5-second user hum to compute signal-to-noise ratio (SNR)
  
  
  ui <- div(
    
    html.head,
    
    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick="recordAndStop(5000, false, true, this.id);")
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer2
  
  ) # end main div
  
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, FALSE) } )
  
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
                                             note_no = "max", interval = NULL, sampled_mean_note = NULL, p_id = NULL, ...) {
  
  # a page type for playing a 5-second tone and recording a user singing with it
  
  # The arguments must be in this order: 
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL, 
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no, 
  # note_no = "max", interval = NULL, sampled_mean_note, ...
  
  
  # user_range_index: which index of the user's stored range should be used for the long tone

  
  tone.for.js <- sampled_mean_note
  
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
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("console.log(this.id);playTone(%s, 5, this.id);", tone.for.js))
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer2
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, FALSE) })
  
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
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], true, this.id)", interval.for.js))
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer2
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, FALSE) })
  
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
  
  cat("transpose by: ", -(transpose), sep="\n")
  
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
                    shiny::tags$button(button_text, id="playButton", onclick=paste0("playMidiFileAndRecordAfter(\"",url,"\",true, ",note_no,", true, this.id, ",transpose,")"))
    ),
    
    shiny::tags$div(id="loading_area"),
    html.footer2
  )
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, TRUE) })
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
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], true, this.id)", mel.for.js))
    ),
    
    shiny::tags$div(id="loading_area"),
    
    html.footer2
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, FALSE) })
  
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
#rsconnect::deployApp('/Users/sebsilas/mel_production_aws')
