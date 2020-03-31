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
  includeScript("www/js/main.js"),
  includeScript("www/js/speech.js"),
  includeScript("www/js/audiodisplay.js"),
  includeScript("www/js/modernizr-custom.js"),
  shiny::tags$script(htmltools::HTML('initAudio();// get audio context going')
  ))


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


generate.melody.in.user.range <- function(user_range, rel_melody) {
  
  # user_range: a range of absolute "starting" midi values
  # rel_melody: the melody in relative midi interval format
  
  # take a random starting note
  mel.start.note <- sample(user_range, 1)
  
  # melody as defined by the page argument
  user.optimised.melody <- rel.to.abs.mel(mel.start.note, rel_melody)
  
  return(user.optimised.melody)
  
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
  
  # add filename to global variable store
  #add.file.info.to.list(trial.filename, state = state)
  
  list(trial.id = NULL, # should be page label
       trial.timecode = tc,
       trial.filename =  trial.filename
  )
  
}


create.test <- function(data) {
  
  # main test builder from excel file
  
  tl <- c()
  
  # data: excel file
  for (row in 1:nrow(data)) {
    
    
    page_type <- data[row, "page_type"]
    page_id <- data[row, "id"]
    other <- data[row, "other"]
    text <- data[row, "text"]
    button_text <- data[row, "button_text"]
    stimuli_no <- data[row, "stimuli_no"]
    note_no <- data[row, "note_no"]
    save_answer <- paste0(", save_answer = ", data[row, "save_answer"]," ")
    print(save_answer)
    
    
    # handle button text
    if (page_type == "NAFC_page") {
      
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
        #print(str_c)
        count <- count + 1
      }
      
      #print(str_c)
      str_c <- paste0("c(",str_c,")")
      #print(str_c)
      
      button_text <- paste0("choices = ",str_c)
      
      #print(button_text)
      
      
    }
    
    else {
      button_text <- paste0("button_text =\"",button_text,"\"")
    }
    
    #print(button_text)
    
    # handle page text
    
    if (page_type == "NAFC_page" || page_type == "volume_calibration_page" || page_type == "text_input_page") {
      text <- paste0("prompt=htmltools::HTML(\'",text,"\'), ")
    }
    
    else {
      text <- paste0("body=htmltools::HTML(\'",text,"\'), ")
    }
    
    # handle pages that need stimuli
    
    if (page_type == "play_long_tone_record_audio_page") {
      other <- paste0("user_range_index= ", stimuli_no)
    }
    
    
    else if (page_type == "play_interval_record_audio_page") {
      other <- paste0("interval=simple_intervals[", stimuli_no, "]")
    }
    
    else if (page_type == "midi_page" || page_type == "play_mel_record_audio_page") {
      
      if (note_no == "max"){
        note_no <- "\"max\""  
      }
      
      other <- paste0("stimuli = ", stimuli_no, ", note_no=",note_no)
    }
    
    else { }
    
    
    # handle other field
    
    if (is.na(other)) {
      other <- ""
    } else {
      other <- paste0(other, ", ")
    }
    
    
    # put it together
    
    if (page_type == "one_button_page" || page_type == "volume_calibration_page") {
      page <- paste0(page_type, "(",other,text,button_text,")")
      
    }
    else {
      page <- paste0(page_type, "(label=\"",page_id,"\", ",other,text,button_text, save_answer,")")
      #print(page)
    }
    
    
    page <- eval(parse(text=page))
    save2diskmes <- eval(parse(text="elt_save_results_to_disk(complete = FALSE)"))
    
    
    tl <- c(tl,page,save2diskmes)
    
  }
  
  tl <- c(tl, eval(parse(text="elt_save_results_to_disk(complete = TRUE)")), 
          eval(parse(text="final_page(\"The end\")"))
  ) # after last page
  
  
  
  tl <- as.list(tl)
  
  return(tl)
  
}


calculate.range <- function(state, ...) {
  
  # currently a page for testing purposes, but perhaps just a function later
  
  res <- as.list(results(state))
  
  # get user session directory
  session_dir <- get_user_session_dir(state)
  
  user_singing_calibration_file_name <- res$results$user_singing_calibration$trial.filename
  user_singing_calibration <- readWave(paste0(session_dir,user_singing_calibration_file_name))
  
  # calculating periodograms of sections each consisting of 1024 observations,
  # overlapping by 512 observations:
  WspecObject <- periodogram(user_singing_calibration, width = 1024, overlap = 512)
  
  # calculate the fundamental frequency:
  #ff <- tuneR::FF(WspecObject, peakheight=0.015) #tuneR solution: issue, no bandpass try to get below working
  
  #ff <- seewave::autoc(user_singing_calibration, f = 44100, fmin = round(lowest_freq), fmax = round(highest_freq), plot = FALSE, xlab = "Time (s)", ylab = "Frequency (kHz)", ylim = c(0, f/44100), threshold = 2) # NB, also threshold argument
  df <- seewave::dfreq(user_singing_calibration , clip = 0.1, threshold = 10, wl=87.5, bandpass = c(round(lowest_freq), round(highest_freq)))
  #print(ff)
  
  # mean ff
  #user.mean.FF <- round(mean(ff, na.rm = TRUE), 2) * 1000
  #user.mean.midi <- round(freq_to_midi(user.mean.FF))
  
  # mean df
  user.mean.DF <- round(mean(df, na.rm = TRUE), 2) * 1000
  user.mean.DF.midi <- round(freq_to_midi(user.mean.DF))
  
  #print(user.mean.FF)
  #print(user.mean.midi)
  
  
  # define a user range
  
  #user.range <- generate.user.range(user.mean.midi)
  
  user.range <- generate.user.range(user.mean.DF.midi)
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    
    #renderPlot({plot(ff)}), # optional: plotenergy = FALSE for tuneR
    
    renderPlot({plot(df)}), # optional: plotenergy = FALSE
    
    #renderText({sprintf("The mean FF was %.2f", user.mean.FF)}), # mean FF
    
    #renderText({sprintf("The mean MIDI note was %i", user.mean.midi)}), # mean midi note
    
    renderText({sprintf("The mean DF was %.2f", user.mean.DF)}), # mean DF
    
    renderText({sprintf("The mean MIDI note was %i", user.mean.DF.midi)}), # mean midi note
    
    
    # next page
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, get_answer = function(input, ...) toString(input$user.range))
  
}



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
  
  print(html.file.list)
  
  
  ui <- div(
    
    html.head, # end head
    
    # start body
    
    html.file.list,
    
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = FALSE)
}


calculate.SNR.page <- reactive_page(function(state, ...) {
  
  # currently page for testing but probably just a function 
  
  res <- as.list(results(state))
  
  print(res)
  
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
  
  if (as.numeric(SNR) < 5) {
    display_error("Sorry, your signal is too noisy. Please try making your environment less noisy and/or your microphone signal better to complete the test.")
  }
  
  #periodograms
  userbgWspecObject <- tuneR::periodogram(user_background, width = 1024, overlap = 512, fastdisp = TRUE)
  userhumWspecObject <- tuneR::periodogram(user_hum, width = 1024, overlap = 512, fastdisp = TRUE)
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    renderText({paste0("SNR: ",round(SNR,2))}),
    
    renderText({"User Background"}), # optional: plotenergy = FALSE
    
    renderPlot({plot(userbgWspecObject)}, width = 100, height = 50), # optional: plotenergy = FALSE
    
    renderText({"User Hum"}), # optional: plotenergy = FALSE
    
    renderPlot({plot(userhumWspecObject)}, width = 100, height = 50), # optional: plotenergy = FALSE
    
    
    # next page
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui)
  
})


## main test pages

record_background_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, save_answer = TRUE, body = NULL, button_text = NULL) {
  
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  
  
  ui <- div(
    
    html.head
    
    
    
    
    
    , # end head
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton",
                                       onclick="recordAndStop(5000, false);")
                    
    ),
    
    shiny::tags$div(id="loading_area")
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}


record_5_second_hum_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, save_answer = TRUE, button_text = NULL) {
  
  # a page type for recording a 5-second user hum to compute signal-to-noise ratio (SNR)
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick="recordAndStop(5000);")
    ),
    
    shiny::tags$div(id="loading_area")
    
    
  ) # end main div
  
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}



singing_calibration_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, save_answer = TRUE, button_text = NULL) {
  
  
  # ask the user to sing a well-known song
  
  ui <- div(
    
    
    html.head,
    
    
    # start body
    
    body,
    
    
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick="recordAndStop(ms = null,showStop=true);")
    ),
    shiny::tags$div(id="loading_area")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}




play_long_tone_record_audio_page <- function(user_range_index, admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, save_answer = TRUE, button_text = NULL) {
  
  # a page type for playing a 5-second tone and recording a user singing with it
  
  # args
  # user_range_index: which index of the user's stored range should be used for the long tone
  
  
  #saved.user.range # not setup yet. this should be taken from the beginning of the test
  
  saved.user.range <- c(60,61,62,63,64)
  
  
  tone.for.js <- saved.user.range[user_range_index]
  
  
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





play_mel_record_audio_page <- function(stimuli_no, note_no = "max", admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, save_answer = TRUE, button_text = NULL) {
  
  # a page type for playing a melody, recording user audio response and saving as a file
  
  #saved.user.range # not setup yet. this should be taken from the beginning of the test
  
  saved.user.range <- c(60,61,62,63,64)
  
  if (note_no == "max") {
    note_no <- length(stimuli[[stimuli_no]])
  }
  
  melody <- generate.melody.in.user.range(saved.user.range, stimuli[[stimuli_no]])[0:note_no]
  
  mel.for.js <- toString(melody)
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s])", mel.for.js))
    ),
    
    shiny::tags$div(id="loading_area")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}


play_interval_record_audio_page <- function(interval, admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, save_answer = TRUE, button_text = NULL) {
  
  # a page type for playing a single interval, recording user audio response and saving as a file
  
  #saved.user.range # not setup yet. this should be taken from the beginning of the test
  
  saved.user.range <- c(60,61,62,63,64)
  
  interval <- generate.melody.in.user.range(saved.user.range, interval)
  
  interval.for.js <- toString(interval)
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    body,
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s])", interval.for.js))
    ),
    
    shiny::tags$div(id="loading_area")
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}



# create a page type for playing back midi files

midi_page <- function(stimuli_no, note_no = NULL, admin_ui = NULL,
                      on_complete = NULL, label=NULL, body = NULL, save_answer = TRUE, button_text = NULL) {
  
  
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
                    shiny::tags$button(button_text, id="playButton", onclick=paste0("playMidiFileAndRecordAfter(\"",url,"\",true, ",note_no,")")),
    ),
    
    shiny::tags$div(id="loading_area")
    
  )
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
}



microphone_calibration_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL, body = NULL, button_text = NULL, save_answer = FALSE) {
  
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
        style = "display:block; margin:1px auto;"),
    
    
    helpText("Click on the microphone to record."),
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
    hr(),
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = user_info_check)
}


video_page <- function(admin_ui = NULL, on_complete = NULL, label = NULL, src = NULL, body = NULL, button_text = "Next", save_answer = FALSE, video = NULL) {
  
  # page for presenting popup videos. TBC later
  
  #video_html <- htmltools::HTML(paste0(body,video.popup.html.1,src,video.popup.html.2))
  
  print(body)
  ui <- div(
    
    html.head, # end head
    
    # start body
    
    body,
    
    tags$video(src = src, type = "video/mp4", autoplay = FALSE, controls = TRUE, width="600px", height="450px"),
    
    br(),
    
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = FALSE)
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