# imports

library(psychTestR)
library(htmltools)
library(shiny)
library(shinyBS)
library(shinyjs)
library(tuneR)
library(seewave)
library(hrep)
require(rjson)

# constants

midi_notes <- c(40:84)
freq_notes <- lapply(midi_notes, midi_to_freq)
lowest_freq <- midi_to_freq(midi_notes[1])
highest_freq <- midi_to_freq(midi_notes[length(midi_notes)])
freq_range <- c(lowest_freq, highest_freq)
simple_intervals <- c(-12:24)


# import stimuli as relative midi notes
stimuli <- readRDS("Berkowitz_midi_relative.RDS")

# list of page types that don't return audio

non.audio.pages <- list("get_user_info", "microphone_test", "present_files", "quiet_question", "headphones_question")



# html header

html.head <- shiny::tags$head(shiny::tags$script(
htmltools::HTML('
// Create the XHR object.
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
}
')),

shiny::tags$style('._hidden { display: none;}'), # to hide textInputs
#includeScript("www/js/midi.js"),
shiny::tags$script(src="https://www.midijs.net/lib/midi.js"),
includeScript("www/js/Tone.js"),
includeScript("www/js/main.js"),
includeScript("www/js/speech.js"),
includeScript("www/js/audiodisplay.js"),
shiny::tags$script(htmltools::HTML('initAudio();// get audio context going')
))




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
  SNR <- 20*log10(abs(rms(env(signal))-rms(env(noise)))/rms(env(noise))) 
  return(SNR)
}





### PAGES ###


# NOTES
# reference tutorial: http://www.vesnam.com/Rblog/transcribing-music-from-audio-files-2/
# consider stereo/mono!! ...


SNR.page <- reactive_page(function(state, ...) {
  
  
  dis <- as.list(results(state))
  
  
  # first for user background
  user_background <- dis$results$user_background$audio
  
  
  ## split two channel audio
  audio_split_user_background <- length(user_background)/2
  
  user_background1 <- as.numeric(unlist(user_background[1:audio_split_user_background]))
  user_background2 <- as.numeric(unlist(user_background[(audio_split_user_background+1):length(user_background)]))
  
  
  # # construct wav object that the API likes
  userbgWobj <- Wave(user_background1, user_background2, samp.rate = 44100, bit = 16)
  
  userbgWobj <- normalize(userbgWobj, unit = "16", pcm = TRUE)
  
  userbgWobj <- mono(userbgWobj)
  
  
  #periodogram
  userbgWspecObject <- tuneR::periodogram(userbgWobj, width = 1024, overlap = 512)
  
  
  # same thing for note sing
  user_hum <- dis$results$user_hum$audio
  
  ## split two channel audio
  audio_split_user_hum <- length(user_hum)/2
  user_hum1 <- user_hum[1:audio_split_user_hum]
  user_hum2 <- user_hum[(audio_split_user_hum+1):length(user_hum)]
  
  # construct wav object that the API likes
  user_humWobj <- Wave(user_hum1, user_hum2, samp.rate = 44100, bit = 16)
  user_humWobj <- normalize(user_humWobj, unit = "16", pcm = TRUE)
  user_humWobj <- mono(user_humWobj)
  
  #periodogram
  userhumWspecObject <- periodogram(user_humWobj, width = 1024, overlap = 512)
  
  SNR <- compute.SNR(user_humWobj,userbgWobj)
  
  ui <- div(
    
    html.head,
    
    # start body
    
    renderText({paste0("SNR: ",round(SNR,2))}),
    
    #renderText({"User Background"}), # optional: plotenergy = FALSE
    
    #renderPlot({plot(userbgWspecObject)}, width = 100, height = 50), # optional: plotenergy = FALSE
    
    #renderText({"User Hum"}), # optional: plotenergy = FALSE
    
    #renderPlot({plot(userhumWspecObject)}, width = 100, height = 50), # optional: plotenergy = FALSE
    
    
    # next page
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui)
  
})


calculate.range <- function(answer, ...) {
  
  a <- answer$audio
  
  ## split two channel audio
  audio_split <- length(a)/2
  a1 <- a[1:audio_split]
  a2 <- a[(audio_split+1):length(a)]
  
  # construct wav object that the API likes
  Wobj <- Wave(a1, a2, samp.rate = 44100, bit = 16)
  Wobj <- normalize(Wobj, unit = "16", pcm = TRUE)
  Wobj <- mono(Wobj)
  
  # calculating periodograms of sections each consisting of 1024 observations,
  # overlapping by 512 observations:
  WspecObject <- periodogram(Wobj, width = 1024, overlap = 512)
  
  # calculate the fundamental frequency:
  ff <- tuneR::FF(WspecObject, peakheight=0.015) #tuneR solution: issue, no bandpass try to get below working
  
  #ff <- seewave::autoc(WspecObject, f = 44100, fmin = round(lowest_freq), fmax = round(highest_freq), plot = FALSE) # NB, also threshold argument
  
  print(ff)
  
  # mean ff
  user.mean.FF <- round(mean(ff, na.rm = TRUE), 2)
  user.mean.midi <- round(freq_to_midi(user.mean.FF))
  
  print(user.mean.FF)
  print(user.mean.midi)
  
  
  # define a user range
  
  user.range <- generate.user.range(user.mean.midi)
  
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    
    renderPlot({plot(ff)}), # optional: plotenergy = FALSE
    
    renderText({sprintf("The mean FF was %.2f", user.mean.FF)}), # mean FF
    
    renderText({sprintf("The mean MIDI note was %i", user.mean.midi)}), # mean midi note
    
    
    # next page
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, get_answer = function(input, ...) toString(input$user.range))
  
}



get.answer.grab.audio <- function(input, ...) {
  tc = Sys.time()
  list(trial.id = NULL, # should be page label
       audio = input$audio,
       trial.timecode = tc,
       trial.filename =  gsub("[^0-9]","",tc)
  )
}



add.file.info.to.list <- function(state, answer) {
  
  # for testing. keep a list of the file names to present at the end of a test
  
  trial.filename <- answer$trial.filename
  trial.name <- answer
  
  print(answer)
  
  state_global <- get_global("file_list", state)
  
  if (is.null(state_global) == TRUE) {
    
    # if a file_list doesn't exist, create one and add the first item to the list
    
    print("it's null")
    
    set_global("file_list", list(trial.filename), state)
    
    print("added item")
    print(get_global("file_list", state))
    
    
  } 
  else {
    print("it's not null")
    # if it does exist, then append the latest response to the list
    
    file_list <- get_global("file_list", state)
    
    updated.list <- c(file_list, trial.filename)
    
    set_global("file_list", updated.list, state)
    
    print("added item")
    
    print(get_global("file_list", state))
    
    
  }
  
  
}

process.audio <- code_block(function(state, answer, ...) {
  # saves audio from page before
  # answer is  audio from the previous page, as extracted by get_answer()
  
  a <- answer$audio
  
  trial.filename <- answer$trial.filename
  
  ## split two channel audio
  audio_split <- length(a)/2
  
  a1 <- a[1:audio_split]
  
  a2 <- a[(audio_split+1):length(a)]
  
  # construct wav object that the API likes
  Wobj <- Wave(a1, a2, samp.rate = 44100, bit = 16)
  Wobj <- normalize(Wobj, unit = "16", pcm = TRUE)
  #Wobj <- mono(Wobj)
  
  # writing the file to the www directory
  wav_name <- paste0("www/audio",trial.filename,".wav")
  writeWave(Wobj, wav_name, extensible = FALSE)
  wav_name
  
  # session-specific writing
  session_dir <- get_session_info(state, complete = FALSE)$p_id
  wav_name_sess <- paste0("output/sessions/",session_dir, "/audio",trial.filename,".wav")
  writeWave(Wobj, wav_name_sess, extensible = FALSE)
  wav_name_sess
  
  # and append the file name to the test session global variable
  
  add.file.info.to.list(state, answer)
})






record_background_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL) {
  
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  
  
  ui <- div(
    
   html.head
    

    
    
    
    , # end head
    
    # start body
    
    shiny::tags$p("We need to record 5 seconds of your room WITHOUT you singing, just to see what your background noise is like. When you are ready to record your environment for 5 seconds, press the button below."),
    
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button("I'm Ready to record my background", id="playButton", onclick="recordAndStop(5000, false);")
                    
    ),
    
    shiny::tags$div(id="loading_area")
    
    ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}


record_5_second_hum_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL) {
  
  # a page type for recording a 5-second user hum to compute signal-to-noise ratio (SNR)
  
  
  ui <- div(

    html.head,
    
    # start body
    
    shiny::tags$p("Now we need to record you humming any comfortable note for 5-seconds. Feel free to practice first. When you are ready, take a deep breath, start humming and then click the Ready button just after. Try to keep one long hum without stopping at all. You can stop humming when the red bird disappears."),
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button("I'm Ready to hum (and will start just before I click this)", id="playButton", onclick="recordAndStop(5000);")
    ),
    
    shiny::tags$div(id="loading_area")
    
    
    ) # end main div
  
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}



singing_calibration_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL) {
  
  
  # ask the user to sing a well-known song
  
  ui <- div(
   
    
    html.head,
    
    
    # start body
    
    shiny::tags$p("Please sing \"Happy Birthday\" using the following lyrics and name:"),
    
    shiny::tags$p("Happy birthday to you. Happy birthday to you. Happy birthday to Alex. Happy birthday to you."),
    
    
    shiny::tags$p("Press Stop when you are finished."),
    
    
    
    shiny::tags$div(id="button_area",
                    shiny::tags$button("Sing Happy Birthday", id="playButton", onclick="recordAndStop(ms = null,showStop=true);")
    ),
    shiny::tags$div(id="loading_area")
    
    
    ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}




play_long_tone_record_audio_page <- function(user_range_index, admin_ui = NULL, on_complete = NULL, label= NULL) {
  
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
    
    shiny::tags$p("When you click the button below, you will hear a 5-second tone. You must try your best to sing along with this tone immediately. The idea is to sing the exact same tone."),
    shiny::tags$div(id="button_area",
                    shiny::tags$button("Play Tone and Sing Along", id="playButton", onclick=sprintf("playTone(%s, 5)", tone.for.js))
    ),
    
    shiny::tags$div(id="loading_area")
    
    ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}





play_mel_record_audio_page <- function(stimuli_no, note_no, admin_ui = NULL, on_complete = NULL, label= NULL) {
  
  # a page type for playing a melody, recording user audio response and saving as a file
  
  #saved.user.range # not setup yet. this should be taken from the beginning of the test
  
  saved.user.range <- c(60,61,62,63,64)
  
  melody <- generate.melody.in.user.range(saved.user.range, stimuli[stimuli_no])[0:note_no]
  
  mel.for.js <- toString(melody)
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    shiny::tags$p("Press Play to hear a melody. Please keep singing it back until you think you have sung it correctly, then press Stop. Don't worry if you don't think you sung it right, just do your best!"),
    shiny::tags$div(id="button_area",
                    shiny::tags$button("Play Melody", id="playButton", onclick=sprintf("playSeq([%s])", mel.for.js))
    ),
    
    shiny::tags$div(id="loading_area")
    
    
    ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}


play_interval_record_audio_page <- function(interval, admin_ui = NULL, on_complete = NULL, label= NULL) {
  
  # a page type for playing a single interval, recording user audio response and saving as a file
  
  #saved.user.range # not setup yet. this should be taken from the beginning of the test
  
  saved.user.range <- c(60,61,62,63,64)
  
  interval <- generate.melody.in.user.range(saved.user.range, interval)
  
  interval.for.js <- toString(interval)
  
  # listen for clicks from play button then play
  
  
  ui <- div(
    
    html.head,
    
    # start body
    
    shiny::tags$p("You will hear two notes. Click the button below and sing them back immediately. Don't worry if you make a mistake, just press stop after you tried once."),
    shiny::tags$div(id="button_area",
                    shiny::tags$button("Play Two Notes", id="playButton", onclick=sprintf("playSeq([%s])", interval.for.js))
    ),
    
    shiny::tags$div(id="loading_area")
    
    ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = get.answer.grab.audio)
  
}



# create a page type for playing back midi files

midi_page <- function(stimuli_no,
                      admin_ui = NULL,
                      on_complete = NULL, label=NULL) {
  
  url <- paste0("/berkowitz_midi_rhythmic/Berkowitz",stimuli_no,".mid")
  
  ui <- div(
    
    html.head,
    
    # start body
    shiny::tags$p("Press Play to hear a melody. This time try and sing back the melody and the rhythm as best you can. Just do your best on the first go then press stop!"),
    
    shiny::tags$div(id="button_area",
    shiny::tags$button("Play Melody", id="playButton", onclick=paste0("playMidiFileAndRecordAfter(\"",url,"\")")),
    ),
  
  shiny::tags$div(id="loading_area")
  
  )
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete)
}



microphone_calibration_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL) {
  
  # NB, this page needs its own unique head because we don't want to initAudio()
  
  ui <- div(
      shiny::tags$style('._hidden { display: none;}'), # to hide textInputs
      includeScript("www/js/Tone.js"),
      includeScript("www/js/main.js"),
      includeScript("www/js/speech.js"),
      includeScript("www/js/audiodisplay.js"),
    
    # start body
    
    shiny::tags$p(
      "We need to test your microphone before we proceed. Please make sure your microphone is plugged in then click below. You should see your signal coming in below. If you do not, then your microphone may not be setup properly and you will need to try again."
    ),
    
    
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
    trigger_button("next", "Next"),
    hr()
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE)
}


get_user_info_page<- function(admin_ui = NULL, on_complete = NULL, label= NULL) {
  
  
  ui <- div(
    
    html.head, # end head
    
    # start body
    
    div(shiny::tags$input(id = "user_info"), class="_hidden"
    )
    ,
    br(),
    shiny::tags$button("Get User Info", id="getUserInfoButton", onclick="getUserInfo();"),
    br(),
    br(),
    br(),
    trigger_button("next", "Next")
    
    
  ) # end main div
  
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, ...) fromJSON(input$user_info))
}



present_files_page <- function(state, admin_ui = NULL, on_complete = NULL, label= NULL) {
  
  
  # get list of (all) page titles
  
  page_titles <- names(as.list(results(state))$results)
  
  print(page_titles)
  
  # create a list of pages that return audio
  
  joined_list <- unlist(c(page_titles, non.audio.pages))
  
  print(joined_list)
  
  audio_pages <- joined_list[!(duplicated(joined_list)|duplicated(joined_list, fromLast=TRUE))]   # return only the unique value
  
  print(audio_pages)
  
  # now get list of file names
  
  
  file_list <- get_global("file_list", state)
  
  html.file.list <- list() # instantiate empty string
  
  count <- 1
  count_4 <- 1
  
  for (f in file_list) {
    
    title <- audio_pages[count]
    path <- paste0("audio",f,".wav")
    
    new.title <- tags$p(title)
    
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


# create the timeline
timeline <- list(
  
  one_button_page(
    div(
      p("Welcome to the", tags$strong("Melody Singing Test!")),
      p("Please click below to proceed.")
    ),
    button_text = "I'm Ready"
  ),
  
  
  NAFC_page(label = "quiet_question",
            prompt = "Are you in a quiet environment?",
            choices = c("Yes", "No"),
            on_complete = function(answer, ...) {
              res <- suppressWarnings(answer)
              if (!is.na(res) && res == "Yes") TRUE
              else display_error("Sorry, you cannot complete the test unless you are in a quiet environment.")
            }),

  NAFC_page(label = "headphones_question",
            prompt = "To complete this test you must wear headphones. You are not allowed to playback sound through your speakers. Please confirm that you will use headphones.",
            choices = c("Yes, I am using headphones.", "I cannot use headphones."),
            on_complete = function(answer, ...) {
              res <- suppressWarnings(answer)
              if (!is.na(res) && res == "Yes, I am using headphones.") TRUE
              else display_error("Sorry, you cannot complete the test unless you are using headphones.")
            }),


  #volume_calibration_page(url = "test_headphones.mp3", type='mp3', button_text = "I can hear the song, move on."),

  get_user_info_page(label="get_user_info"),


  elt_save_results_to_disk(complete = FALSE),

  code_block(function(state, ...) {
    # seems like this may need to be after some form of results to disk saving
    session_dir <- get_session_info(state, complete = FALSE)$p_id

    print(session_dir)

  }),

  microphone_calibration_page(label = "microphone_test"),

  record_background_page(label="user_background"),

  process.audio,

  elt_save_results_to_disk(complete = FALSE),

  record_5_second_hum_page(label = "user_hum"),

  process.audio,

  SNR.page,

  elt_save_results_to_disk(complete = FALSE),
  
  singing_calibration_page(label = "user_singing_calibration"),
  
  process.audio,
  
  elt_save_results_to_disk(complete = FALSE),
  
  
  reactive_page(function(answer, ...) {
    calculate.range(answer = answer)
  }),
  
  play_long_tone_record_audio_page(label="tone_1", user_range_index=1),
  
  process.audio,
  
  elt_save_results_to_disk(complete = FALSE),
  
  play_interval_record_audio_page(label="interval_1", interval=simple_intervals[1]),
  
  process.audio,
  
  elt_save_results_to_disk(complete = FALSE),
  
  
  play_mel_record_audio_page(stimuli_no = 1, note_no = 4, label="melody_1"),
  
  process.audio,
  
  elt_save_results_to_disk(complete = FALSE),
  
  midi_page(stimuli_no = 2, label="rhythm_mel_1"),
  
  process.audio,
  
  elt_save_results_to_disk(complete = FALSE),
  
  reactive_page(function(state, ...) {
    present_files_page(state = state, label = "present_files")
  }),
  
  
  # pilot Q's
  text_input_page("pilot_1", "Were all the instructions easy to understand? If not, please described what confused you.", save_answer = TRUE,
                 button_text = "Next", width = "300px",
                  height = "100px"),
  
  text_input_page("pilot_2", "Please describe any issues you experienced with the test?", save_answer = TRUE,
                  button_text = "Next", width = "300px",
                  height = "100px"),
  
  
  elt_save_results_to_disk(complete = TRUE), # after last page
  
  final_page("The end")
)




# run the test
test <- make_test(
  elts = timeline,
  opt = test_options("Melody Singing", "demo",
                     display = display_options(
                       css = "style.css")
  )
)



#shiny::runApp(".")