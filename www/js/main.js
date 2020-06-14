console.log("loaded main.js");


/// playback stuff ///

function toneJSInit() {
  
  // sound: i.e "tone" or "piano"
  
  console.log("toneJS Inited!");
  
    window.synthParameters = {
        oscillator: {
          type: 'sine',
          partialCount: 4
        },
        envelope: { // http://shura.shu.ac.uk/8259/1/96913_Soranzo_psychoacoustics.pdf
          attack: 0.01,
          decay: 0.01,
          sustain: 0.50, // this is changed from the parameters above, which was 0.25
          release: 0.01,
          attackCurve: 'cosine'
        }
      };
    
    //create a synth and connect it to the master output (your speakers)
    window.synth = new Tone.Synth(synthParameters).toMaster();
  

  // create a piano and connect to master output
  window.piano = SampleLibrary.load({
    instruments: "piano",
    minify: true
   });
    
  window.piano.toMaster();
 
}

toneJSInit();
//

function testFeatureCapability() {

    if (Modernizr.audio & Modernizr.audiopreload & Modernizr.webaudio) {
        console.log("This browser has the necessary features");
        Shiny.setInputValue("browser_capable", "TRUE");
        } else {
        console.log("This browser does not have the necessary features");
        Shiny.setInputValue("browser_capable", "FALSE");
        }
}

function getUserInfo () {
    testFeatureCapability();
    console.log(navigator);
    var _navigator = {};
    for (var i in navigator) _navigator[i] = navigator[i];
    delete _navigator.plugins;
    delete _navigator.mimeTypes;
    navigatorJSON = JSON.stringify(_navigator);
    console.log(navigatorJSON);
    console.log("Browser:" + navigator.userAgent);
    Shiny.setInputValue("user_info", navigatorJSON);
}


// UI functions //

function hidePlayButton() {

var x = document.getElementById("playButton");
 if (x.style.display === "none") {
 x.style.display = "block";
 } else {
 x.style.display = "none";
 }

}


function hideRecordImage() {

  var x = document.getElementById("button_area");
       if (x.style.display === "none") {
   x.style.display = "block";
   } else {
   x.style.display = "none";
   }
    
}

function showStopButton() {
 
    var stopButton = document.createElement("button");
    stopButton.style.display = "block";
    stopButton.innerText = "Stop"; // Insert text
    stopButton.addEventListener("click", function () { 
        NewAudio.stopRecording("playButton"); 
        });
    button_area.appendChild(stopButton);

}

function showRecordingIcon() {

  var img = document.createElement("img"); 
  img.style.display = "block";
  
  img.src =  "./img/sing.png"; 
  img.width = "280";
  img.height = "280";
  button_area.appendChild(img);

}

function showLoadingIcon() {

    var img = document.createElement("img"); 
    img.src =  "./img/loading.gif"; 
    img.width = "320";
    img.height = "224";
    loading_area.appendChild(img);
}


function recordUpdateUI(showStop, hidePlay) {

    // update the recording UI
    // if showStop is true, then give the user the option to press the stop button
    // if hidePlay is true, then hide the play button


    if  (hidePlay === true) {
      hidePlayButton();
    }
    
    if (showStop === true) {
        setTimeout(() => {  showStopButton(); }, 500); // a little lag
    }

    setTimeout(() => {  showRecordingIcon(); }, 500); // a little lag
   
}

function showHiddenButton (e) {
  e.classList.remove("_hidden");
}


function hideButton (e) {
  e.classList.add("_hidden");
}


//


function recordAndStop (ms, showStop, hidePlay, id, sound) {
    // start recording but then stop after x milliseconds
    
    NewAudio.startRecording(id);

     if (ms === null) {
        recordUpdateUI(showStop, hidePlay);
     }

     else {
        recordUpdateUI(showStop, hidePlay);
        setTimeout(() => {  NewAudio.stopRecording(id); }, ms); 
     }

}
  
function triggerNote(sound, freq_tone, seconds) {
  
  //console.log(freq_tone);
  console.log(Tone.Midi(freq_tone).toMidi());


  if (sound === "piano") {
  	piano.triggerAttackRelease(freq_tone, seconds);

  }
  
  else {
  	console.log("tone");
    synth.triggerAttackRelease(freq_tone, seconds);
  }

}

function  playTone(tone, seconds, id, sound) {
  // play a tone for x seconds
  rangeTest(tone);

  tone = Number(tone);
  console.log(tone);

  freq_tone = Tone.Frequency(tone, "midi").toNote();
  console.log(freq_tone);
  
  triggerNote(sound, freq_tone, seconds);
  
  recordAndStop(seconds*1000+500, false, true, id);

  updatePlaybackCount();
  
  Shiny.setInputValue("stimuli_pitch", tone);

}
 

 
function playSeq (note_list, hidePlay, id, sound) {
    // hide play. boolean. whether to hide the play button
  
  rangeTest(note_list);

  updatePlaybackCount();
  
    // hide play button after 3rd playback attempt
  if (playback_count === 3) {
    hidePlayButton();
  }

  midi_list = note_list.map(x => Tone.Frequency(x, "midi").toNote());
  console.log(midi_list);
  last_note = midi_list.length;
  count = 0;
  var pattern = new Tone.Sequence(function(time, note){
    console.log(note);
    triggerNote(sound, note, 0.50);
    
    count = count + 1;
  
    if (count === last_note) {
      if (playback_count === 1) { recordAndStop(null, true, hidePlay, id); } // only record the first time
      pattern.stop();
      Tone.Transport.stop();
    }

  }, midi_list);
  
  pattern.start(0).loop = false;
  Tone.Transport.start();
 
  Shiny.setInputValue("stimuli_pitch", note_list);

}
   

function toneJSPlay (midi, note_no, hidePlay, transpose, id, sound) {
  
    console.log(transpose);
    const now = Tone.now() + 0.5;
    const synths = [];
    midi.tracks.forEach(track => {
        
        if (note_no === "max") {
            notes_list = track.notes; console.log(track.notes); // need to test full notes
            dur = track['duration'] * 1000; 
         
        } else {
                dur = 0;
               notes_list = track['notes'].slice(0, note_no);
               notes_list.forEach(el => { 
                   console.log(el['duration']); 
                   dur = dur+el['duration'];
                   console.log(dur);
                })
        }

        dur = dur * 1000;
        console.log(dur);

        setTimeout(() => {  recordAndStop(null, true, hidePlay, id); }, dur); 

        //create a synth for each track
        const synth = new Tone.PolySynth(2, Tone.Synth, synthParameters).toMaster();
        synths.push(synth);
        //schedule all of the events
        notes_list.forEach(note => {
          name = note.name;
          console.log(transpose);
          console.log(name);
          transposed_note = Tone.Frequency(name).transpose(transpose);
          console.log("transposed note",transposed_note);
        //synth.triggerAttackRelease(transposed_note, note.duration, note.time + now, note.velocity);
        triggerNote(sound, transposed_note, note.duration);
        });
        
        console.log(notes_list);
        
        shiny_notes = [];
        shiny_ticks = [];
        shiny_duration = [];
        shiny_durationTicks = [];
        
        notes_list.forEach(note => {
          console.log(note);
          shiny_notes.push(note.midi);
          shiny_ticks.push(note.ticks);
          shiny_duration.push(note.duration);
          shiny_durationTicks.push(note.durationTicks);
        });
        
        console.log(JSON.stringify(shiny_notes));
        console.log(JSON.stringify(shiny_ticks));
        console.log(JSON.stringify(shiny_duration));
        console.log(JSON.stringify(shiny_durationTicks));
        
        Shiny.setInputValue("stimuli_pitch", JSON.stringify(shiny_notes));
        Shiny.setInputValue("stimuli_ticks", JSON.stringify(shiny_ticks));
        Shiny.setInputValue("stimuli_duration", JSON.stringify(shiny_duration));
        Shiny.setInputValue("stimuli_durationTicks", JSON.stringify(shiny_durationTicks));
        
    });

}
    
async function midiToToneJS (url, note_no, hidePlay, transpose, id, sound) {
      
// load a midi file in the browser
const midi = await Midi.fromUrl(url).then(midi => {

    console.log(midi);
    console.log(transpose);
    toneJSPlay(midi, note_no, hidePlay, transpose, id, sound);
    
})
}


// Define a function to handle status messages

function playMidiFileAndRecordAfter(url, toneJS, note_no, hidePlay, id, transpose, sound) {
    

    // toneJS: boolean. true if file file to be played via toneJS. otherwise, via MIDIJS
    // note_no, optional no of notes to cap at 

    if (toneJS === true) {
        midiToToneJS(url, note_no, hidePlay, transpose, id, sound);
    }

    else {
    function display_message(mes) {
        console.log(mes);
    }

    MIDIjs.message_callback = display_message; 
    MIDIjs.player_callback = display_time; 

    console.log(MIDIjs.get_audio_status());

    MIDIjs.play(url);

    // Define a function to handle player events
    function display_time(ev) {

    console.log(ev.time); // time in seconds, since start of playback

    MIDIjs.get_duration(url,  function(seconds) { console.log("Duration: " + seconds); 

    if (ev.time > seconds) {
        console.log("file finished!");
        MIDIjs.player_callback = null;
        recordAndStop(null, true, true, id);
    } 
        
    });

    }
    }
    
}

playback_count = 0; // number of times user presses play in a trial

function updatePlaybackCount() {
    playback_count =  playback_count + 1;
    console.log(playback_count);
    Shiny.setInputValue("playback_count", playback_count);
 }


 //

 function range(start, stop, step) {
  var a = [start], b = start;
  while (b < stop) {
      a.push(b += step || 1);
  }
  return a;
}

var soprano = range(60, 84, 1);
var alto = range(53, 77, 1);
var tenor = range(48, 72, 1);
var baritone = range(45, 69, 1);
var bass = range(40, 64, 1);

function rangeTest(notes_list) {

    if (typeof notes_list == 'number') {
      notes_list = [notes_list];
    }

    notes_list.forEach(function(note) {

      if (soprano.includes(note) === true) {
        console.log("this comes in the soprano range!");
      }

      if (alto.includes(note) === true) {
        console.log("this comes in the alto range!");
      }

      if (tenor.includes(note) === true) {
        console.log("this comes in the tenor range!");
      }

      if (baritone.includes(note) === true) {
        console.log("this comes in the baritone range!");
      }

      if (bass.includes(note) === true) {
        console.log("this comes in the bass range!");
      } 

  });

}