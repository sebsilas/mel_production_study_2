console.log("loaded main.js");

const synth_params = {
    oscillator: {
      type: 'sine',
      partialCount: 4
    },
    envelope: { // http://shura.shu.ac.uk/8259/1/96913_Soranzo_psychoacoustics.pdf
      attack: 0.01,
      decay: 0.01,
      sustain: 0.25,
      release: 0.01,
      attackCurve: 'cosine'
    }
  }

//create a synth and connect it to the master output (your speakers)
const synth = new Tone.Synth(synth_params).toMaster();


//

window.AudioContext = window.AudioContext || window.webkitAudioContext;

var audioContext = new AudioContext();
var audioInput = null,
    realAudioInput = null,
    inputPoint = null,
    audioRecorder = null;
var rafID = null;
var analyserContext = null;
var canvasWidth, canvasHeight;
var recIndex = 0;

/* TODO:

- offer mono option
- "Monitor input" switch
*/

function saveAudio() {
    //audioRecorder.exportWAV( doneEncoding );
    // could get mono instead by saying
    audioRecorder.exportMonoWAV( doneEncoding );
    console.log("saveAudio called");
}

function gotBuffers(buffers, initiateNext) {

     // if (updateDisplay===true) {
    //     var canvas = document.getElementById( "wavedisplay" ); 
    //     drawBuffer( canvas.width, canvas.height, canvas.getContext('2d'), buffers[0] );
    // }

    Shiny.onInputChange("audio", buffers);
    //Shiny.onInputChange("next_page", performance.now());
    next_page();
    hideRecordImage();
    console.log("reached end of gotBuffers");



    // the ONLY time gotBuffers is called is right after a new recording is completed -
    // so here's where we should set up the download.
    //audioRecorder.exportWAV( doneEncoding );
  
}


function gotBuffersUI(buffers) {

    var canvas = document.getElementById( "wavedisplay" ); 
    drawBuffer( canvas.width, canvas.height, canvas.getContext('2d'), buffers[0] );

   Shiny.onInputChange("audio", buffers);
 
}

function doneEncoding( blob ) {
    Recorder.setupDownload( blob, "myRecording" + ((recIndex<10)?"0":"") + recIndex + ".wav" );
    recIndex++;
}

function toggleRecording( e ) {
    if (e.classList.contains("recording")) {
        // stop recording
        audioRecorder.stop();
        audioContext.suspend();
        e.classList.remove("recording");
    } else {
        // start recording

        if (!audioRecorder)
            return;
        audioContext.resume();
        e.classList.add("recording");
        audioRecorder.clear();
        audioRecorder.record();
    }
} 

function startRecording( e ) {
    console.log(audioRecorder);
        // start recording
        if (!audioRecorder)
            return;
        audioContext.resume();
        audioRecorder.clear();
        audioRecorder.record();
    }

function stopRecording( e ) {   // stop recording
    audioRecorder.stop();
    audioRecorder.getBuffers(gotBuffers);
    console.log("reached end of stopRecording");
}

function convertToMono( input ) {
    var splitter = audioContext.createChannelSplitter(2);
    var merger = audioContext.createChannelMerger(2);

    input.connect( splitter );
    splitter.connect( merger, 0, 0 );
    splitter.connect( merger, 0, 1 );
    return merger;
}

function cancelAnalyserUpdates() {
    window.cancelAnimationFrame( rafID );
    rafID = null;
}

function updateAnalysers(time) {
    if (!analyserContext) {
        var canvas = document.getElementById("analyser");
        canvasWidth = canvas.width;
        canvasHeight = canvas.height;
        analyserContext = canvas.getContext('2d');
    }

    // analyzer draw code here
    {
        var SPACING = 3;
        var BAR_WIDTH = 1;
        var numBars = Math.round(canvasWidth / SPACING);
        var freqByteData = new Uint8Array(analyserNode.frequencyBinCount);

        analyserNode.getByteFrequencyData(freqByteData);

        analyserContext.clearRect(0, 0, canvasWidth, canvasHeight);
        analyserContext.fillStyle = '#F6D565';
        analyserContext.lineCap = 'round';
        var multiplier = analyserNode.frequencyBinCount / numBars;

        // Draw rectangle for each frequency bin.
        for (var i = 0; i < numBars; ++i) {
            var magnitude = 0;
            var offset = Math.floor( i * multiplier );
            // gotta sum/average the block, or we miss narrow-bandwidth spikes
            for (var j = 0; j< multiplier; j++)
                magnitude += freqByteData[offset + j];
            magnitude = magnitude / multiplier;
            var magnitude2 = freqByteData[i * multiplier];
            analyserContext.fillStyle = "hsl( " + Math.round((i*360)/numBars) + ", 100%, 50%)";
            analyserContext.fillRect(i * SPACING, canvasHeight, BAR_WIDTH, -magnitude);
        }
    }

    rafID = window.requestAnimationFrame( updateAnalysers );
}

function toggleMono() {
    if (audioInput != realAudioInput) {
        audioInput.disconnect();
        realAudioInput.disconnect();
        audioInput = realAudioInput;
    } else {
        realAudioInput.disconnect();
        audioInput = convertToMono( realAudioInput );
    }

    audioInput.connect(inputPoint);
}

function gotStream(stream) {
    inputPoint = audioContext.createGain();

    // Create an AudioNode from the stream.
    realAudioInput = audioContext.createMediaStreamSource(stream);
    audioInput = realAudioInput;
    audioInput.connect(inputPoint);

    audioInput = convertToMono(audioInput);

    analyserNode = audioContext.createAnalyser();
    analyserNode.fftSize = 2048;
    inputPoint.connect( analyserNode );

    audioRecorder = new Recorder( inputPoint );

    zeroGain = audioContext.createGain();
    zeroGain.gain.value = 0.0;
    inputPoint.connect( zeroGain );
    zeroGain.connect( audioContext.destination );
    updateAnalysers();
}

function initAudio() {
        if (!navigator.getUserMedia)
            navigator.getUserMedia = navigator.webkitGetUserMedia || navigator.mozGetUserMedia;
        if (!navigator.cancelAnimationFrame)
            navigator.cancelAnimationFrame = navigator.webkitCancelAnimationFrame || navigator.mozCancelAnimationFrame;
        if (!navigator.requestAnimationFrame)
            navigator.requestAnimationFrame = navigator.webkitRequestAnimationFrame || navigator.mozRequestAnimationFrame;
        
    navigator.getUserMedia(
        {
            "audio": {
                "mandatory": {
                    "googEchoCancellation": "false",
                    "googAutoGainControl": "false",
                    "googNoiseSuppression": "false",
                    "googHighpassFilter": "false"
                },
                "optional": []
            },
        }, gotStream, function(e) {
            alert('Error getting audio');
            console.log(e);
        });

}

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
stopButton.innerText = "Stop"; // Insert text
stopButton.addEventListener("click", stopRecording);
button_area.appendChild(stopButton);
var br = document.createElement("br");
button_area.appendChild(br);
}

function showRecordingIcon() {

var img = document.createElement("img"); 
img.src =  "./img/sing.png"; 
img.width = "280";
img.height = "280";
button_area.appendChild(img);

var br = document.createElement("br");
button_area.appendChild(br);

}

function showLoadingIcon() {

    var img = document.createElement("img"); 
    img.src =  "./img/loading.gif"; 
    img.width = "320";
    img.height = "224";
    loading_area.appendChild(img);
    }


function recordUpdateUI(showStop) {

    // update the recording UI
    // if showStop is true, then give the user the option to press the stop button

  
    hidePlayButton();
    
    if (showStop === true) {
        setTimeout(() => {  showStopButton(); }, 500); // a little lag

    }

    setTimeout(() => {  showRecordingIcon(); }, 500); // a little lag

   
}


function recordAndStop (ms, showStop) {
    // start recording but then stop after x milliseconds
     
    console.log("stop!");

    startRecording();

     if (ms === null) {
        recordUpdateUI(showStop);
     }

     else {
        recordUpdateUI(showStop);
        setTimeout(() => {  stopRecording(); }, ms); 
     }

   }
  


 function  playTone(tone, seconds) {
  // play a tone for x seconds

  tone = Number(tone);
  console.log(tone);

  freq_tone = Tone.Frequency(tone, "midi").toNote();
  console.log(freq_tone);

  synth.triggerAttackRelease(freq_tone, seconds);

  recordAndStop(seconds*1000+500, false);

 }
 

 function playSeq (note_list) {

  //console.log(note_list); // testing
  //note_list.forEach(element => console.log(element)); // testing
 
  midi_list = note_list.map(x => Tone.Frequency(x, "midi").toNote());
  last_note = midi_list[midi_list.length - 1];
  
  var pattern = new Tone.Sequence(function(time, note){
  synth.triggerAttackRelease(note, 0.25);
  //console.log(note);
 
  if (note === last_note) {
  //console.log("finished!");
  recordAndStop(null, true);
  }

  }, midi_list);
 
  
  pattern.start(0).loop = false;
  // begin at the beginning
  Tone.Transport.start();
 
}
   

function toneJSPlay (midi, note_no) {

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

        setTimeout(() => {  recordAndStop(null, true); }, dur); 

        //create a synth for each track
        const synth = new Tone.PolySynth(2, Tone.Synth, synth_params).toMaster();
        synths.push(synth);
        //schedule all of the events
        notes_list.forEach(note => {
        synth.triggerAttackRelease(note.name, note.duration, note.time + now, note.velocity);
        });
    });

}
    
async function midiToToneJS (url, note_no) {

// load a midi file in the browser
const midi = await Midi.fromUrl(url).then(midi => {

    console.log(midi);

    toneJSPlay(midi, note_no);
    
})
}


// Define a function to handle status messages

function playMidiFileAndRecordAfter(url, toneJS, note_no) {
  
    // toneJS: boolean. true if file file to be played via toneJS. otherwise, via MIDIJS
    // note_no, optional no of notes to cap at 


    if (toneJS === true) {
        midiToToneJS(url, note_no);
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
        recordAndStop(null, true);
    } 
        
    });

    }
    }
}


function showHiddenButton (e) {
  e.classList.remove("_hidden");
}


function hideButton (e) {
  e.classList.add("_hidden");
}



