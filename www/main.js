console.log("loaded main.js");

//create a synth and connect it to the master output (your speakers)
const synth = new Tone.Synth().toMaster();

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
    hideRecordImage(); showLoadingIcon();
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

function getUserInfo () {
    console.log(navigator);
    var _navigator = {};
    for (var i in navigator) _navigator[i] = navigator[i];
    delete _navigator.plugins;
    delete _navigator.mimeTypes;
    navigatorJSON = JSON.stringify(_navigator);
    console.log(navigatorJSON);
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
var br = document.createElement("br");
stopButton.innerText = "Stop"; // Insert text
stopButton.addEventListener("click", stopRecording);
button_area.appendChild(br);
button_area.appendChild(stopButton);
}

function showRecordingIcon() {

var img = document.createElement("img"); 
img.src =  "./sing.png"; 
img.width = "280";
img.height = "280";
button_area.appendChild(img);
}

function showLoadingIcon() {

    var img = document.createElement("img"); 
    img.src =  "./loading.gif"; 
    img.width = "320";
    img.height = "224";
    loading_area.appendChild(img);
    }


function playSeq (note_list) {

 console.log(note_list); // testing
 note_list.forEach(element => console.log(element)); // testing

 midi_list = note_list.map(x => Tone.Frequency(x, "midi").toNote());
last_note = midi_list[midi_list.length - 1];
 
 var pattern = new Tone.Sequence(function(time, note){
 synth.triggerAttackRelease(note, 0.25);
 console.log(note);

 if (note === last_note) {
 console.log("finished!");

setTimeout(() => {  showRecordingIcon();showStopButton(); }, 1000);
// start recording
 startRecording();
 }
 }, midi_list);

 
 pattern.start(0).loop = false;
 // begin at the beginning
 Tone.Transport.start();

hidePlayButton();


}


function AutoFiveSecondRecord () {

     // start recording
     startRecording(); 
     hidePlayButton();
    
    setTimeout(() => {  showRecordingIcon(); }, 500);
    setTimeout(() => {  stopRecording(); }, 4000);
   
   }

   function recordNoPlayback () {

    // start recording
    startRecording();

    setTimeout(() => {  
        showRecordingIcon();
        showStopButton(); 
        }, 500);
        
        hidePlayButton();
   
   }
  


   function  playTone(tone) {

    tone = Number(tone);
    console.log(tone);

    freq_tone = Tone.Frequency(tone, "midi").toNote();
    console.log(freq_tone);

    synth.triggerAttackRelease(freq_tone, 6);

    // start recording
    startRecording();
    setTimeout(() => {  showRecordingIcon(); }, 500); // after a little pause show the recording icon
    hidePlayButton(); // hide the playbutton
    setTimeout(() => {  stopRecording(); }, 4000); // stop recording after 4 seconds
   }