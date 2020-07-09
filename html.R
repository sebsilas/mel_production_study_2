
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
  includeScript("www/js/Tonejs-Instruments.js"),
  includeScript("www/js/modernizr-custom.js"),
  shiny::tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js"),
  shiny::tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"),
  includeScript("www/js/main.js"),
)





# footer
html.footer <- div(
  htmltools::includeScript("https://sdk.amazonaws.com/js/aws-sdk-2.2.32.min.js"),
  includeScript("www/js/record.js"),
  shiny::tags$script('
  console.log(p_id);
  var wRegion = "us-east-1";
  var poolid = "us-east-1:9ff91514-5fa9-4820-a30a-d0e9c1f21fa0";
  var s3bucketName = "melody-singing-task-study-2";
  var audioPath = "/audio-files";
  function audio(wRegion,poolid,path) {
    AStream = new AudioStream(wRegion,poolid,path);
    return AStream;
  }
  var NewAudio = audio(wRegion,poolid,s3bucketName+audioPath);
  NewAudio.audioStreamInitialize();
  '))

html.footer2 <- div(
  htmltools::includeScript("https://sdk.amazonaws.com/js/aws-sdk-2.2.32.min.js"),
  shiny::tags$script('
  console.log(p_id);
  var wRegion = "us-east-1";
  var poolid = "us-east-1:9ff91514-5fa9-4820-a30a-d0e9c1f21fa0";
  var s3bucketName = "melody-singing-task-study-2";
  var audioPath = "/audio-files";
  function AudioBuild(wRegion,poolid,path) {
    console.log("AudioBuild called");
    console.log(typeof AudioStream);
    AudioS = new AudioStream(wRegion,poolid,path);
    return AudioS;
  }
  var NewAudio = AudioBuild(wRegion,poolid,s3bucketName+audioPath);
  NewAudio.audioStreamInitialize();
  '))


# alternative footers which put each participant's audio in its own folder
html.footer.alt <- div(
  htmltools::includeScript("https://sdk.amazonaws.com/js/aws-sdk-2.2.32.min.js"),
  includeScript("www/js/record.js"),
  shiny::tags$script('
  var wRegion = "us-east-1";
  var poolid = "us-east-1:9ff91514-5fa9-4820-a30a-d0e9c1f21fa0";
  var s3bucketName = "melody-singing-task-study-2";
  console.log(p_id);
  var audioPath = `/${p_id}/audio-files`;
  function audio(wRegion,poolid,path) {
    AStream = new AudioStream(wRegion,poolid,path);
    return AStream;
  }
  var NewAudio = audio(wRegion,poolid,s3bucketName+audioPath);
  NewAudio.audioStreamInitialize();
  '))

html.footer2.alt <- div(
  htmltools::includeScript("https://sdk.amazonaws.com/js/aws-sdk-2.2.32.min.js"),
  shiny::tags$script('
  var wRegion = "us-east-1";
  var poolid = "us-east-1:9ff91514-5fa9-4820-a30a-d0e9c1f21fa0";
  var s3bucketName = "melody-singing-task-study-2";
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
