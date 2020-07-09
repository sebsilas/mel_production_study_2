console.log("main.js loaded");

var synthParameters = {
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
  };

//create a synth and connect it to the master output (your speakers)
var synth = new Tone.Synth(synthParameters).toMaster();


function playTones (note_list) {

    console.log(note_list); // testing
    note_list.forEach(element => console.log(element)); // testing
   
    note_list = note_list.map(x => Tone.Frequency(x));
   last_note = note_list[note_list.length - 1];
    
    var pattern = new Tone.Sequence(function(time, note){
    synth.triggerAttackRelease(note, 0.25);
    console.log(note);
   
    if (note === last_note) {
    console.log("finished!");
    }
    
    }, note_list);
   
    
    pattern.start(0).loop = false;
    // begin at the beginning
    Tone.Transport.start();
   
   
}
   