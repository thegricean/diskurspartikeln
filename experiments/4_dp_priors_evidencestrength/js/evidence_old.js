var NUM_SLIDERS = 1;

function numberWithCommas(x) {
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function showSlide(id) {
  $(".slide").hide();
  $("#"+id).show();
}

function random(a,b) {
  if (typeof b == "undefined") {
    a = a || 2;
    return Math.floor(Math.random()*a);
  } else {
    return Math.floor(Math.random()*(b-a+1)) + a;
  }
}

function shuffle(array) {
  var currentIndex = array.length
    , temporaryValue
    , randomIndex
    ;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {

    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}

function clearForm(oForm) {
  var sliderVar = "";
  for(var i=1; i<=NUM_SLIDERS; i++)
  {
    sliderVar = "#slider" + i;
    $(sliderVar).slider("value", 0.5);
    $(sliderVar).css({"background":"#FFFFFF"});
    $(sliderVar + " .ui-slider-handle").css({
        "background":"#FAFAFA",
        "border-color": "#CCCCCC" });
    sliderVar = "slider" + i;
    document.getElementById(sliderVar).style.background = "";
  }
  
  var elements = oForm.elements; 
  
  oForm.reset();

  for(var i=0; i<elements.length; i++) {
    field_type = elements[i].type.toLowerCase();
    switch(field_type) {
    
      case "text": 
      case "password": 
      case "textarea":
            case "hidden":  
        
        elements[i].value = ""; 
        break;
          
      case "radio":
      case "checkbox":
          if (elements[i].checked) {
            elements[i].checked = false; 
        }
        break;
  
      case "select-one":
      case "select-multi":
                  elements[i].selectedIndex = -1;
        break;
  
      default: 
        break;
    }
  }
}

Array.prototype.random = function() {
  return this[random(this.length)];
}

function setQuestion(array) {
    var i = random(0, array.length - 1);
    var q = array[i];
    return q;
}

function shuffledArray(arrLength)
{
  var j, tmp;
  var arr = new Array(arrLength);
  for (i = 0; i < arrLength; i++)
  {
    arr[i] = i;
  }
  for (i = 0; i < arrLength-1; i++)
  {
    j = Math.floor((Math.random() * (arrLength - 1 - i)) + 0.99) + i;
    tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }
  return arr;
}

function shuffledSampleArray(arrLength, sampleLength)
{
  var arr = shuffledArray(arrLength);
  var beginIndex = Math.floor(Math.random() * (arrLength-sampleLength+1));
  return arr.slice(beginIndex, beginIndex+sampleLength);
}

function getRadioCheckedValue(formNum, radio_name)
{
   var oRadio = document.forms[formNum].elements[radio_name];
   for(var i = 0; i < oRadio.length; i++)
   {
      if(oRadio[i].checked)
      {
         return oRadio[i].value;
      }
   }
   return '';
}

function randomizeSharpOffset()
{
  
  var r = Math.floor((Math.random()*6)+1);
  if (r < 4) { return r; }
  else { return 3-r; }
  /*
  var r = Math.floor((Math.random()*3)+1);
  return r;
  */
}

//var quantifiersArray = ["some", "all"];

var allConditions = 
[
[
{"domain": "rain", "scenario": "Imagine that you are sitting in a room.", 
"evidence": 
["You look out the window and see raindrops falling from the sky.", 
"You hear the sound of water dripping on the roof.",
"You check the weather report on the Internet, which says it is raining.",
"You see a person come in from outside with wet hair and wet clothes.",
"Earlier today, you had seen dark clouds in the sky."
],
"proposition": "it is raining"},
{"domain": "coffee", "scenario": "Imagine that there is a cup of coffee on the table in front of you.",
"evidence": 
["You take a sip of the coffee and feel that it is cold.",
"You touch the coffee cup and feel that it is cold.",
"You see that there is no steam coming from the coffee.",
"You know that the coffee has been on the table for an hour.",
"You see that the cup isn't insulated."
],
"proposition": "the coffee is cold"
},
{"domain": "dog", "scenario": "Imagine that you are sitting in your living room in the evening.",
"evidence": 
["You look outside and see Fluffy, the neighbor's dog, standing on the porch and barking.",
"You hear the sound of a dog barking.",
"You are listening to music with your earphones. You know that your neighbor's dog often barks in the evening.",
"You are listening to music with your earphones. You look out the window and see that the mailman has just arrived at your neighbor's doorstep, when all of a sudden he jumps back.",
"Your neighbor just got a new dog."
],
"proposition": "the neighbor's dog is barking"
},
{"domain": "dinner", "scenario": "Imagine that you are at home.",
"evidence": 
["You just prepared dinner and set it out on the table.",
"Your spouse tells you that dinner is ready.",
"Dinner is usually ready at around 6pm. You look at the clock and it is 6pm.",
"You smell food coming from the dining room.",
"You're hungry."
],
"proposition": "dinner is ready"
}
//{"sentenceType": "necessary", "sentence": "It must be raining."},
//{"sentenceType": "likely", "sentence": "It's probably raining."},
//{"sentenceType": "possible", "sentence": "It might be raining."},
]
];

var allNames = ["Ann", "Barbara", "Cathy", "Diana", "Emma", "Fiona", "Grace", "Heather", "Iris", "Jane", "Kathy",
"Lena", "Mary", "Nancy", "Olga", "Patty", "Rebecca", "Stephanie", "Tracy", "Victoria", "Wendy", "Yvonne", 
"Albert", "Bob", "Calvin", "David", "Edward", "Frank", "George", "Henry", "Ivan", "Jake", "Kevin", "Larry",
"Matt", "Nathan", "Oliver", "Patrick", "Robert", "Steven", "Tom", "Victor", "Winston", "Zack"];
allNames = shuffle(allNames);

var debug = false;
if(debug) { allConditions = debugConditions; }

var numConditions = allConditions.length;
var chooseCondition = random(0, numConditions-1);
var allTrialOrders = allConditions[chooseCondition];
var numTrials = allTrialOrders.length;
var shuffledOrder = shuffledSampleArray(allTrialOrders.length, numTrials);
var currentTrialNum = 0;
var trial;
var numComplete = 0;

showSlide("instructions");
$("#trial-num").html(numComplete);
$("#total-num").html(numTrials);


var experiment = {
  condition: chooseCondition + 1,
  domains: new Array(numTrials),
  evidences: new Array(numTrials),
  probs: new Array(numTrials),
  
  orders: new Array(numTrials),

  gender: "",
  age:"",
  income:"",
  nativeLanguage:"",
  comments:"",

  description: function() {
    showSlide("description");
    $("#tot-num").html(numTrials);  
  },
  end: function() {
    var gen = getRadioCheckedValue(1, "genderButton");
    var ag = document.age.ageRange.value;
    var lan = document.language.nativeLanguage.value;
    var comm = document.comments.input.value;
    var incomeVal = document.income.incomeRange.value;
    experiment.gender = gen;
    experiment.age = ag;
    experiment.nativeLanguage = lan;
    experiment.comments = comm;
    experiment.income = incomeVal;
    clearForm(document.forms[1]);
    clearForm(document.forms[2]);
    clearForm(document.forms[3]);
    clearForm(document.forms[4]);
    //clearForm(document.forms[5]);
    //clearForm(document.forms[6]);    
    showSlide("finished");
    setTimeout(function() {turk.submit(experiment) }, 1500);
  },
  next: function() {
    if (numComplete > 0) {
      
      experiment.probs[currentTrialNum] = parseFloat(document.getElementById("hiddenSliderValue1").value);
      //experiment.evidenceRatings[currentTrialNum] = parseFloat(document.getElementById("hiddenSliderValue2").value);
      
      experiment.orders[currentTrialNum] = numComplete;
      //experiment.preciseEatenQuants[currentTrialNum] = document.getElementById("preciseEatenQuant").innerHTML;
          
      clearForm(document.forms[0]);
      clearForm(document.forms[1]);
    }
    if (numComplete >= numTrials) {
      $('.bar').css('width', (200.0 * numComplete/numTrials) + 'px');
      $("#trial-num").html(numComplete);
      $("#total-num").html(numTrials);
      showSlide("askInfo");
    } else {
      $('.bar').css('width', (200.0 * numComplete/numTrials) + 'px');
      $("#trial-num").html(numComplete);
      $("#total-num").html(numTrials);
      currentTrialNum = numComplete;

      trial = allTrialOrders[shuffledOrder[numComplete]];
      
      //experiment.literalQuantifiers[numComplete] = trial.literalQuantifier;

      showSlide("stage");
      name = allNames.shift();
      evidence = shuffle(trial.evidence).shift();
      $("#scenario").html(trial.scenario);
      $("#evidence").html(evidence);
      $("#proposition").html(trial.proposition);
      $('#slider1 .ui-slider-handle').hide();
      $('#slider2 .ui-slider-handle').hide();
      experiment.domains[currentTrialNum] = trial.domain;
      experiment.evidences[currentTrialNum] = evidence;

      numComplete++;
    }
  }
}


$("#slider1").slider({
               animate: true,
               //$("#slider1 .ui-slider-handle").empty();
               max: 1 , min: 0, step: 0.01, value: 0.5,
               //$("#slider1").empty();
               //$("#slider1 .ui-slider-handle").hide();
               create: function( event, ui) {
                    $(" .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#slider1 .ui-slider-handle').show();
                   $("#slider1 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",

                      "opacity": 1
                   });
               },
               
               change: function( event, ui ) {
                   $('#hiddenSliderValue1').attr('value', ui.value);
                   $("#slider1").css({"background":"#99D6EB"});
                   //$("#slider1 .ui-slider-handle").show();
                   $("#slider1 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});

$("#slider2").slider({
               animate: true,
               
               max: 1 , min: 0, step: 0.01, value: 0.5,
               create: function( event, ui) {
                    $("#slider2 .ui-slider-handle").css({
                      "opacity": 0
                   });
               },
               slide: function( event, ui ) {
                $('#slider2 .ui-slider-handle').show();
                   $("#slider2 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29",
                      "opacity": 1
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue2').attr('value', ui.value);
                   $("#slider2").css({"background":"#99D6EB"});
                   $("#slider2 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29",
                     "opacity": 1
                   });
               }});
$("#slider3").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider3 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue3').attr('value', ui.value);
                   $("#slider3").css({"background":"#99D6EB"});
                   $("#slider3 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider4").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider4 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue4').attr('value', ui.value);
                   $("#slider4").css({"background":"#99D6EB"});
                   $("#slider4 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider5").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider5 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue5').attr('value', ui.value);
                   $("#slider5").css({"background":"#99D6EB"});
                   $("#slider5 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider6").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider6 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue6').attr('value', ui.value);
                   $("#slider6").css({"background":"#99D6EB"});
                   $("#slider6 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});
$("#slider7").slider({
               animate: true,
               
               max: 40 , min: 0, step: 1, value: 20,
               slide: function( event, ui ) {
                   $("#slider7 .ui-slider-handle").css({
                      "background":"#E0F5FF",
                      "border-color": "#001F29"
                   });
               },
               change: function( event, ui ) {
                   $('#hiddenSliderValue7').attr('value', ui.value);
                   $("#slider7").css({"background":"#99D6EB"});
                   $("#slider7 .ui-slider-handle").css({
                     "background":"#667D94",
                     "border-color": "#001F29" });
               }});



