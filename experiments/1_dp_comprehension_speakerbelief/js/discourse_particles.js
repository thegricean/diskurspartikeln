// Returns a random integer between min (included) and max (excluded)
// Using Math.round() will give you a non-uniform distribution!
function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min)) + min;
}

function caps(a) {return a.substring(0,1).toUpperCase() + a.substring(1,a.length);}

function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.cause_effect_prior = slide({
    name : "cause_effect_prior",
    present : exp.all_stims,
    start : function() {
	$(".err").hide();
      $(".slidererr").hide();	
    },
      present_handle : function(stim) {
    	this.trial_start = Date.now();
	    $(".sliderbutton").show();    	
	//$("#eventdescription").val("");	
      this.init_sliders();
      exp.sliderPost = {};
      $("#number_guess").html("?");
	  this.stim = stim;
	  console.log(this.stim);
	var contextsentence = "Sie sind in einem fensterlosen Raum.";
	var description = stim.dein + " " + stim.freund + " " + stim.name + " kommt rein und sagt: ";
	var sentence = stim.sentence;
	var prompt = "Glaubt "+ stim.name +", dass "+stim.complement+"?";
	var evidence = "Wie, denken Sie, kommt "+stim.name+" zu "+stim.gen+" Wissen über "+stim.evidence_question+"?";
	var evidenceorder = _.shuffle(["evidence1","evidence2","evidence3","evidence4","evidence5"]);
	console.log(evidenceorder);	


	$("#contextsentence").html(contextsentence);
	$("#description").html(description);
	  $("#sentence").html('"'+sentence+'"');
	$("#prompt").html(prompt);				  
	$("#evidence").html(evidence);
	
	var radiotablehtml = "";
	var pronoun = stim.nom;

      for (var i=1; i<6; i++) {
		if (stim.item == "abendessen") { 
			if (evidenceorder[i-1] == "evidence2") {
				if (stim.nom == "er") {
					pronoun = "seine Gattin";
				} else {
					pronoun = "ihr Gatte";					
				}
			} else { pronoun = stim.nom; }
		}
      	
		radiotablehtml = radiotablehtml + '<tr><td align="left"><input type="radio" name="radioresponse" class="radio'+i+'" value="'+evidenceorder[i-1]+'"/><label for="radio'+i+'">'+caps(pronoun)+' '+stim[evidenceorder[i-1]]+'.</label></td></tr>';	
      }
				
      
	  $("#radiotable").html(radiotablehtml);
	  $("#evidence").hide();
	  $("#radiotable").hide();
	  $(".contbutton").hide();
	
        $(".sliderbutton").click(function() {
	  console.log(exp.sliderPost["mainslider"]);
	if (exp.sliderPost["mainslider"] > -1) {
	    $(".sliderbutton").unbind("click"); 
	    $(".sliderbutton").hide();
	    $(".slidererr").hide();
	    $("#evidence").show();
	    $("#radiotable").show();
	    $(".contbutton").show();
      } else {
        $(".slidererr").show();
      }
	});
      
	  console.log(this);
        $(".contbutton").click(function() {
	  var ok_to_go_on = true;
		console.log($("input[name=radioresponse]:checked").val());
	  if ($("input[name=radioresponse]:checked").val() == undefined) {
	  	ok_to_go_on = false;
	  }
      if (ok_to_go_on) {
	$(".contbutton").unbind("click");      	
	stim.evidenceexplanation = $("input[name=radioresponse]:checked").val();         	
        exp.data_trials.push({
          "item_type" : stim.item_type,
          "slide_number_in_experiment" : exp.phase,
          "sentence": stim.sentence,
          "item": stim.item,
          "name": stim.name,
          "gender" : stim.gender,
            "rt" : Date.now() - _s.trial_start,
          "response_type" : "speakerbelief",            
	    "response" : exp.sliderPost["mainslider"]
        });
        exp.data_trials.push({
          "item_type" : stim.item_type,
          "slide_number_in_experiment" : exp.phase,
          "sentence": stim.sentence,
          "item": stim.item,
          "name": stim.name,
          "gender" : stim.gender,
            "rt" : Date.now() - _s.trial_start,
          "response_type" : "evidence",            
	    "response" : stim.evidenceexplanation
        });        
          $(".err").hide();
          _stream.apply(_s); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
	});
	  
      },

      init_sliders : function() {
      var slider_ids = ["mainslider"];
      for (var i=0; i<slider_ids.length; i++) {
        var slider_id = slider_ids[i];
        utils.make_slider("#slider_" + slider_id,
          function(which_slider_id_is_this) {
            return function(event, ui) {
              exp.sliderPost[which_slider_id_is_this] = ui.value;
            };
          }(slider_id) //wraps up index variable slider_id
        )
      }
    },

  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        eDucation : $("#eDucation").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {

  var items = _.shuffle([
    {
	  "context": "Sie sind in einem fensterlosen Raum.",
	  "bare" : "Es hat geregnet.",
	  "muss" : "Es muss geregnet haben.",
	  "wohl" : "Es hat wohl geregnet.",	  
	  "vermutlich" : "Es hat vermutlich geregnet.",	  
	  "item" : "regen",
	"complement" : "es geregnet hat",
	"evidence_question" : "den Regen",
	"evidence1" : "hatte aus dem Fenster gesehen und beobachtet, wie Regentropfen vom Himmel fielen",
	"evidence2" : "konnte hören, wie Wasser auf das Dach prasselte",
	"evidence3" : "hat gesehen, wie jemand mit nassen Haaren und durchnässten Kleidern von draußen hereinkam",	
	"evidence4" : "hat im Internet den Wetterbericht gelesen, in dem stand, dass es regnen würde",
	"evidence5" : "hat heute Vormittag dunkle Wolken am Himmel gesehen"
    },
    {
	  "context": "Sie sind in einem fensterlosen Raum.",
	  "bare" : "Das Abendessen ist fertig geworden.",
	  "muss" : "Das Abendessen muss fertig geworden sein.",
	  "wohl" : "Das Abendessen ist wohl fertig geworden.",	  
	  "vermutlich" : "Das Abendessen ist vermutlich fertig geworden.",	  
	"item" : "abendessen",
	  "complement" : "das Abendessen fertig geworden ist",
	"evidence_question" : "das Abendessen",
	"evidence1" : "hat gerade das Abendessen zubereitet und auf den Tisch gestellt",
	"evidence2" : "hat gesagt, dass das Abendessen fertig sei",
	"evidence3" : "weiß, dass das Abendessen normalerweise um 18 Uhr fertig ist. Ein Blick auf die Uhr zeigt, dass es gerade 18 Uhr ist",	
	"evidence4" : "vernimmt den Geruch von Essen, der aus dem Esszimmer kommt",
	"evidence5" : "hat Hunger"	
    },
    {
	  "context": "Sie sind in einem fensterlosen Raum.",
	  "bare" : "Der Kaffee ist kalt geworden.",
	  "muss" : "Der Kaffee muss kalt geworden sein.",
	  "wohl" : "Der Kaffee ist wohl kalt geworden.",
	  "vermutlich" : "Der Kaffee ist vermutlich kalt geworden.",	  
	"item" : "kaffee",
	  "complement" : "der Kaffee kalt geworden ist",
	"evidence_question" : "den Kaffee",
	"evidence1" : "hat einen Schluck Kaffee getrunken und festgestellt, dass er kalt war",
	"evidence2" : "hat die Kaffeetasse berührt und festgestellt, dass sie kalt war",
	"evidence3" : "wusste, dass der Kaffee seit einer Stunde auf dem Tisch stand",
	"evidence4" : "hat gesehen, dass aus dem Kaffee kein Dampf aufstieg",
	"evidence5" : "hat gesehen, dass die Tasse nicht isoliert war"
		
    },
    {
	  "context": "Sie sind in einem fensterlosen Raum.",
	  "bare" : "Der Nachbarshund hat gebellt.",
	  "muss" : "Der Nachbarshund muss gebellt haben.",
	  "wohl" : "Der Nachbarshund hat wohl gebellt.",	  
	  "vermutlich" : "Der Nachbarshund hat vermutlich gebellt.",	  
	"item" : "hund",
	  "complement" : "der Nachbarshund hat gebellt",
	"evidence_question" : "den Hund",
	"evidence1" : "schaute aus dem Fenster und sah Struppi, den Hund der Nachbarn, wie er am Zaun stand und bellte",
	"evidence2" : "hat einen Hund bellen gehört",
	"evidence3" : "hatte Kopfhörer auf und hörte Musik, sah aber aus dem Fenster und beobachtete, wie der Postbote vor der Nachbarstür einen Satz nach hinten machte",			
	"evidence4" : "hatte Kopfhörer auf und hörte Musik, weiß aber, dass der Hund der Nachbarn abends oft bellt",
	"evidence5" : "weiß, dass sich die Nachbarn gerade einen Hund angeschafft haben"
    }      
  ]);

  var names = _.shuffle([
    {
      "name":"Robert",
      "gender":"M"
    },
    {
      "name":"Michael",
      "gender":"M"
    },
    {
      "name":"David",
      "gender":"M"
    },
    {
      "name":"Joseph",
      "gender":"M"
    },
    {
      "name":"Thomas",
      "gender":"M"
    },
    {
      "name":"Christoph",
      "gender":"M"
    },
    {
      "name":"Daniel",
      "gender":"M"
    },
    {
      "name":"Paul",
      "gender":"M"
    },
    {
      "name":"Mark",
      "gender":"M"
    },
    {
      "name":"Georg",
      "gender":"M"
    },
    {
      "name":"Stefan",
      "gender":"M"
    },
    {
      "name":"Andreas",
      "gender":"M"
    },
    {
      "name":"Kevin",
      "gender":"M"
    },
    {
      "name":"Tim",
      "gender":"M"
    },
    {
      "name":"Jan",
      "gender":"M"
    },
    {
      "name":"Jens",
      "gender":"M"
    },
    {
      "name":"Nico",
      "gender":"M"
    },
    {
      "name":"Erik",
      "gender":"M"
    },
    {
      "name":"Jakob",
      "gender":"M"
    },
    {
      "name":"Jonathan",
      "gender":"M"
    },
    {
      "name":"Frank",
      "gender":"M"
    },
    {
      "name":"Christian",
      "gender":"M"
    },
    {
      "name":"Samuel",
      "gender":"M"
    },
    {
      "name":"Benjamin",
      "gender":"M"
    },
    {
      "name":"Patrick",
      "gender":"M"
    },
    {
      "name":"Mario",
      "gender":"M"
    },
    {
      "name":"Dennis",
      "gender":"M"
    },
    {
      "name":"Alex",
      "gender":"M"
    },
    {
      "name":"Maria",
      "gender":"F"
    },
    {
      "name":"Jenny",
      "gender":"F"
    },
    {
      "name":"Elisabeth",
      "gender":"F"
    },
    {
      "name":"Linda",
      "gender":"F"
    },
    {
      "name":"Emilia",
      "gender":"F"
    },
    {
      "name":"Susanne",
      "gender":"F"
    },
    {
      "name":"Jessika",
      "gender":"F"
    },
    {
      "name":"Sarah",
      "gender":"F"
    },
    {
      "name":"Karina",
      "gender":"F"
    },
    {
      "name":"Bettina",
      "gender":"F"
    },
    {
      "name":"Lisa",
      "gender":"F"
    },
    {
      "name":"Sandra",
      "gender":"F"
    },
    {
      "name":"Helena",
      "gender":"F"
    },
    {
      "name":"Kim",
      "gender":"F"
    },
    {
      "name":"Caroline",
      "gender":"F"
    },
    {
      "name":"Michelle",
      "gender":"F"
    },
    {
      "name":"Melissa",
      "gender":"F"
    },
    {
      "name":"Melanie",
      "gender":"F"
    },
    {
      "name":"Laura",
      "gender":"F"
    },
    {
      "name":"Steffi",
      "gender":"F"
    },
    {
      "name":"Rebecca",
      "gender":"F"
    },
    {
      "name":"Cynthia",
      "gender":"F"
    },
    {
      "name":"Katharina",
      "gender":"F"
    },
    {
      "name":"Anna",
      "gender":"F"
    },
    {
      "name":"Angela",
      "gender":"F"
    },
    {
      "name":"Kathrin",
      "gender":"F"
    },
    {
      "name":"Nicole",
      "gender":"F"
    },
    {
      "name":"Christina",
      "gender":"F"
    },
    {
      "name":"Diana",
      "gender":"F"
    },
    {
      "name":"Eva",
      "gender":"F"
    },
    {
      "name":"Julie",
      "gender":"F"
    },
    {
      "name":"Judith",
      "gender":"F"
    }
  ]);

  var item_type = _.shuffle(["bare", "muss", "wohl", "vermutlich"]);

  function makeStim(i) {
    //get item
    var item = items[i];
    //get name
    var name_data = names[i];
    var name = name_data.name;
    var gender = name_data.gender;

    //get sentence
      var sentence = item[item_type[i]];
    //get pronouns
    var nom = gender == "M" ? "er" : "sie";
    var acc = gender == "M" ? "ihn" : "sie";
    var gen = gender == "M" ? "seinem" : "ihrem";
    var friend = gender == "M" ? "Freund" : "Freundin";
    var your = gender == "M" ? "Ihr" : "Ihre";        
    //get cause and effect elements
    var item_id = item.item;
      var complement = item.complement;
      var evidence_question = item.evidence_question;
      
      return {
	  "item_type": item_type[i],
	  "sentence":sentence,
      "name": name,
      "gender": gender,
      "sentence": sentence,
      "item": item_id,
      "nom": nom,
      "acc": acc,
      "gen": gen,
      "freund": friend,
      "dein": your,            
      "complement": complement,
      "evidence_question": evidence_question,
      "evidence1": item.evidence1,
      "evidence2": item.evidence2,
      "evidence3": item.evidence3,
      "evidence4": item.evidence4,
      "evidence5": item.evidence5                        
    }
  }
  exp.all_stims = [];
  for (var i=0; i<items.length; i++) {
    exp.all_stims.push(makeStim(i));
  }

  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = {}; //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "cause_effect_prior", 'subj_info', 'thanks'];
  
  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
  $(".nQs").html(exp.nQs);

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}
