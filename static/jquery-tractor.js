// Tractor is our workhorse 

var the_prefix = "";
// This is our main loop; get a command from the Haskell server,
// execute it, go back and ask for more.
function tractor_redraw(count) {
//                alert("tractor_redraw : " + "/example/act/" + tractor_session + "/" + count);
   $.ajax({ url: the_prefix + "/act/" + tractor_session + "/" + count,
            type: "GET",
            dataType: "script",
            success: function success() {
                        tractor_redraw(count + 1);
            }
            // TODO: Add failure
          }); 
}

function tractor_connect(prefix) {
     // start the server-side via tractor
     the_prefix = prefix;
     $.ajax({ url: the_prefix,
              type: "POST",
              data: "",
              dataType: "script"}); 
}

var eventQueues = {};	// TODO: add the use of the queue
var eventCallbacks = {};

// This says someone is listening on a specific event
function tractor_register(eventname, fn) {
     eventQueues[eventname] = [];

     $("body").on(eventname, "." + eventname, function (event) {
	var e = fn(event,this);
	e.eventname = eventname;
//	alert("EVENT " + e);
	if (eventCallbacks[eventname] == undefined) {
//		alert('pushing, no one is waiting (TO BE DONE)');
	// These are effectively ignored (TODO: fix)
		eventQueues[eventname].push(e);
	} else {
		eventCallbacks[eventname](e);
	}
     });

}

// This waits for an event. The second argument is the continuation
function tractor_waitFor(eventname, fn) {
   // TODO: check to see if there is an event waiting
   if (eventCallbacks[eventname] == undefined) {
       eventCallbacks[eventname] = function (e) { 
	  // delete the callback
	  delete eventCallbacks[eventname];
	  // and do the callback
  	  fn(e);
       }
   } else {
	alert("ABORT: reassigning the event queue callback");
   }
}

// There is a requirement that obj be an object or array.
// See RFC 4627 for details.
function tractor_reply(uq,obj) {
	$.ajax({ url: the_prefix + "/reply/" + tractor_session + "/" + uq,
                 type: "POST",
                 data: $.toJSON(obj),
                 contentType: "application/json; charset=utf-8",
                 dataType: "json"});
}

function Y(body){
//	alert("Y");
	body(function(k){Y(body);})
}

/// Extra stuff

function getContext(can) {
//	alert("got to get context ");
//	alert("arg = " + can);
        var canvas = document.getElementById(can);
        if (canvas.getContext) {
                        return canvas.getContext("2d");
        }
        alert("no canvas");
}

function waitForS(eventname) {
//	alert('waitForS');
	return function(k){
//		alert("wait CC");
		tractor_waitFor(eventname, k);
	}
}

function ugg() {
	var v1 = getContext("my-canvas");
	(v1).moveTo(50,50);
	(v1).lineTo(150,150);
	(v1).lineWidth = (10);
	(v1).strokeStyle = ("red");
	(v1).stroke();
/*
	(function(k){((function(k){alert("ABC");k();}))
	(function(){(function(k){((function(k){k(getContext("my-canvas"))}))
	(function(v1){(function(k){((function(k){alert(("CDE:")+(v1));k();}))
	(function(){(function(k){((function(k){(v1).moveTo(99,99);k();}))
	(function(){(function(k){((function(k){alert("4:");k();}))
	(function(){(function(k){((function(k){(v1).lineTo(99,99);k();}))
	(function(){(function(k){((function(k){(v1).lineWidth = (99);k();}))
	(function(){(function(k){((function(k){(v1).strokeStyle = ("red");k();}))
	(function(){(function(k){((function(k){(v1).stroke();k();}))
	(function(){((function(k){alert("9:");
	
	k();}))(k)})})(k)})})(k)})})(k)})})(k)})})(k)})})(k)})})(k)})})(k)})})(function(k){})
*/
}