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

     $("." + eventname).on(eventname, function (event) {
	var e = fn(event,this);
	e.eventname = eventname;
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