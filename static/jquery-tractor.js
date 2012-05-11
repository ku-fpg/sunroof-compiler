// This is our main loop; get a command from the Haskell server,
// execute it, go back and ask for more.
var the_prefix = "";
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

function tractor_register(eventname, fn) {
     $("." + eventname).on(eventname, function (event) {
//	var o = new Object();
//	fn(event,this,o);
	$.ajax({ url: the_prefix + "/event/" + tractor_session + "/" + eventname,
                 type: "POST",
                 data: $.toJSON(fn(event,this)),
                 contentType: "application/json; charset=utf-8",
                 dataType: "json"});
     });
}


// There is a requirement that obj be an object or array.
// See RFC 4627 for details.
function tractor_reply(obj) {
	$.ajax({ url: the_prefix + "/event/" + tractor_session + "/reply",
                 type: "POST",
                 data: $.toJSON(obj),
                 contentType: "application/json; charset=utf-8",
                 dataType: "json"});
}