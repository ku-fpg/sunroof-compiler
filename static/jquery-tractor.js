// This is our main loop; get a command from the Haskell server,
// execute it, go back and ask for more.
function tractor_redraw(count) {
//                alert("tractor_redraw : " + "/example/act/" + tractor_session + "/" + count);
   $.ajax({ url: "/example/act/" + tractor_session + "/" + count,
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
     $.ajax({ url: prefix,
              type: "POST",
              data: "",
              dataType: "script"}); 
}