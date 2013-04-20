-module(http_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init({_Any, http}, Req, State) ->
  {ok, Req, State}.

handle(Req, {port, Port} = State) ->
  {ok, Req2} = cowboy_req:reply(
    200, [{<<"Content-Type">>, <<"text/html">>}], html(Port), Req
  ),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

html(Port) ->
  BinPort = list_to_binary(integer_to_list(Port)),

  [<<"<!DOCTYPE html>
<html>
    <head>
        <meta charset=\"utf-8\">
        <title>WebSockets - Simple chat</title>

        <style>
        * { font-family:tahoma; font-size:12px; padding:0px; margin:0px; }
        p { line-height:18px; }
        div { width:500px; margin-left:auto; margin-right:auto;}
        #content { padding:5px; background:#ddd; border-radius:5px;
                   border:1px solid #CCC; margin-top:10px; }
        #input { border-radius:2px; border:1px solid #ccc;
                 margin-top:10px; padding:5px; width:400px;  }
        #status { width:88px; display:block; float:left; margin-top:15px; }
        </style>
    </head>
    <body>
        <div id=\"server\"></div>
        <div id=\"content\"></div>
        <div>
            <span id=\"status\">Connecting...</span>
            <input type=\"text\" id=\"input\" disabled=\"disabled\" />
        </div>

        <script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js\"></script>
        <script type=\"text/javascript\">
$(function () {
    \"use strict\";

    // for better performance - to avoid searching in DOM
    var content = $('#content');
    var input = $('#input');
    var status = $('#status');

    // my color assigned by the server
    var myColor = false;
    // my name sent to the server
    var myName = false;

    // if user is running mozilla then use it's built-in WebSocket
    window.WebSocket = window.WebSocket || window.MozWebSocket;

    // if browser doesn't support WebSocket, just show some notification and exit
    if (!window.WebSocket) {
        content.html($('<p>', { text: 'Sorry, but your browser doesn\\'t '
                                    + 'support WebSockets.'} ));
        input.hide();
        $('span').hide();
        return;
    }

    // open connection
    var port = ">>, BinPort, <<";
    var server = 'ws://127.0.0.1:' + port + '/websocket';
    $('#server').text(server);

    var connection = new WebSocket(server);

    connection.onopen = function () {
        // first we want users to enter their names
        input.removeAttr('disabled');
        status.text('Choose name:');
    };

    connection.onerror = function (error) {
        // just in there were some problems with conenction...
        content.html($('<p>', { text: 'Sorry, but there\\'s some problem with your '
                                    + 'connection or the server is down.</p>' } ));
    };

    // most important part - incoming messages
    connection.onmessage = function (message) {
        // try to parse JSON message. Because we know that the server always returns
        // JSON this should work without any problem but we should make sure that
        // the massage is not chunked or otherwise damaged.
        try {
            var json = JSON.parse(message.data);
        } catch (e) {
            console.log('This doesn\\'t look like a valid JSON: ', message.data);
            return;
        }

        // NOTE: if you're not sure about the JSON structure
        // check the server source code above
        if (json.type === 'color') { // first response from the server with user's color
            myColor = json.data;
            status.text(myName + ': ').css('color', myColor);
            input.removeAttr('disabled').focus();
            // from now user can start sending messages
        } else if (json.type === 'history') { // entire message history
            // insert every single message to the chat window
            for (var i=0; i < json.data.length; i++) {
                addMessage(json.data[i].author, json.data[i].text,
                           json.data[i].color, new Date(json.data[i].time));
            }
        } else if (json.type === 'message') { // it's a single message
            input.removeAttr('disabled'); // let the user write another message
            addMessage(json.data.author, json.data.text,
                       json.data.color, new Date(json.data.time));
        } else {
            console.log('Hmm..., I\\'ve never seen JSON like this: ', json);
        }
    };

    /**
     * Send mesage when user presses Enter key
     */
    input.keydown(function(e) {
        if (e.keyCode === 13) {
            var msg = $(this).val();
            if (!msg) {
                return;
            }
            // send the message as an ordinary text
            connection.send(msg);
            $(this).val('');
            // disable the input field to make the user wait until server
            // sends back response
            input.attr('disabled', 'disabled');

            // we know that the first message sent from a user their name
            if (myName === false) {
                myName = msg;
            }
        }
    });

    /**
     * This method is optional. If the server wasn't able to respond to the
     * in 3 seconds then show some error message to notify the user that
     * something is wrong.
     */
    setInterval(function() {
        if (connection.readyState !== 1) {
            status.text('Error');
            input.attr('disabled', 'disabled').val('Unable to comminucate '
                                                 + 'with the WebSocket server.');
        }
    }, 3000);

    /**
     * Add message to the chat window
     */
    function addMessage(author, message, color, dt) {
        content.append('<p><span style=\"color:' + color + '\">' + author + '</span> @ ' +
             + (dt.getHours() < 10 ? '0' + dt.getHours() : dt.getHours()) + ':'
             + (dt.getMinutes() < 10 ? '0' + dt.getMinutes() : dt.getMinutes())
             + ': ' + message + '</p>');
    }
});
        </script>
    </body>
</html>">>].