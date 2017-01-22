

:- ensure_loaded(library(slack_client)).

% slack_send(WsOutput,Data):- is_dict(Data),!,json_write_dict(WsOutput,Data),json_write_dict(user_error,Data),!.

%  slack_get_websocket(WS),format(WS,'~w~n',[Data]).

%  curl -X POST --data-urlencode 'payload={"channel": "logicmoo", "username": "@prologmud_connection", "text": "This isosted to #general and comes from a bot named webhookbot.", "icon_emoji": ":ghost:"}' https://hooks.slack.com/services/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

% curl -X POST --data-urlencode 'payload={"channel":"D3U47CE4W", "username":  "@prologmud_connection" , "text":to #general and comes from a bot named webhookbot.", "icon_emoji": ":ghost:"}' https://hooks.slack.com/services/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



:- listing(slack_websocket/3).



:- if(( \+ (is_thread_running(slack_start_listener)))).
:- thread_create(slack_start_listener,_,[alias(slack_start_listener)]).
:- endif.

:- if(( \+ (is_thread_running(slack_start_listener)))).
:- slack_start_listener.
:- endif.

