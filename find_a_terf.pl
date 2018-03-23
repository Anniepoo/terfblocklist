:- module(find_a_terf, [query/2]).
:- use_module(library(http/json)).

:- setting(find_a_terf:consumer_key, atom, 'notvalid',
           'Twitter consumer key').
:- setting(find_a_terf:consumer_secret, atom, 'notvalid',
           'Twitter consumer secret').

:- load_settings('settings.db').

consumer_key(K) :-
    setting(find_a_terf:consumer_key, K).
consumer_secret(S) :-
    setting(find_a_terf:consumer_secret, S).

:- use_module(library(twitter)).

get_twitter_token :-
    consumer_secret(Secret),
    consumer_key(Key),
    get_bearer_token(Key,
                     Secret,
                     _ResponseJSON,
                     _Token,
                     _Error).

query(AtomSearch, Names) :-
    token(T),
    make_a_search(AtomSearch,
                  T,
                  Json,
                  Status),
    handle_result(Status, Json, Names).
query(AtomSearch, Names) :-
    \+ token(_),
    get_twitter_token,
    query(AtomSearch, Names).

handle_result(200, Json, Names) :-
    handle_statuses(Json.statuses, Names).
handle_result(Status, _, _) :-
    Status \= 200,
    throw(error(twitter_error(Status))).

prolog:message(twitter_error(Status)) -->
                [ 'Twitter returned status:'-[Status], nl ].

handle_statuses([], []).
handle_statuses([H | T],
                [terf{
                    screen_name: H.user.screen_name,
                    desc: H.user.description,
                    userid: H.user.id,
                    name: H.user.name,
                    tweets: [
                        tweet{
                            tweetid: H.id,
                            text: H.text}
                    ]
                 }
                 | TTerf]
               ) :-
    handle_statuses(T, TTerf).
