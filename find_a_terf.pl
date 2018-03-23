:- module(find_a_terf, [query/2,
                       unified_queries/1]).

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
    sleep(1),
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

unified_queries(Terfs) :-
    setof(Phrase, query_phrase(Phrase), Phrases),
    maplist(query, Phrases, NestedTerfs),
    flatten(NestedTerfs, FTerfs),
    predsort(compare_by_userid , FTerfs, SortedTerfs),
    consolidate_tweets(SortedTerfs, Terfs).

query_phrase('"biological men"').
query_phrase('"biological man"').
query_phrase('"thinks he\'s a woman"').


% test ones that find me
%query_phrase('"call out to any trans folx/allies out there who are comfy in javascript and would"').
%query_phrase('"long needed a union. Excited to see"').

% like comparison/3 but uses userid, then text
compare_by_userid(Compare, A, B) :-
    compare(C, A.userid, B.userid),
    ( C = '='
    -> [AT|_] = A.tweets,
       [BT|_] = B.tweets,
       compare(Compare, AT.text, BT.text)
    ;  C = Compare
    ).

consolidate_tweets([] , []).
consolidate_tweets([H|T], Out) :-
    consolidate_tweets(H, T, Out).
consolidate_tweets(Terf, [], [Terf]).
consolidate_tweets(Terf, [H | T], OutList) :-
    Terf.userid = H.userid,
    [HTweet | _] = H.tweets,
    Out = Terf.put(_{tweets: [HTweet | Terf.tweets]}),
    consolidate_tweets(Out, T, OutList).
consolidate_tweets(Terf, [H | T], [Terf | OutList]) :-
    Terf.userid \= H.userid,
    consolidate_tweets(H,  T, OutList).
