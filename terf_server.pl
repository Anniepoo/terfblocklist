:- module(terf_server, [
              go/0
          ]).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_files)).
:- use_module(find_a_terf).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(js, '/js', []).
http:location(css, '/css', []).
http:location(img, '/img', []).
user:file_search_path(css, './css').
user:file_search_path(js, './js').
user:file_search_path(icons, './icons').

:- html_resource(style, [virtual(true), requires([css('style.css')]), mime_type(text/css)]).
:- html_resource(script, [virtual(true), requires([js('makelist.js')]), mime_type(text/javascript)]).

go :-
    http_set_session_options([timeout(3600)]),
    http_server(http_dispatch, [port(6070)]).

:- http_handler(/, logged_in_page(home_page), []).
:- http_handler('/login', login_handler, []).

:- http_handler(js(.), http_reply_from_files('js/', []),
           [priority(1000), prefix]).
:- http_handler(css(.), http_reply_from_files('css/', []),
                [priority(1000), prefix]).
:- http_handler(img(.), http_reply_from_files('icons/', []),
                [priority(1000), prefix]).


logged_in_page(Page, Request) :-
    (   logged_in
    ->   call(Page, Request)
    ;   reply_html_page(
            [title('Terf finder')],
            \login_body)
    ).

login_body -->
    html(div([
             div(h1('TERF Finder')),
             div(form([action('/login'), method('POST')],
                      [
                          div([label(for(uname), 'User Name:'),
                               input([type(text), name(uname)], [])
                              ]),
                          div([label(for(pw), 'Password:'),
                               input([type(password), name(pw)])
                              ]),
                     input([type(submit), value(submit)])
                      ]))
         ])).

logged_in :-
    http_session_data(logged_in).


login_handler(Request) :-
    http_parameters(Request,
                    [],
                    [form_data(Data)]),
    memberchk(uname=Uname, Data),
    memberchk(pw=Passwd, Data),
    valid_credentials(Uname, Passwd),
    http_session_asserta(logged_in),
    !,
    http_redirect(moved, '/', Request).
login_handler(_Request) :-
    reply_html_page(
            [title('Terf finder')],
            \login_body).

home_page(_Request) :-
    reply_html_page(
        [title('Terf finder home')],
        \home_body).

valid_credentials('annie', 'lubber').


home_body -->
    {
    query('"are not women"', Terfs)
    },
    html([
        \html_requires(style),
        \html_requires(script),
        html(div(id(headerbar),
                [button(id(exportButton), 'Export CSV')])),
        \all_the_terfs(Terfs)
    ]).

all_the_terfs([]) --> [].
all_the_terfs([TERF | More]) -->
    {
    terf{
        screen_name: ScreenName,
        desc: Profile,
        userid: UserID,
        name: Name,
        tweets: [
            tweet{
                tweetid: TweetID,
                text: Text}
            |_]
        }  :< TERF
    },
    html(div(class(aterf), [
                 p([Name, a([target('_blank'), href('https://twitter.com/~w'-[ScreenName])], [' ', &(commat), ScreenName])]),
                 p(class(profile), Profile),
                 p(a([target('_blank'), href('https://twitter.com/~w/status/~w'-[ScreenName, TweetID])], Text)),
                 label(['block:',
                        input([class(userblock), type(checkbox), name(UserID), checked(checked)], []),
                        'block this user'])
             ])),
    all_the_terfs(More).
