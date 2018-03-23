# TERFBlocklist

Written for @TERFBlocklist on Twitter, a tool to partially automate the process of finding TERF accounts.

## Usage

You need to go to https://apps.twitter.com/ and make an app with a consumer key and consumer secret.

    swipl terf_server.pl
    ?- pack_install(twitter).
    ?- go.
    ?- set_setting(find_a_terf:consumer_key, 'Your consumer key').
true.
    ?- set_setting(find_a_terf:consumer_secret, 'your secret').
true.
    ?-save_settings('settings.db').
    ?- halt.

Now restart the server 

    swipl terf_server.pl
    ?- go.


