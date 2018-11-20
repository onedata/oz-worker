%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions for OpenID Connect server mock.
%%% @end
%%%-------------------------------------------------------------------
-author("Lukasz Opiola").

-ifndef(OIDC_SERVER_MOCK_HRL).
-define(OIDC_SERVER_MOCK_HRL, 1).

-define(MOCK_ENDPOINT_FROM_XRDS(Key),
    str_utils:format_bin("https://example.com/~s", [Key])
).


-type userinfo_entry() :: string() | {xrds, string()} | {Attr :: string(), Url :: string()} | {Attr :: string(), {xrds, string()}}.

% Definition of OpenID Connect client or server behaviour.
%   Client - the spec is translated to corresponding auth.config
%   Server - the spec is used to generate an OIDC server mock.
-record(oidc_spec, {
    %% @formatter:off
    endpoints = #{} :: #{
        xrds => string(),
        authorize => string() | {xrds, string()},
        accessToken => string() | {xrds, string()},
        userInfo => userinfo_entry() | [userinfo_entry()]
    },

    % Expected data that must be sent by Onezone to verify user login
    clientId :: string(),
    clientSecret :: string(),
    scope :: string(),
    accessTokenAcquireMethod = post :: get | post,
    clientSecretPassMethod = urlencoded :: urlencoded  | inAuthHeader,
    accessTokenPassMethod = inAuthHeader :: urlencoded  | inAuthHeader,
    customData = #{} :: #{
        accessToken => #{
            parameters => undefined | #{string() => string()},
            headers => undefined | #{string() => string()}
        },
        userInfo => #{
            parameters => undefined | #{string() => string()},
            headers => undefined | #{string() => string()}
        }
    }
    %% @formatter:on
}).


-endif.


