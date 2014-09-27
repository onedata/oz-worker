%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Dao definitions for provider records
%% @end
%% ===================================================================
-author("Tomasz Lichon").

-ifndef(DAO_PROVIDERS_HRL).
-define(DAO_PROVIDERS_HRL, 1).

%% This record defines a provider who support spaces and can be reached via url
-record(provider, {
    redirection_point :: binary(),
    urls :: [binary()],
    spaces = [] :: [SpaceId :: binary()],
    serial :: binary()
}).

-endif.
