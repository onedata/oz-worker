%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% dao_providers header
%%% @end
%%% Created : 12. May 2014 12:50 PM
%%%-------------------------------------------------------------------

-ifndef(DAO_PROVIDERS_HRL).
-define(DAO_PROVIDERS_HRL, 1).

%% This record defines a user and is handled as a database document
-record(provider, {address = "", spaces = [], groups = []}).

-endif.