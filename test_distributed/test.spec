%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Spec for all ct tests
%%% @end
%%% Created : 29. Apr 2014 2:53 PM
%%%-------------------------------------------------------------------

%% Aliases
{alias, connection_test, "./connection_test/"}.

%% Common dirs
{include, ["../include","."]}.
{logdir, "./logs/"}.

%% Suites
{suites, connection_test, all}.
