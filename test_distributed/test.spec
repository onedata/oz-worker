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
{alias, dao_test, "./dao_test/"}.

%% Common dirs
{include, ["../include","."]}.
{logdir, "./log/"}.

%% Suites
{suites, connection_test, all}.
{suites, dao_test, all}.

%% Enable surefire reports for bamboo
{ct_hooks, [
  {cth_surefire, [
    {path,"TEST-report.xml"}
  ]}
]}.