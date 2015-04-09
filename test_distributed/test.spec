%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Spec for all ct tests
%% ===================================================================

%% Aliases
{alias, connection_test, "./connection_test/"}.
{alias, dao_test, "./dao_test/"}.
{alias, op_channel_test, "./op_channel_test/"}.

%% Common dirs
{include, ["../include","."]}.

%% Suites
{suites, connection_test, all}.
{suites, dao_test, all}.
{suites, op_channel_test, all}.

%% Enable surefire reports for bamboo
{ct_hooks, [
  {cth_surefire, [
    {path,"TEST-report.xml"}
  ]}
]}.