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
{alias, rest_modules_test, "./rest_modules_test/"}.
{alias, op_channel_test, "./op_channel_test/"}.

%% Common dirs
{include, ["../include","."]}.
{logdir, "./logs"}.

%% Suites
{suites, connection_test, all}.
{suites, rest_modules_test, all}.
{suites, op_channel_test, all}.

%% Enable surefire reports for bamboo
{ct_hooks, [
  {cth_surefire, [
    {path,"TEST-report.xml"}
  ]}
]}.
