%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains useful functions commonly used in control_panel modules.
%% @end
%% ===================================================================
-module(gui_utils).
-author("Lukasz Opiola").
-include_lib("n2o/include/wf.hrl").

-export([logotype_footer/1]).


%% ====================================================================
%% API functions
%% ====================================================================

%% logotype_footer/1
%% ====================================================================
%% @doc Convienience function to render logotype footer, coming after page content.
%% @end
-spec logotype_footer(MarginTop :: integer()) -> list().
%% ====================================================================
logotype_footer(MarginTop) ->
  Height = integer_to_binary(MarginTop + 82),
  Margin = integer_to_binary(MarginTop),
  [
    #panel{style = <<"position: relative; height: ", Height/binary, "px;">>, body = [
      #panel{style = <<"text-align: center; z-index: -1; margin-top: ", Margin/binary, "px;">>, body = [
        #image{style = <<"margin: 10px 100px;">>, image = <<"/images/innow-gosp-logo.png">>},
        #image{style = <<"margin: 10px 100px;">>, image = <<"/images/plgrid-plus-logo.png">>},
        #image{style = <<"margin: 10px 100px;">>, image = <<"/images/unia-logo.png">>}
      ]}
    ]}
  ].
