%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for GUI message record, holding static HTML contents
%%% displayed in the GUI.
%%% There is a limited set of allowed message ids, all of which always
%%% map to en existing record. The default state of a record
%%% is enabled with empty message. This makes Onepanel GUI display
%%% warning encouraging the admin to set the message. Changing 'enabled'
%%% to false hides the message if exists and silences the warning.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_message).
-author("Wojciech Geisler").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/api_errors.hrl").

%% API
-export([exists/1, get/1, get_as_map/1, update/2]).

%% datastore_model callbacks
-export([get_record_struct/1, get_record_version/0]).

-type id() :: binary().
-type record() :: #gui_message{}.
-type map_repr() :: #{enabled := boolean(), body := binary()}.
-type diff_map() :: #{enabled => boolean(), body => binary()}.
-type doc() :: datastore_doc:doc(record()).
-export_type([id/0, record/0]).

-define(CTX, #{model => ?MODULE}).

-define(ALLOWED_IDS, [
    <<"cookie_consent_notification">>,
    <<"privacy_policy">>,
    <<"signin_notification">>
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Checks whether given message Id exists, which is equivalent
%% to its presence on the list of expected Ids.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> boolean().
exists(MessageId) ->
    is_allowed_id(MessageId).


%%--------------------------------------------------------------------
%% @doc
%% Returns message by id.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(MessageId) ->
    case is_allowed_id(MessageId) of
        true ->
            case datastore_model:get(?CTX, MessageId) of
                {ok, #document{} = Doc} ->
                    {ok, Doc};
                {error, not_found} ->
                    {ok, #document{key = MessageId, value = default_record(MessageId)}};
                Error ->
                    Error
            end;
        false ->
            {error, not_found}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns message record as a map for use by Onepanel.
%% @end
%%--------------------------------------------------------------------
-spec get_as_map(id()) -> {ok, map_repr()} | {error, term()}.
get_as_map(MessageId) ->
    case ?MODULE:get(MessageId) of
        {ok, #document{value = #gui_message{enabled = Enabled, body = Body}}} ->
            {ok, #{enabled => Enabled, body => Body}};
        Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Toggles message state or body.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff_map()) -> {ok, doc()} | {error, term()}.
update(MessageId, DiffMap) ->
    DiffFun = fun(Record) ->
        {ok, Record#gui_message{
            enabled = maps:get(enabled, DiffMap, Record#gui_message.enabled),
            body = maps:get(body, DiffMap, Record#gui_message.body)
        }}
    end,
    case is_allowed_id(MessageId) of
        true ->
            {ok, Default} = DiffFun(default_record(MessageId)),
            datastore_model:update(?CTX, MessageId, DiffFun, Default);
        false ->
            {error, not_found}
    end.


%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> datastore_model:record_version().
get_record_version() ->
    1.


%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {enabled, boolean},
        {body, string}
    ]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec default_record(id()) -> record().
default_record(<<"signin_notification">>) ->
    LoginNotification = oz_worker:get_env(login_notification, ""),
    #gui_message{
        % preserve existing login notification
        body = str_utils:unicode_list_to_binary(LoginNotification)
    };

default_record(_MessageId) ->
    #gui_message{}.


-spec is_allowed_id(id()) -> boolean().
is_allowed_id(MessageId) ->
    lists:member(MessageId, ?ALLOWED_IDS).
