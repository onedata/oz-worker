%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Logging utilities for IdP auth i.e. Single Sign-On protocols (OpenId or SAML).
%%% @end
%%%-------------------------------------------------------------------
-module(idp_auth_logger).


-include("auth/auth_errors.hrl").
-include("auth/auth_common.hrl").


-define(LOG_GUI_LOGIN_ATTEMPTS, oz_worker:get_env(log_idp_gui_login_attempts, true)).
-define(LOG_TOKEN_AUTH_ATTEMPTS, oz_worker:get_env(log_idp_token_auth_attempts, true)).
-define(LOG_FILE, oz_worker:get_env(idp_auth_log_file, "./log/idp_auth.log")).
-define(MAX_LOG_SIZE, oz_worker:get_env(idp_auth_log_max_size, 104857600)).


%% API
-export([log_user_info_collection_to_file/3]).
-export([log_error/5, log_error_to_file/2]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec log_user_info_collection_to_file(
    auth_config:idp(),
    idp_auth:flow_type(),
    binary() | fun(() -> binary())
) -> ok.
log_user_info_collection_to_file(IdP, FlowType, BodyOrFun) ->
    should_log(FlowType) andalso log_to_file("user info collected; idp: ~ts, flow type: ~ts, body:~n~ts", [
        IdP,
        FlowType,
        case BodyOrFun of
            Fun when is_function(Fun, 0) -> Fun();
            Body when is_binary(Body) -> Body
        end
    ]),
    ok.


-spec log_error(
    {error, term()},
    auth_config:idp(),
    idp_auth:flow_type(),
    state_token:state_token(),
    stacktrace()
) -> ok.
log_error(?ERROR_BAD_AUTH_CONFIG, _, _, _, Stacktrace) ->
    ?auth_debug(
        "Login request failed due to bad auth config: ~ts", [
            iolist_to_binary(onedata_logger:pr_stacktrace(Stacktrace))
        ]
    );
log_error(?ERROR_INVALID_STATE, _, _, StateToken, _) ->
    ?auth_debug(
        "Cannot validate login request - invalid state ~ts (not found)",
        [StateToken]
    );
log_error(?ERROR_INVALID_AUTH_REQUEST, IdP, FlowType, StateToken, Stacktrace) ->
    ?auth_debug(
        "Cannot validate login request for IdP '~tp' (flow: ~ts, state: ~ts) - invalid auth request~n"
        "Stacktrace: ~ts", [IdP, FlowType, StateToken, iolist_to_binary(onedata_logger:pr_stacktrace(Stacktrace))]
    );
log_error(?ERR_USER_BLOCKED, IdP, FlowType, StateToken, _) ->
    ?auth_debug(
        "Declining login request for IdP '~tp' (flow: ~ts, state: ~ts) - the user is blocked",
        [IdP, FlowType, StateToken]
    );
log_error(?ERROR_IDP_UNREACHABLE(Reason), IdP, FlowType, StateToken, _) ->
    ?auth_warning(
        "Cannot validate login request for IdP '~tp' (flow: ~ts, state: ~ts) - IdP not reachable: ~tp",
        [IdP, FlowType, StateToken, Reason]
    );
log_error(?ERROR_BAD_IDP_RESPONSE(Endpoint, Code, Headers, Body), IdP, FlowType, StateToken, _) ->
    ?auth_warning(
        "Cannot validate login request for IdP '~tp' (flow: ~ts, state: ~ts) - unexpected response from IdP:~n"
        "Endpoint: ~ts~n"
        "Code: ~tp~n"
        "Headers: ~tp~n"
        "Body: ~ts",
        [IdP, FlowType, StateToken, Endpoint, Code, Headers, Body]
    );
log_error(?ERROR_CANNOT_RESOLVE_REQUIRED_ATTRIBUTE(Attr), IdP, FlowType, StateToken, _) ->
    ?auth_debug(
        "Cannot map attributes for IdP '~tp' (flow: ~ts, state: ~ts) - atrribute '~tp' not found",
        [IdP, FlowType, StateToken, Attr]
    );
log_error(?ERROR_BAD_ATTRIBUTE_TYPE(Attribute, Type), IdP, FlowType, StateToken, _) ->
    ?auth_debug(
        "Cannot map attributes for IdP '~tp' (flow: ~ts, state: ~ts) - atrribute '~tp' "
        "does not have the required type '~tp'",
        [IdP, FlowType, StateToken, Attribute, Type]
    );
log_error(?ERROR_ATTRIBUTE_MAPPING_ERROR(Attribute, IdPAttributes, EType, EReason, Stacktrace), IdP, FlowType, StateToken, _) ->
    ?auth_debug(
        "Cannot map attributes for IdP '~tp' (flow: ~ts, state: ~ts) - atrribute '~tp' "
        "could not be mapped due to an error - ~tp:~tp~n"
        "IdP attributes: ~tp~n"
        "Stacktrace: ~ts",
        [IdP, FlowType, StateToken, Attribute, EType, EReason, IdPAttributes, iolist_to_binary(onedata_logger:pr_stacktrace(Stacktrace))]
    );
log_error(?ERROR_ACCOUNT_ALREADY_LINKED_TO_CURRENT_USER(UserId), IdP, FlowType, StateToken, _) ->
    ?auth_debug(
        "Cannot link account from IdP '~tp' for user '~ts' (flow: ~ts, state: ~ts) - account already linked to the user",
        [IdP, UserId, FlowType, StateToken]
    );
log_error(?ERROR_ACCOUNT_ALREADY_LINKED_TO_ANOTHER_USER(UserId, OtherUserId), IdP, FlowType, StateToken, _) ->
    ?auth_debug(
        "Cannot link account from IdP '~tp' for user '~ts' (flow: ~ts, state: ~ts) - account already linked to user '~ts'",
        [IdP, UserId, FlowType, StateToken, OtherUserId]
    );
log_error(?ERR_INTERNAL_SERVER_ERROR(Ref), IdP, FlowType, StateToken, _) ->
    % The logging is already done when throwing this error
    ?auth_debug(
        "Cannot validate login request for IdP '~tp' (flow: ~ts, state: ~ts) - internal server error (ref: ~ts)",
        [IdP, FlowType, StateToken, Ref]
    );
log_error(Error, IdP, FlowType, StateToken, Stacktrace) ->
    ?auth_error(
        "Cannot validate login request for IdP '~tp' (flow: ~ts, state: ~ts) - ~tp~n"
        "Stacktrace: ~ts", [IdP, FlowType, StateToken, Error, iolist_to_binary(onedata_logger:pr_stacktrace(Stacktrace))]
    ).


-spec log_error_to_file(io:format(), [term()]) -> ok.
log_error_to_file(Format, Args) ->
    log_to_file(Format, Args).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec log_to_file(io:format(), [term()]) -> ok.
log_to_file(Format, Args) ->
    onedata_logger:log_with_rotation(
        ?LOG_FILE,
        Format,
        Args,
        ?MAX_LOG_SIZE
    ).


%% @private
-spec should_log(idp_auth:flow_type()) -> boolean().
should_log(gui_login) -> true == ?LOG_GUI_LOGIN_ATTEMPTS;
should_log(_) -> true == ?LOG_TOKEN_AUTH_ATTEMPTS.
