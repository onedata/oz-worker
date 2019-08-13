%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of system errors into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(error_rest_translator).
-author("Lukasz Opiola").

-include("http/rest.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").

-export([response/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Translates an entity logic error into REST response.
%% @end
%%--------------------------------------------------------------------
-spec response({error, term()}) -> #rest_resp{}.
response({error, Type}) ->
    IdAndDetails = gs_protocol_errors:error_to_json(?BASIC_PROTOCOL, {error, Type}),

    case translate({error, Type}) of
        Code when is_integer(Code) ->
            #rest_resp{code = Code, body = #{<<"error">> => IdAndDetails}};
        {Code, {MessageFormat, FormatArgs}} ->
            MessageBinary = str_utils:format_bin(
                str_utils:to_list(MessageFormat), FormatArgs
            ),
            #rest_resp{code = Code, body = #{<<"error">> => IdAndDetails#{
                <<"description">> => MessageBinary
            }}};
        {Code, MessageBinary} ->
            #rest_resp{code = Code, body = #{<<"error">> =>  IdAndDetails#{
                <<"description">> => MessageBinary
            }}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Translates an entity logic error into HTTP code, headers and body.
%% @end
%%--------------------------------------------------------------------
-spec translate({error, term()}) -> Code |
{Code, {MessageFormat :: binary(), FormatArgs :: [term()]}} |
{Code, MessageBinary :: binary()} when Code :: integer().
% General errors
translate(?ERROR_INTERNAL_SERVER_ERROR) ->
    ?HTTP_500_INTERNAL_SERVER_ERROR;

translate(?ERROR_NOT_IMPLEMENTED) ->
    ?HTTP_501_NOT_IMPLEMENTED;

translate(?ERROR_TEMPORARY_FAILURE) ->
    {?HTTP_503_SERVICE_UNAVAILABLE, <<"Service unavailable: temporary failure">>};

translate(?ERROR_NOT_SUPPORTED) ->
    {?HTTP_400_BAD_REQUEST,
        <<"This operation is not supported">>
    };

translate(?ERROR_NOT_FOUND) ->
    ?HTTP_404_NOT_FOUND;

translate(?ERROR_UNAUTHORIZED) ->
    ?HTTP_401_UNAUTHORIZED;

translate(?ERROR_FORBIDDEN) ->
    ?HTTP_403_FORBIDDEN;

% Errors connected with tokens
translate(?ERROR_BAD_TOKEN) ->
    {?HTTP_401_UNAUTHORIZED,
        <<"Provided access token could not be understood by the server">>
    };
translate(?ERROR_TOKEN_INVALID) ->
    {?HTTP_401_UNAUTHORIZED,
        <<"Provided access token is not valid">>
    };
translate(?ERROR_TOKEN_CAVEAT_UNVERIFIED(Caveat)) ->
    {?HTTP_401_UNAUTHORIZED,
        <<"Provided access token is not valid - unverified caveat: '", Caveat/binary, "'">>
    };
translate(?ERROR_TOKEN_SUBJECT_INVALID) ->
    {?HTTP_401_UNAUTHORIZED,
        <<"Token cannot be created for requested subject">>
    };
translate(?ERROR_TOKEN_AUDIENCE_FORBIDDEN) ->
    {?HTTP_400_BAD_REQUEST,
        <<"Requested audience is forbidden for this subject">>
    };
translate(?ERROR_TOKEN_SESSION_INVALID) ->
    {?HTTP_401_UNAUTHORIZED,
        <<"This token was issued for a session that no longer exists">>
    };
translate(?ERROR_BAD_AUDIENCE_TOKEN) ->
    {?HTTP_401_UNAUTHORIZED,
        <<"Provided audience token is not valid">>
    };

% Errors connected with bad data
translate(?ERROR_MALFORMED_DATA) ->
    {?HTTP_400_BAD_REQUEST,
        <<"Provided data could not be understood by the server">>
    };
translate(?ERROR_BAD_BASIC_CREDENTIALS) ->
    {?HTTP_401_UNAUTHORIZED,
        <<"Provided basic authorization credentials are not valid">>
    };

translate(?ERROR_BAD_IDP_ACCESS_TOKEN(OAuthProviderId)) ->
    {?HTTP_401_UNAUTHORIZED,
        {<<"Provided access token for \"~p\" is not valid">>, [OAuthProviderId]}
    };
translate(?ERROR_MISSING_REQUIRED_VALUE(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Missing required value: ~s">>, [Key]}
    };
translate(?ERROR_MISSING_AT_LEAST_ONE_VALUE(Keys)) ->
    KeysList = str_utils:join_binary(Keys, <<", ">>),
    {?HTTP_400_BAD_REQUEST,
        {<<"Missing data, you must provide at least one of: ~p">>, [KeysList]}
    };
translate(?ERROR_BAD_DATA(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" could not be understood by the server">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_EMPTY(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must not be empty">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_ATOM(Key)) ->
    % Atoms are strings in json
    translate(?ERROR_BAD_VALUE_BINARY(Key));
translate(?ERROR_BAD_VALUE_BOOLEAN(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must be a boolean">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_LIST_OF_ATOMS(Key)) ->
    % Atoms are strings in json
    translate(?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key));
translate(?ERROR_BAD_VALUE_BINARY(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must be a string">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must be a list of strings">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_INTEGER(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must be an integer">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_FLOAT(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must be a floating point number">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_JSON(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must be a valid JSON">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_TOKEN(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" is not a valid token">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" is not a valid list of IPv4 addresses">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_DOMAIN(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" is not a valid domain">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_SUBDOMAIN) ->
    {?HTTP_400_BAD_REQUEST,
        <<"Bad value: provided subdomain is not valid">>
    };
translate(?ERROR_BAD_VALUE_EMAIL) ->
    {?HTTP_400_BAD_REQUEST,
        <<"Bad value: provided e-mail is not a valid e-mail.">>
    };
translate(?ERROR_BAD_VALUE_TOO_LOW(Key, Threshold)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must be at least ~B">>, [Key, Threshold]}
    };
translate(?ERROR_BAD_VALUE_TOO_HIGH(Key, Threshold)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must not exceed ~B">>, [Key, Threshold]}
    };
translate(?ERROR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" must be between <~B, ~B>">>, [Key, Low, High]}
    };
translate(?ERROR_BAD_VALUE_NOT_ALLOWED(Key, AllowedValues)) ->
    % Convert binaries to strings so that
    % we do not include << >> signs in the response.
    AllowedValuesNotBin = lists:map(
        fun(Val) -> case Val of
            Bin when is_binary(Bin) -> binary_to_list(Bin);
            (Val) -> Val
        end end, AllowedValues),
    {?HTTP_400_BAD_REQUEST, {
        <<"Bad value: provided \"~s\" must be one of: ~p">>,
        [Key, AllowedValuesNotBin]
    }};
translate(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, AllowedValues)) ->
    % Convert binaries to strings so that we do not include << >> signs in the response.
    AllowedValuesNotBin = lists:map(
        fun(Val) when is_binary(Val) -> binary_to_list(Val);
            (Val) -> Val
        end, AllowedValues),
    {?HTTP_400_BAD_REQUEST, {
        <<"Bad value: provided \"~s\" must be a list containing zero or more following values: ~p">>,
        [Key, AllowedValuesNotBin]
    }};
translate(?ERROR_BAD_VALUE_ID_NOT_FOUND(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided ID (\"~s\") does not exist">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        % until VFS-3817 has been resolved do not change this description
        % as it is used to identify this specific error
        {<<"Bad value: provided identifier (\"~s\") is already occupied">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_BAD_TOKEN_TYPE(Key)) ->
    {?HTTP_400_BAD_REQUEST,
        {<<"Bad value: provided \"~s\" is of invalid type">>, [Key]}
    };
translate(?ERROR_BAD_VALUE_NAME) ->
    {?HTTP_400_BAD_REQUEST, <<"Bad value: ", (?NAME_REQUIREMENTS_DESCRIPTION)/binary>>};
translate(?ERROR_BAD_VALUE_FULL_NAME) ->
    {?HTTP_400_BAD_REQUEST, <<"Bad value: ", (?FULL_NAME_REQUIREMENTS_DESCRIPTION)/binary>>};
translate(?ERROR_BAD_VALUE_USERNAME) ->
    {?HTTP_400_BAD_REQUEST, <<"Bad value: ", (?USERNAME_REQUIREMENTS_DESCRIPTION)/binary>>};
translate(?ERROR_BAD_VALUE_PASSWORD) ->
    {?HTTP_400_BAD_REQUEST, <<"Bad value: ", (?PASSWORD_REQUIREMENTS_DESCRIPTION)/binary>>};
translate(?ERROR_BAD_VALUE_IDENTIFIER(Key)) ->
    {?HTTP_400_BAD_REQUEST, {
        <<"Bad value: provided \"~s\" is not a valid identifier.">>, [Key]
    }};
translate(?ERROR_PROTECTED_GROUP) ->
    {?HTTP_403_FORBIDDEN,
        <<"Forbidden: this group is protected and cannot be deleted.">>
    };
% Errors connected with relations between entities
translate(?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId)) ->
    RelationToString = case {ChType, ParType} of
        {od_space, od_provider} -> <<"is not supported by">>;
        {_, _} -> <<"is not a member of">>
    end,
    {?HTTP_400_BAD_REQUEST, {<<"Bad value: ~s ~s ~s">>, [
        ChType:to_string(ChId),
        RelationToString,
        ParType:to_string(ParId)
    ]}};
translate(?ERROR_RELATION_ALREADY_EXISTS(ChType, ChId, ParType, ParId)) ->
    RelationToString = case {ChType, ParType} of
        {od_space, od_provider} -> <<"is already supported by">>;
        {_, _} -> <<"is already a member of">>
    end,
    {?HTTP_400_BAD_REQUEST, {<<"Bad value: ~s ~s ~s">>, [
        ChType:to_string(ChId),
        RelationToString,
        ParType:to_string(ParId)
    ]}};
translate(?ERROR_CANNOT_DELETE_ENTITY(EntityType, EntityId)) ->
    {?HTTP_500_INTERNAL_SERVER_ERROR, {
        <<"Cannot delete ~s, failed to delete some dependent relations">>,
        [EntityType:to_string(EntityId)]
    }};
translate(?ERROR_CANNOT_ADD_RELATION_TO_SELF) ->
    {?HTTP_400_BAD_REQUEST, <<"Cannot add relation to self.">>};

translate(?ERROR_SUBDOMAIN_DELEGATION_NOT_SUPPORTED) ->
    {?HTTP_400_BAD_REQUEST, <<"Subdomain delegation is not supported by this Onezone.">>};
translate(?ERROR_SUBDOMAIN_DELEGATION_DISABLED) ->
    {?HTTP_400_BAD_REQUEST, <<"Subdomain delegation is disabled for this Oneprovider.">>};

translate(?ERROR_BASIC_AUTH_NOT_SUPPORTED) ->
    {?HTTP_401_UNAUTHORIZED, <<"Basic auth is not supported by this Onezone.">>};
translate(?ERROR_BASIC_AUTH_DISABLED) ->
    {?HTTP_400_BAD_REQUEST, <<"Basic auth is disabled for this user.">>};

% Wildcard match
translate({error, Reason}) ->
    ?error("Unexpected error: {error, ~p} in rest error translator", [Reason]),
    translate(?ERROR_INTERNAL_SERVER_ERROR).
