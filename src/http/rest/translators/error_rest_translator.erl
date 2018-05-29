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

-include("rest.hrl").
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
    case translate({error, Type}) of
        Code when is_integer(Code) ->
            #rest_resp{code = Code};
        {Code, {MessageFormat, FormatArgs}} ->
            MessageBinary = str_utils:format_bin(
                str_utils:to_list(MessageFormat), FormatArgs
            ),
            #rest_resp{code = Code, body = #{<<"error">> => MessageBinary}};
        {Code, MessageBinary} ->
            #rest_resp{code = Code, body = #{<<"error">> =>  MessageBinary}}
    end.


%%--------------------------------------------------------------------
%% @private
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

translate(?ERROR_NOT_SUPPORTED) ->
    ?HTTP_501_NOT_IMPLEMENTED;

translate(?ERROR_NOT_FOUND) ->
    ?HTTP_404_NOT_FOUND;

translate(?ERROR_UNAUTHORIZED) ->
    ?HTTP_401_UNAUTHORIZED;

translate(?ERROR_FORBIDDEN) ->
    ?HTTP_403_FORBIDDEN;

% Errors connected with macaroons
translate(?ERROR_BAD_MACAROON) ->
    {?HTTP_400_BAD_REQUEST,
        <<"Provided authorization token could not be understood by the server">>
    };
translate(?ERROR_MACAROON_INVALID) ->
    {?HTTP_400_BAD_REQUEST,
        <<"Provided authorization token is not valid">>
    };
translate(?ERROR_MACAROON_EXPIRED) ->
    {?HTTP_400_BAD_REQUEST,
        <<"Provided authorization token has expired">>
    };
translate(?ERROR_MACAROON_TTL_TO_LONG(MaxTtl)) ->
    {?HTTP_400_BAD_REQUEST,
        <<"Provided authorization token has too open TTL (it must not exceed ~B seconds)">>,
        [MaxTtl]
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
translate(?ERROR_BAD_EXTERNAL_ACCESS_TOKEN(OAuthProviderId)) ->
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
        {<<"Missing data, you must provide at least one of: ">>, [KeysList]}
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
translate(?ERROR_BAD_VALUE_ALIAS) ->
    {?HTTP_400_BAD_REQUEST, <<
        "Bad value: provided alias must be 2-15 characters long and composed of letters and digits."
        "Dashes and underscores are allowed (but not at the beginning or the end). "
        "Use null value to unset the alias."
    >>};
translate(?ERROR_BAD_VALUE_NAME) ->
    {?HTTP_400_BAD_REQUEST, <<
        "Bad value: Name must be 2-50 characters long and composed of UTF-8 letters, digits, brackets and underscores."
        "Dashes, spaces and dots are allowed (but not at the beginning or the end)."
    >>};
translate(?ERROR_BAD_VALUE_USER_NAME) ->
    {?HTTP_400_BAD_REQUEST, <<
        "Bad value: User name must be 2-50 characters long and composed of UTF-8 letters and digits."
        "Dashes, spaces, dots, commas and apostrophes are allowed (but not at the beginning or the end). "
    >>};
translate(?ERROR_BAD_VALUE_IDENTIFIER(Key)) ->
    {?HTTP_400_BAD_REQUEST, {
        <<"Bad value: provided \"~s\" is not a valid identifier.">>, [Key]
    }};
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
        {od_space, od_provider} -> <<"is alraedy supported by">>;
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
translate(?ERROR_CANNOT_JOIN_GROUP_TO_ITSELF) ->
    {?HTTP_400_BAD_REQUEST, <<"Groups cannot join themselves.">>};
translate(?ERROR_SUBDOMAIN_DELEGATION_DISABLED) ->
    {?HTTP_400_BAD_REQUEST, <<"Subdomain delegation is currently disabled.">>};
translate(?ERROR_BAD_VALUE_EMAIL) ->
    {?HTTP_400_BAD_REQUEST, <<"Bad value: provided e-mail is not a valid e-mail.">>};
% Wildcard match
translate({error, Reason}) ->
    ?error("Unexpected error: {error, ~p} in rest error translator", [Reason]),
    translate(?ERROR_INTERNAL_SERVER_ERROR).
