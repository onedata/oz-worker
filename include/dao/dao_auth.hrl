%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Dao definitions for authorization records
%% @end
%% ===================================================================
-author("Konrad Zemek").


-ifndef(DAO_AUTH_HRL).
-define(DAO_AUTH_HRL, 1).

%% Records of this type store OpenID Authorization Codes and associated details
%% essential for trading the code for an Access Token.
-record(authorization, {
    code :: binary(),                    %% The OpenID Authorization Code
    user_id :: binary(),                 %% UserID of the authorizing user
    provider_id :: binary(),             %% ProviderID of the authorized Provider (OpenID Client)
    expiration_time :: non_neg_integer() %% Expiration time point of the Authorization Code in seconds since epoch
}).

%% Records of this type store OpenID Access Tokens and associated data used for
%% OpenID Client authorization.
-record(access, {
    token :: binary(),                   %% The OpenID Access Token
    token_hash :: binary(),              %% A hash of the token defined as base64(sha512(token))
    refresh_token :: binary(),           %% The refresh token associated with the access
    user_id :: binary(),                 %% UserID of the authorizing user
    provider_id :: binary(),             %% ProviderID of the authorized Provider (OpenID Client)
    client_name :: binary() | atom,      %% Human-readable name of the authorized Provider
    expiration_time :: non_neg_integer() %% Expiration time point of the Access Token in seconds since epoch
}).

-endif.
