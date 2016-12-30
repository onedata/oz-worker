%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of system errors into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(user_rest_translator).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([response/4]).

response(create, _UserId, authorize, {ok, DischargeMacaroon}) ->
    #rest_resp{code = ?HTTP_200_OK, body = DischargeMacaroon};

response(create, _UserId, client_tokens, {ok, Token}) ->
    #rest_resp{code = ?HTTP_200_OK, body = #{<<"token">> => Token}};

response(create, _UserId, client_tokens, {ok, Token}) ->
    #rest_resp{code = ?HTTP_200_OK, body = #{<<"token">> => Token}};


response(get, UserId, entity, {ok, User}) ->
    #od_user{
        name = Name,
        login = Login,
        alias = Alias,
        email_list = EmailList
    } = User,
    #rest_resp{code = ?HTTP_200_OK, body = #{
        <<"userId">> => UserId,
        <<"name">> => Name,
        <<"login">> => Login,
        <<"alias">> => Alias,
        <<"emailList">> => EmailList
    }}.