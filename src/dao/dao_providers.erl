%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides high level DB API for handling providers documents.
%% @end
%% ===================================================================
-module(dao_providers).
-author("Tomasz Lichon").

%% Includes
-include("dao/dao_providers.hrl").
-include("dao/dao_types.hrl").

%% API
-export([save_provider/1, remove_provider/1, exist_provider/1, get_provider/1]).

%% save_provider/1
%% ====================================================================
%% @doc Saves provider to DB. Argument should be either #provider{} record
%% (if you want to save it as new document) <br/>
%% or #veil_document{} that wraps #provider{} if you want to update descriptor in DB. <br/>
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec save_provider(Provider :: provider_info() | provider_doc()) -> {ok, provider_id()} | {error, any()} | no_return().
%% ====================================================================
save_provider(#provider{} = Provider) ->
	save_provider(#veil_document{record = Provider});
save_provider(#veil_document{record = #provider{}, uuid = UUID} = ProviderDoc) when is_list(UUID) ->
    dao_records:save_record(ProviderDoc).


%% remove_provider/1
%% ====================================================================
%% @doc Removes provider from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec remove_provider(ProviderId:: uuid()) ->
	ok | {error, any()} | no_return().
%% ====================================================================
remove_provider(ProviderId) ->
    dao_records:remove_record(ProviderId).

%% exist_provider/1
%% ====================================================================
%% @doc Checks whether provider exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec exist_provider(ProviderId :: uuid()) -> {ok, true | false} | {error, any()}.
%% ====================================================================
exist_provider(ProviderId) ->
    dao_records:exist_record(ProviderId).

%% get_provider/1
%% ====================================================================
%% @doc Gets provider from DB
%% Non-error return value is always {ok, #veil_document{record = #provider}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec get_provider(ProviderId :: uuid()) -> {ok, provider_doc()} | {error, any()} | no_return().
%% ====================================================================
get_provider(ProviderId) ->
    dao_records:get_record(ProviderId).
