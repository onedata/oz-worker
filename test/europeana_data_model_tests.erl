%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of europeana_data_model module.
%%% @end
%%%-------------------------------------------------------------------
-module(europeana_data_model_tests).
-author("Katarzyna Such").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include("entity_logic.hrl").

-define(NAMESPACES, "xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\"",
    "xmlns:dcterms=\"http:\/\/purl.org\/dc\/terms\/\"",
    "xmlns:edm=\"http:\/\/www.europeana.eu\/schemas\/edm\/\"",
    "xmlns:ore=\"http:\/\/www.openarchives.org\/ore\/terms\/\"",
    "xmlns:owl=\"http:\/\/www.w3.org\/2002\/07\/owl#\"",
    "xmlns:rdf=\"http:\/\/www.w3.org\/1999\/02\/22-rdf-syntax-ns#\"",
    "xmlns:foaf=\"http:\/\/xmlns.com\/foaf\/0.1\/\"",
    "xmlns:skos=\"http:\/\/www.w3.org\/2004\/02\/skos\/core#\"",
    "xmlns:rdaGr2=\"http:\/\/rdvocab.info\/ElementsGr2\/\"",
    "xmlns:wgs84_pos=\"http:\/\/www.w3.org\/2003\/01\/geo\/wgs84_pos#\"",
    "xmlns:crm=\"http:\/\/www.cidoc-crm.org\/cidoc--crm\/\"",
    "xmlns:cc=\"http:\/\/creativecommons.org\/ns#\"").

-define(EXPECTED, <<"<?xml version=\"1.0\"?>",
    "<rdf:RDF ", ?NAMESPACES,">",
    "<edm:ProvidedCHO rdf:about=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3\">",
    "<dc:title xml:lang=\"en\">Metadata Example Record Tier A<\/dc:title>",
    "<dc:type>book<\/dc:type>",
    "<dc:language>deu<\/dc:language>",
    "<edm:type>TEXT<\/edm:type>",
    "<dcterms:isPartOf>Europeana Foundation Example Records<\/dcterms:isPartOf>",
    "<dc:identifier>oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3</dc:identifier>",
    "<\/edm:ProvidedCHO>",
    "<ore:Aggregation rdf:about=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3_AGG\">",
    "<edm:aggregatedCHO rdf:resource=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3\"\/>",
    "<edm:dataProvider>Europeana Foundation<\/edm:dataProvider>",
    "<edm:isShownBy rdf:resource=\"http:\/\/media.culturegrid.org.uk\/mediaLibrary\/Partage\/LoveArtNouveau\/Glasgow\/DSCF4092.JPG\"\/>",
    "<edm:provider>Europeana Foundation<\/edm:provider>",
    "<edm:rights rdf:resource=\"http:\/\/rightsstatements.org\/vocab\/NoC-OKLR\/1.0\/\"\/>",
    "<\/ore:Aggregation>",
    "<\/rdf:RDF>">>).

-define(IDENTIFIER, <<"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3">>).

%%%===================================================================
%%% Tests functions
%%%===================================================================

no_providerCHO_identifier_test() ->
    Input = <<"<?xml version=\"1.0\"?>",
        "<rdf:RDF ", ?NAMESPACES,">",
        "<edm:ProvidedCHO>",
        "<dc:title xml:lang=\"en\">Metadata Example Record Tier A<\/dc:title>",
        "<dc:type>book<\/dc:type>",
        "<dc:language>deu<\/dc:language>",
        "<edm:type>TEXT<\/edm:type>",
        "<dcterms:isPartOf>Europeana Foundation Example Records<\/dcterms:isPartOf>",
        "<dc:identifier>oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3</dc:identifier>",
        "<\/edm:ProvidedCHO>",
        "<ore:Aggregation rdf:about=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3_AGG\">",
        "<edm:aggregatedCHO rdf:resource=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3\"\/>",
        "<edm:dataProvider>Europeana Foundation<\/edm:dataProvider>",
        "<edm:isShownBy rdf:resource=\"http:\/\/media.culturegrid.org.uk\/mediaLibrary\/Partage\/LoveArtNouveau\/Glasgow\/DSCF4092.JPG\"\/>",
        "<edm:provider>Europeana Foundation<\/edm:provider>",
        "<edm:rights rdf:resource=\"http:\/\/rightsstatements.org\/vocab\/NoC-OKLR\/1.0\/\"\/>",
        "<\/ore:Aggregation>",
        "<\/rdf:RDF>">>,
    Actual_XMLElement = europeana_data_model:encode(Input, [?IDENTIFIER]),
    Actual = iolist_to_binary(xmerl:export_simple([Actual_XMLElement], xmerl_xml, [])),
    ?assertEqual(?EXPECTED, Actual).


no_dc_identifier_test() ->
    Input = <<"<?xml version=\"1.0\"?>",
        "<rdf:RDF ", ?NAMESPACES,">",
        "<edm:ProvidedCHO rdf:about=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3\">",
        "<dc:title xml:lang=\"en\">Metadata Example Record Tier A<\/dc:title>",
        "<dc:type>book<\/dc:type>",
        "<dc:language>deu<\/dc:language>",
        "<edm:type>TEXT<\/edm:type>",
        "<dcterms:isPartOf>Europeana Foundation Example Records<\/dcterms:isPartOf>",
        "<\/edm:ProvidedCHO>",
        "<ore:Aggregation rdf:about=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3_AGG\">",
        "<edm:aggregatedCHO rdf:resource=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3\"\/>",
        "<edm:dataProvider>Europeana Foundation<\/edm:dataProvider>",
        "<edm:isShownBy rdf:resource=\"http:\/\/media.culturegrid.org.uk\/mediaLibrary\/Partage\/LoveArtNouveau\/Glasgow\/DSCF4092.JPG\"\/>",
        "<edm:provider>Europeana Foundation<\/edm:provider>",
        "<edm:rights rdf:resource=\"http:\/\/rightsstatements.org\/vocab\/NoC-OKLR\/1.0\/\"\/>",
        "<\/ore:Aggregation>",
        "<\/rdf:RDF>">>,
    Actual_XMLElement = europeana_data_model:encode(Input, [?IDENTIFIER]),
    Actual = iolist_to_binary(xmerl:export_simple([Actual_XMLElement], xmerl_xml, [])),
    ?assertEqual(?EXPECTED, Actual).


no_OREAggregation_identifier_test() ->
    Input = <<"<?xml version=\"1.0\"?>",
        "<rdf:RDF ", ?NAMESPACES,">",
        "<edm:ProvidedCHO rdf:about=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3\">",
        "<dc:title xml:lang=\"en\">Metadata Example Record Tier A<\/dc:title>",
        "<dc:type>book<\/dc:type>",
        "<dc:language>deu<\/dc:language>",
        "<edm:type>TEXT<\/edm:type>",
        "<dcterms:isPartOf>Europeana Foundation Example Records<\/dcterms:isPartOf>",
        "<dc:identifier>oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3</dc:identifier>",
        "<\/edm:ProvidedCHO>",
        "<ore:Aggregation>",
        "<edm:aggregatedCHO rdf:resource=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3\"\/>",
        "<edm:dataProvider>Europeana Foundation<\/edm:dataProvider>",
        "<edm:isShownBy rdf:resource=\"http:\/\/media.culturegrid.org.uk\/mediaLibrary\/Partage\/LoveArtNouveau\/Glasgow\/DSCF4092.JPG\"\/>",
        "<edm:provider>Europeana Foundation<\/edm:provider>",
        "<edm:rights rdf:resource=\"http:\/\/rightsstatements.org\/vocab\/NoC-OKLR\/1.0\/\"\/>",
        "<\/ore:Aggregation>",
        "<\/rdf:RDF>">>,
    Actual_XMLElement = europeana_data_model:encode(Input, [?IDENTIFIER]),
    Actual = iolist_to_binary(xmerl:export_simple([Actual_XMLElement], xmerl_xml, [])),
    ?assertEqual(?EXPECTED, Actual).


no_aggregatedCHO_resource_test() ->
    Input = <<"<?xml version=\"1.0\"?>",
        "<rdf:RDF ", ?NAMESPACES,">",
        "<edm:ProvidedCHO rdf:about=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3\">",
        "<dc:title xml:lang=\"en\">Metadata Example Record Tier A<\/dc:title>",
        "<dc:type>book<\/dc:type>",
        "<dc:language>deu<\/dc:language>",
        "<edm:type>TEXT<\/edm:type>",
        "<dcterms:isPartOf>Europeana Foundation Example Records<\/dcterms:isPartOf>",
        "<dc:identifier>oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3</dc:identifier>",
        "<\/edm:ProvidedCHO>",
        "<ore:Aggregation rdf:about=\"oai:datahub.egi.eu:ffa72790ef709fa864aa36d70374e33dchf6b3\">",
        "<edm:aggregatedCHO />",
        "<edm:dataProvider>Europeana Foundation<\/edm:dataProvider>",
        "<edm:isShownBy rdf:resource=\"http:\/\/media.culturegrid.org.uk\/mediaLibrary\/Partage\/LoveArtNouveau\/Glasgow\/DSCF4092.JPG\"\/>",
        "<edm:provider>Europeana Foundation<\/edm:provider>",
        "<edm:rights rdf:resource=\"http:\/\/rightsstatements.org\/vocab\/NoC-OKLR\/1.0\/\"\/>",
        "<\/ore:Aggregation>",
        "<\/rdf:RDF>">>,
    Actual_XMLElement = europeana_data_model:encode(Input, [?IDENTIFIER]),
    Actual = iolist_to_binary(xmerl:export_simple([Actual_XMLElement], xmerl_xml, [])),
    ?assertEqual(?EXPECTED, Actual).


wrong_identifier_test() ->
    Input = <<"<?xml version=\"1.0\"?>",
        "<rdf:RDF ", ?NAMESPACES,">",
        "<edm:ProvidedCHO rdf:about=\"??????\">",
        "<dc:title xml:lang=\"en\">Metadata Example Record Tier A<\/dc:title>",
        "<dc:type>book<\/dc:type>",
        "<dc:language>deu<\/dc:language>",
        "<edm:type>TEXT<\/edm:type>",
        "<dcterms:isPartOf>Europeana Foundation Example Records<\/dcterms:isPartOf>",
        "<dc:identifier>??????????????????</dc:identifier>",
        "<\/edm:ProvidedCHO>",
        "<ore:Aggregation rdf:about=\"??????????????????\">",
        "<edm:aggregatedCHO rdf:resource=\"??????????????????\"\/>",
        "<edm:dataProvider>Europeana Foundation<\/edm:dataProvider>",
        "<edm:isShownBy rdf:resource=\"http:\/\/media.culturegrid.org.uk\/mediaLibrary\/Partage\/LoveArtNouveau\/Glasgow\/DSCF4092.JPG\"\/>",
        "<edm:provider>Europeana Foundation<\/edm:provider>",
        "<edm:rights rdf:resource=\"http:\/\/rightsstatements.org\/vocab\/NoC-OKLR\/1.0\/\"\/>",
        "<\/ore:Aggregation>",
        "<\/rdf:RDF>">>,
    Actual_XMLElement = europeana_data_model:encode(Input, [?IDENTIFIER]),
    Actual = iolist_to_binary(xmerl:export_simple([Actual_XMLElement], xmerl_xml, [])),
    ?assertEqual(?EXPECTED, Actual).

-endif.