%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Macros used in OAI test suite.
%%% @end
%%%-------------------------------------------------------------------
-author("Jakub Kudzia").

-define(DC_METADATA_XML,
    <<"<?xml version=\"1.0\"?>",
    "<metadata xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\">"
    "<dc:title>Test dataset<\/dc:title>",
    "<dc:creator>John Johnson _πœę ßþą_śðæŋ-əłżź.ćńµジ<\/dc:creator>"/utf8,
    "<dc:creator>Jane Doe<\/dc:creator>",
    "<dc:subject>Test of datacite<\/dc:subject>",
    "<dc:description>Lorem ipsum lorem ipusm<\/dc:description>",
    "<dc:publisher>Onedata<\/dc:publisher>",
    "<dc:publisher>EGI<\/dc:publisher>",
    "<dc:date>2016<\/dc:date>",
    "<dc:format>application\/pdf<\/dc:format>",
    "<dc:identifier>onedata:LKJHASKFJHASLKDJHKJHuah132easd<\/dc:identifier>",
    "<dc:language>eng<\/dc:language>",
    "<dc:rights>CC-0<\/dc:rights>",
    "<\/metadata>">>
).

-define(EDM_METADATA_XML,
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <RDF xmlns=\"http:\/\/www.w3.org\/1999\/02\/22-rdf-syntax-ns#\"
         xmlns:edm=\"http:\/\/www.europeana.eu\/schemas\/edm\/\"
         xmlns:rdfs=\"http:\/\/www.w3.org\/2000\/01\/rdf-schema#\"
         xmlns:skos=\"http:\/\/www.w3.org\/2004\/02\/skos\/core#\"
         xmlns:ore=\"http:\/\/www.openarchives.org\/ore\/terms\/\"
         xmlns:rdf=\"http:\/\/www.w3.org\/1999\/02\/22-rdf-syntax-ns#\"
         elementFormDefault=\"qualified\"
         attributeFormDefault=\"qualified\">

      <edm:ProvidedCHO>
        <edm:title>Test dataset<\/edm:title>
        <edm:creator>John Doe<\/edm:creator>
        <edm:subject>Test of datacite<\/edm:subject>
      <\/edm:ProvidedCHO>

    <\/RDF>
">>
).