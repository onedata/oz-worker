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

-define(DC_METADATA_XML_WITH_IDENTIFIERS(PublicHandle, PublicShareUrl),
    <<"<?xml version=\"1.0\"?>",
    "<metadata xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\">"
    "<dc:identifier>", PublicHandle/binary, "</dc:identifier>",
    "<dc:identifier>", PublicShareUrl/binary,"</dc:identifier>",
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

-define(NAMESPACES, "xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\" xmlns:dcterms=\"http:\/\/purl.org\/dc\/terms\/\" xmlns:edm=\"http:\/\/www.europeana.eu\/schemas\/edm\/\" xmlns:ore=\"http:\/\/www.openarchives.org\/ore\/terms\/\" xmlns:owl=\"http:\/\/www.w3.org\/2002\/07\/owl#\" xmlns:rdf=\"http:\/\/www.w3.org\/1999\/02\/22-rdf-syntax-ns#\" xmlns:foaf=\"http:\/\/xmlns.com\/foaf\/0.1\/\" xmlns:skos=\"http:\/\/www.w3.org\/2004\/02\/skos\/core#\" xmlns:rdaGr2=\"http:\/\/rdvocab.info\/ElementsGr2\/\" xmlns:wgs84_pos=\"http:\/\/www.w3.org\/2003\/01\/geo\/wgs84_pos#\" xmlns:crm=\"http:\/\/www.cidoc-crm.org\/cidoc--crm\/\" xmlns:cc=\"http:\/\/creativecommons.org\/ns#\"").


-define(EDM_METADATA_XML,
    <<"<?xml version=\"1.0\"?>",
        "<rdf:RDF ", ?NAMESPACES,">",
        "<edm:ProvidedCHO rdf:about=\"??????\">",
        "<dc:title xml:lang=\"en\">Metadata Example Record Tier A<\/dc:title>",
        "<dc:type>book<\/dc:type>",
        "<dc:language>deu<\/dc:language>",
        "<edm:type>TEXT<\/edm:type>",
        "<dcterms:isPartOf>Europeana Foundation Example Records<\/dcterms:isPartOf>",
        "<dc:identifier>??????</dc:identifier>",
        "<\/edm:ProvidedCHO>",
        "<ore:Aggregation rdf:about=\"??????\">",
        "<edm:aggregatedCHO rdf:resource=\"??????\"\/>",
        "<edm:dataProvider>Europeana Foundation<\/edm:dataProvider>",
        "<edm:isShownBy rdf:resource=\"http:\/\/media.culturegrid.org.uk\/mediaLibrary\/Partage\/LoveArtNouveau\/Glasgow\/DSCF4092.JPG\"\/>",
        "<edm:provider>Europeana Foundation<\/edm:provider>",
        "<edm:rights rdf:resource=\"http:\/\/rightsstatements.org\/vocab\/NoC-OKLR\/1.0\/\"\/>",
        "<\/ore:Aggregation>",
        "<\/rdf:RDF>">>).

-define(EDM_METADATA_WITH_IDENTIFIERS(PublicHandle), <<"<?xml version=\"1.0\"?>",
    "<rdf:RDF ", ?NAMESPACES,">",
    "<edm:ProvidedCHO rdf:about=\"", PublicHandle/binary,"\">",
    "<dc:title xml:lang=\"en\">Metadata Example Record Tier A<\/dc:title>",
    "<dc:type>book<\/dc:type>",
    "<dc:language>deu<\/dc:language>",
    "<edm:type>TEXT<\/edm:type>",
    "<dcterms:isPartOf>Europeana Foundation Example Records<\/dcterms:isPartOf>",
    "<dc:identifier>", PublicHandle/binary,"</dc:identifier>",
    "<\/edm:ProvidedCHO>",
    "<ore:Aggregation rdf:about=\"", PublicHandle/binary,"_AGG\">",
    "<edm:aggregatedCHO rdf:resource=\"", PublicHandle/binary,"\"\/>",
    "<edm:dataProvider>Europeana Foundation<\/edm:dataProvider>",
    "<edm:isShownBy rdf:resource=\"http:\/\/media.culturegrid.org.uk\/mediaLibrary\/Partage\/LoveArtNouveau\/Glasgow\/DSCF4092.JPG\"\/>",
    "<edm:provider>Europeana Foundation<\/edm:provider>",
    "<edm:rights rdf:resource=\"http:\/\/rightsstatements.org\/vocab\/NoC-OKLR\/1.0\/\"\/>",
    "<\/ore:Aggregation>",
    "<\/rdf:RDF>">>).