#!/usr/bin/env bash

curl  -v "http://172.17.0.6/oai_pmh?verb=Identify" --write-out \\n%{http_code}\\n%{content_type}\\n

curl -v --data "verb=Identify" "http://172.17.0.6/oai_pmh" --write-out \\n%{http_code}\\n%{content_type}\\n