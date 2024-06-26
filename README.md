
> [!CAUTION]
> This package will modify and delete tidders in Tiddlywiki. Use it with caution. 
> Create a backup and test it before using,

# tw-rscripts

R package `twrscripts` is a collections of R Scripts for Tiddlywiki to manage

* authors/colleagues of publications
* reference list of a publication


## Data structure

Tiddlers with tag `Colleague` refer to a person in a real life with following fields which are used 
in this package.

* `url` for personal webpage, with assumption to contain a list of publications.
* `google-scholar` for personal profile to [Google Schole](https://scholar.google.com).
* `orcid` for personal profile to [ORCID](https://orcid.org/).
* `researcherid` for personal profile to [Web of Science ResearcherID](https://www.webofscience.com/wos/author/search).
* `scopus` for author profile to [Scopus](https://www.scopus.com/). 

Tiddlers with tag `bibtex-entry` refer to publication which can be imported wtih [TW-Refnotes](https://kookma.github.io/TW-Refnotes/) plugin with following tags and fields which are used 
in this package.

* tags with colleague names for authoring this publication.
* field `bibtex-doi` as DOI for this publication. DOI is in many functions to identify a publication
* field `reference` to list all references of a publication (only in the Tiddlywiki).
* field `reference-count` for number of reference in field `reference`.
* field `cited-count` for number of publications to cite this publication.

## Output and intermediate files


## Authoring publications

The authors of a publication is searched from multiple platforms (i.e. Personal Webpage, Google Scholar, ORCID, ResearcherID and Scopus) according to related information.
