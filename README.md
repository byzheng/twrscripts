
> [!CAUTION]
> This package will modify and delete tidders in Tiddlywiki. Use it with caution. 
> Create a backup and test it before using,

# tw-rscripts

R package `twrscripts` is a collections of R Scripts for Tiddlywiki to manage

* authors/colleagues of publications
* reference list of a publication

## Installation

Install the developing version from [Github](https://github.com/byzheng/twrscripts).

```r
remotes::install_github('byzheng/twrscripts')
```


## Data structure

Tiddlers with tag `Colleague` refer to a person in a real life with following fields which are used in this package.

* `url` for personal webpage, with assumption to contain a list of publications.
* `google-scholar` for personal profile to [Google Schole](https://scholar.google.com).
* `orcid` for personal profile to [ORCID](https://orcid.org/).
* `researcherid` for personal profile to [Web of Science ResearcherID](https://www.webofscience.com/wos/author/search).
* `scopus` for author profile to [Scopus](https://www.scopus.com/). 

Tiddlers with tag `bibtex-entry` refer to publication which can be imported wtih [TW-Refnotes](https://kookma.github.io/TW-Refnotes/) plugin with following tags and fields which are used 
in this package.

* `tags` with colleague names for authoring this publication.
* field `bibtex-doi` as DOI for this publication. DOI is in many functions to identify a publication
* field `reference` to list all references of a publication (only in the Tiddlywiki. Data obtained from [crossref](https://www.crossref.org/)).
* field `reference-count` for number of reference in field `reference`.
* field `cited-count` for number of publications to cite this publication.

## Output and intermediate files
To avoid redownloading data from multiple resources (e.g. Google Scholar, ORCID, Web of Science/ResearcherID, Scopus), retrieved data are locally stored and reused for a period. The local folder is configured by option `output` with default `output` in the working directory. The data will be expried in certain days (90 days in default and configured by option `file_expired`). As online API constrained daily request number, only a small number of files are removed at each call (3 in default and configured by option `file_remove_max`). The maximum number of colleagues is limited for each call by option `author_max` (10 in default) to constrain request number for WebAPI.

If you run authoring as daily schedule task, all files will be gradually updated. 

## Authoring publications

The authors of a publication is searched from multiple platforms (i.e. Personal Webpage, Google Scholar, ORCID, ResearcherID and Scopus) according to related information.

### Scopus

In the tiddlers with tag `Colleague`, field `scopus` is used as author profile in [Scopus](https://www.scopus.com/). R package [rscopus](https://github.com/muschellij2/rscopus) is used to retrieve publication list for each colleague. See [rscopus](https://github.com/muschellij2/rscopus) to configure API Key for Scopus.


