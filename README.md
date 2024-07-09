
> [!CAUTION]
> This package will modify and delete tidders in Tiddlywiki. Use it with caution. 
> Create a backup and test it before using,

# tw-rscripts

R package `twrscripts` is a collections of R Scripts for [Tiddlywiki](https://tiddlywiki.com/) to manage

* authoring publications using multiple sources (ORCID, Scopus, Google Scholar, ResearcherID, homepage)
* reference list of publications (crossref and opencitations)

with following scenarios

* Tiddlywiki is configured with [Node.js](https://tiddlywiki.com/static/Installing%2520TiddlyWiki%2520on%2520Node.js.html) in localhost or no authortication requirement.
* using [TW-Refnotes](https://kookma.github.io/TW-Refnotes/) to manage reference in Tiddlywiki
* using data structure below.
* proper configurations of API authortication for multiple sources (e.g. ORCID, Scopus, ResearcherID). 

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
* field `reference` to list all references of a publication (only in the Tiddlywiki. Data obtained from [crossref](https://www.crossref.org/) and [opencitations](https://opencitations.net/)).
* field `reference-count` for number of reference in field `reference`.
* field `cited-count` for number of publications to cite this publication.

## Output and intermediate files
To avoid redownloading data from multiple resources (e.g. Google Scholar, ORCID, Web of Science/ResearcherID, Scopus), retrieved data are locally stored and reused for a period. The local folder is configured by option `output` with default `output` in the working directory. The data will be expried in certain days (90 days in default and configured by option `file_expired`). As online API constrained daily request number, only a small number of files are removed at each call (3 in default and configured by option `file_remove_max`). The maximum number of colleagues is limited for each call by option `author_max` (10 in default) to constrain request number for WebAPI.

If you run authoring as daily schedule task, all files will be gradually updated. 

## Authoring publications

The authors of a publication is searched from multiple platforms (i.e. Personal Webpage, Google Scholar, ORCID, ResearcherID and Scopus) according to related information.

### Scopus

In the tiddlers with tag `Colleague`, field `scopus` is used as author profile in [Scopus](https://www.scopus.com/). 

R package [rscopus](https://github.com/muschellij2/rscopus) is used to retrieve publication list for each colleague. See [rscopus](https://github.com/muschellij2/rscopus) to configure API Key for Scopus.


### ORCID

In the tiddlers with tag `Colleague`, field `orcid` is used as author profile in [ORCID](https://orcid.org/). 

R package [rorcid](https://github.com/ropensci-archive/rorcid) is used to retrieve works from ORCID. Package `rorcid` is archived now and there is no alternative R package to access ORCID API in R. This package use two system variables to authenticate orcid (i.e. `ORCID_CLIENT_ID` and `ORCID_CLIENT_SECRET`).

### Researcher ID

In the tiddlers with tag `Colleague`, field `researcherid` is used as author profile in [Web of Scient](https://www.webofscience.com/). 

[WOS Starter API] (https://developer.clarivate.com/apis/wos-starter) is used to retrieve works from Web of Scient.
This package use environment variable to authenticate WOS (i.e. `WOS_STARTER_KEY`).

R package [rorcid](https://github.com/ropensci-archive/rorcid) is used to retrieve works from ORCID. Package `rorcid` is archived now and there is no alternative R package to access ORCID API in R. This package use two system variables to authenticate orcid (i.e. `ORCID_CLIENT_ID` and `ORCID_CLIENT_SECRET`).

## Google Scholar

R package `scholar` is used to retrieve works from [Google Scholar](https://scholar.google.com/) according to `google-scholar`field in Tiddlywiki. As Google Scholar does not provide DOI for publications, the publication is matched according to the same published year and journal and similar title. Consequently, the matching might not be accurate.


### Colleague Homepage

In the tiddlers with tag `Colleague`, field `url` is used as author homepage. All `doi`s in the url are assumed as colleague's publications. 

## Reference list

The reference list of a publication is obtained from [crossref](https://www.crossref.org/) with R package [rcrossref](https://github.com/ropensci/rcrossref) and and [opencitations](https://opencitations.net/), and added into field `reference` in the Tiddlywiki. Two fields `reference-count` and `cited-count` is also updated for number of reference and citation for a publication, respectively. 





## Usage

* Function `works` is to retrive works from all resources above.
* Function `reference` is to get reference list.
* Function `convert_pinyin` is to convert tiddler with Chinese title into pinyin in `aka` field.
* Function `check_colleague` is to check duplicate fields for colleagues.

Alternatively, a batch job can be created for daily schedule (all records) and manully execution (new records). Here is an example R Script for all functions

```r
args = commandArgs(trailingOnly=TRUE)
#source("Rcode/function.R")
message(Sys.time())
if (length(args) == 0) {
    is_new <- TRUE
} else {
    is_new <- FALSE
}
twrscripts::tws_options(host = "http://127.0.0.1:8080/",
                        output = "output")
# Check colleagues
tryCatch({
    twrscripts::check_colleagues()    
}, warning = function(w) {
    warning(w)
    stop("Duplicates in the colleague's fields.")
})

system.time({
    message("Update works for colleagues")
    twrscripts::works(is_new = is_new)
    message("Get reference list from crossref")
    twrscripts::reference()
    message("Convert title to pinyin")
    twrscripts::convert_pinyin()
})
```

This R script can be saved into a file, e.g. tw_script.R. In Windows, a batch script can be used to schedule a daily task (e.g. tw_script.bat).

```
Rscript.exe tw_script.R update 1>standard.out 2>standdar.err
```

