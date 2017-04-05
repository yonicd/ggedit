gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

openBrowser:
	R --vanilla -e 'browseURL("../docs/index.html")' 

update_online:
	cp -a ../docs/* ~/projects/ggedit/docs