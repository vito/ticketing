token := $(shell cat github-token)

elm.min.js: elm.js
	uglifyjs < $< > $@

elm.notoken.js: Ticketing.elm Pagination.elm
	elm make Ticketing.elm --yes --output $@

elm.js: elm.notoken.js github-token
	sed -e "s/TOKEN-GOES-HERE/$(token)/g" elm.notoken.js > elm.js
