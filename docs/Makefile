build:
	bundle exec jekyll build

serve:
	bundle exec jekyll serve

deploy: build
	aws s3 sync --delete _site/ s3://yo.lukaskollmer.me
	aws cloudfront create-invalidation --distribution-id 'E1UJUK0WPHIODZ' --paths "/*"
