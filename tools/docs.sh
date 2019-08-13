#!/usr/bin/env bash

args=("$@")

build () {
    bundle exec jekyll build
}

serve () {
    bundle exec jekyll serve
}

deploy () {
    build
    aws s3 sync --delete _site/ s3://yo-lang.net
    if [ "${args[1]}" != "-s3-only" ]
    then
        aws cloudfront create-invalidation --distribution-id 'EFR5XO3UYE8F7' --paths "/*"
    fi
}

cd ./docs


case "${args[0]}" in
    "build") build ;;
    "serve") serve ;;
    "deploy") deploy ;;
    *) echo "F"; exit 1 ;;
esac
