#!/usr/bin/env bash
# Renders each test/cases/*/golden on RedMica and saves the screenshot
# as test/cases/*/redmica.png.
set -euo pipefail
cd "$(dirname "$0")"
compose() { HOST_UID=$(id -u) HOST_GID=$(id -g) docker compose "$@"; }
trap 'compose --profile shoot down -v' EXIT

compose up -d --build redmica
echo 'waiting for RedMica...'
until compose exec -T redmica ruby -rnet/http -e 'Net::HTTP.get(URI("http://localhost:3000/"))' 2>/dev/null; do
  sleep 3
done
compose exec -T redmica bin/rails runner /seed.rb
compose run --rm shooter
