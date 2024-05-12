#!/usr/bin/env bash
set -e
set -x

# build docker images
docker-compose down && docker-compose build
# start database
docker-compose up -d db
# init db
until cat db_schema.sql |  docker exec -i paragraph_api_db_1 psql -U postgres
do
  echo "Waiting for postgres"
  sleep 2;
done
# start api server
docker-compose up -d apiserver
