#!/usr/bin/env sh

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" -d "$POSTGRES_DB"  <<-EOSQL
  CREATE DATABASE "severnata-zvezda" WITH OWNER = "postgres";
EOSQL