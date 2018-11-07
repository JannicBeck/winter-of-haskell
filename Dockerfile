FROM postgres:11.0
COPY scripts/init-db.sql /docker-entrypoint-initdb.d/