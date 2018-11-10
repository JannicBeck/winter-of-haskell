FROM postgres:11.0
COPY scripts/ /docker-entrypoint-initdb.d/