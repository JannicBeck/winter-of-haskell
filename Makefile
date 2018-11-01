run: 
	stack build --exec winter-of-haskell-exe
db:
	docker run --name winter-postgres -e POSTGRES_USER=winter -e POSTGRES_PASSWORD=winter -e POSTGRES_DB=winter-db -d -p 5432:5432 postgres
killAll:
	docker stop $$(docker ps -a -q); docker rm $$(docker ps -a -q)