run: 
	stack build && stack exec winter-of-haskell-exe
db:
	docker run --name winter-postgres -e POSTGRES_PASSWORD=postgres -d -p 5432:5432 postgres
admin:
	docker run -v pgadmin4:/home/pgadmin/.pgadmin -p 5050:5050 -e --link winter-postgres:winter-postgres --name pgadmin -d meedan/pgadmin; sleep 3; sensible-browser http://localhost:5050
killAll:
	docker stop $$(docker ps -a -q); docker rm $$(docker ps -a -q)