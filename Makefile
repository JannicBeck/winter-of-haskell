imageName = winter-image
containerName = winter-container

run: 
	stack build --exec winter-of-haskell-exe

db:
	(docker inspect --type=image $(imageName)) || (docker build . -t $(imageName))
	(docker start $(containerName)) || (docker run --name $(containerName) -d -p 5432:5432 $(imageName))
	docker ps -a --filter "name=$(containerName)" --format "{{.Names}}: {{.Status}}"

psql:
	docker start $(containerName)
	docker exec -ti $(containerName) psql -d winter-db -U winter

kill:
	docker stop $$(docker ps -a -q); docker rm $$(docker ps -a -q)

resetdb:
	docker stop $(containerName) | true
	docker rm $(containerName) | true
	docker rmi $(imageName) | true
	make db