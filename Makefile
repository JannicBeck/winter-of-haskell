imageName = winter-image
containerName = winter-container

run: 
	stack build --exec winter-of-haskell-exe
db:
	(docker inspect --type=image $(imageName)) || (docker build . -t $(imageName))
	(docker start $(containerName)) || (docker run --name $(containerName) -d -p 5432:5432 $(imageName))
killAll:
	docker stop $$(docker ps -a -q); docker rm $$(docker ps -a -q)