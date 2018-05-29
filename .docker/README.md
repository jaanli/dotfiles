# Dockerfile for ubuntu c++ development environment

Installing on mac: https://pilsniak.com/how-to-install-docker-on-mac-os-using-brew/

Starting docker daemon:
```
docker-machine create default --driver xhyve --xhyve-experimental-nfs-shareu
docker-machine start default
eval $(docker-machine env)
```

Build the docker image from the dockerfile after starting docker daemon:
```
# cd to directory of DockerFile
cd ~/.docker
# copy id_rsa, id_rsa.pub
cp ~/.ssh/id_rsa ~/.ssh/id_rsa.pub .
docker build .
```

```
# run privileged
docker run --privileged -it -v /Users:/Users BUILT_IMAGE_HASH
# follow steps in dockerfiles that need to be done manually: https://github.com/altosaar/dotfiles
# rename the image using commit with the above changes
docker commit CONTAINER_NAME working-main-dev

# then can detach and reattach from the container using <ctrl+e, e>.
# to stop
docker stop working-main-dev
# can restart with 
docker start working-main-dev
```

Build:
```
cp ~/.ssh/* .
# need privileged: https://rubicks.github.io/post/docker-run-privileged-gdb/
docker run --privileged -it docker-dev
```

Attach:
`docker attach <TAB>`

Detach:
<ctrl+e,e>

Workflow:
* Leave container running (about 5-10 minutes to set it up again)
* Edit files in the mounted directory (/Users/jaan/Dropbox)
* Push changes to github repo after testing in the container
* Run larger jobs on remote server (e.g. tiger)

