# Dockerfile for ubuntu c++ development environment

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


