export APP_NAME="severnata-zvezda"
export GIT_BRANCH="main"
export DOCKER_TAG="gonz/severnata-zvezda:latest"

docker pull fpco/stack-build:lts-16.31

docker-build-cacher build
docker-build-cacher cache
