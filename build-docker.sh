export APP_NAME="severnata-zvezda"
export GIT_BRANCH="main"
export DOCKER_TAG="severnata-zvezda:latest"

docker-build-cacher build
docker-build-cacher cache