FROM fpco/stack-build:lts-16.31 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

FROM fpco/stack-build:lts-16.31 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# Running the actual application
FROM ubuntu:20.10 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

# Arguments

ARG YESOD_PORT
ENV YESOD_PORT=${YESOD_PORT:-3000}
EXPOSE ${YESOD_PORT}/tcp

RUN apt update && apt install -y ca-certificates libpq5 libgmp10

COPY --from=build /opt/build/bin .
COPY static /opt/app/static
COPY config /opt/app/config

CMD ["/opt/app/severnata-zvezda"]
