# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.191.0/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version: bionic, focal
ARG VARIANT="focal"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

ENV EDITOR=vim

# [Optional] Uncomment this section to install additional OS packages.
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    build-essential \
    mit-scheme \
    chicken-bin \
    racket \
    racket-doc \
    vim
