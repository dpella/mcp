# syntax=docker/dockerfile:1.7-labs

FROM ubuntu:noble as build-package

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Args
ARG USER_NAME=haskell
ARG GHC_VERSION=9.12.2
ARG CABAL_VERSION=3.16.0.0
ARG UID=1001
ARG GID=1001

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Europe/Stockholm \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    USER_NAME=${USER_NAME}\
    UID=${UID} \
    GID=${GID}


# Install dependencies
# We ignore the "pin versions" warning here, as we are not aiming for long-term
# stability.
# hadolint ignore=DL3008
RUN --mount=type=cache,id=apt-cache-mcp,target=/var/cache/apt \
    --mount=type=cache,id=apt-libs-mcp,target=/var/lib/apt \
    ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
    echo $TZ > /etc/timezone && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
      sudo \
      git \
      curl \
      ca-certificates \
      locales \
      build-essential \
      libffi-dev \
      libgmp-dev \
      zlib1g-dev \
      && \
    apt-get autoremove -y && \
    apt-get clean -y && \
    sed -i 's/^# *en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen && locale-gen && \
    rm -rf /var/lib/apt/lists/*

# user
RUN groupadd -g "$GID" -o "$USER_NAME" && \
    useradd -l -m -u "$UID" -g "$GID" -G sudo -o -s /bin/bash -d /home/$USER_NAME "$USER_NAME" && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# Switch to the new user
USER ${UID}:${GID}
WORKDIR /home/$USER_NAME

# toolchain env
ENV GHCUP_INSTALL_BASE_PREFIX=/home/$USER_NAME \
    HOME=/home/$USER_NAME \
    PATH=/home/$USER_NAME/.cabal/bin:/home/$USER_NAME/.ghcup/bin:$PATH \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_NO_UPGRADE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    BOOTSTRAP_HASKELL_INSTALL=0

# ghcup + toolchain
RUN curl -fsSL https://get-ghcup.haskell.org -o /tmp/get-ghcup.sh && \
    chmod +x /tmp/get-ghcup.sh && \
    /tmp/get-ghcup.sh && \
    ghcup install ghc "$GHC_VERSION" && \
    ghcup set ghc "$GHC_VERSION" && \
    ghcup install cabal "$CABAL_VERSION" && \
    cabal --version && ghc --version


# We copy only the cabal files, since these won't change usually. This lets us avoid
# rebuilding the dependencies all the time.
COPY --parents --chown=${UID}:${GID} */*.cabal */*/*.cabal /app/
COPY --chown=${UID}:${GID} cabal.project /app/cabal.project

WORKDIR /app
# Using the cabal files, we can build the dependencies
RUN --mount=type=cache,id=mcp-build-cache,uid=${UID},gid=${GID},target=/app/dist-newstyle \
    cabal update && \
    cabal build all --only-dependencies --haddock-all --project-file=cabal.project --project-dir=/app

COPY --link --chown=${UID}:${GID} mcp-types/src /app/mcp-types/src
COPY --link --chown=${UID}:${GID} mcp-types/test /app/mcp-types/test
COPY --link --chown=${UID}:${GID} mcp-server/src /app/mcp-server/src
COPY --link --chown=${UID}:${GID} mcp-server/test /app/mcp-server/test
COPY --link --chown=${UID}:${GID} mcp-server/example/Main.hs /app/mcp-server/example/Main.hs

# Build all the packages
RUN --mount=type=cache,id=mcp-build-cache,uid=${UID},gid=${GID},target=/app/dist-newstyle \
     cabal build all --haddock-all --project-file=cabal.project --project-dir=/app

COPY --link --chown=${UID}:${GID} *.md /app/
COPY --link --chown=${UID}:${GID} mcp-types/CHANGELOG.md mcp-types/README.md mcp-types/LICENSE /app/mcp-types/
COPY --link --chown=${UID}:${GID} mcp-server/CHANGELOG.md mcp-server/README.md mcp-server/LICENSE /app/mcp-server/

RUN --mount=type=cache,id=mcp-build-cache,uid=${UID},gid=${GID},target=/app/dist-newstyle \
     cabal haddock all --haddock-for-hackage --project-file=cabal.project --project-dir=/app  && \
     cabal test all --enable-tests --project-file=cabal.project --project-dir=/app

COPY --link --chown=${UID}:${GID} scripts /app/scripts

USER root

RUN --mount=type=cache,id=mcp-build-cache,uid=${UID},gid=${GID},target=/app/dist-newstyle \
    set -euo pipefail; \
    mkdir -p /artifacts/docs; \
    find /app/dist-newstyle/build -type d -path '*/doc/html' -exec cp -r {} /artifacts/docs \;

USER ${UID}:${GID}
# Default command
CMD ["true"]
