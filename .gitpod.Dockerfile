FROM gitpod/workspace-full:latest

USER root
RUN apt-get update && apt-get install -y curl xz-utils gcc make libtinfo5 libgmp-dev zlib1g-dev
RUN mkdir /workspace/.stack && chown gitpod /workspace/.stack && \
    mkdir /workspace/.cabal && chown gitpod /workspace/.cabal && \
    mkdir /workspace/.local && chown gitpod /workspace/.local

USER gitpod
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=/workspace/.local/bin:/workspace/.cabal/bin:$HOME/.ghcup/bin:$PATH
ENV STACK_ROOT=/workspace/.stack
ENV CABAL_DIR=/workspace/.cabal

RUN ghcup install ghc && \
    ghcup install cabal && \
    ghcup install stack && \
    ghcup install hls && \
    ghcup set ghc && \
    stack config set install-ghc false --global && \
    stack config set system-ghc true --global && \
    echo 'local-bin-path: /workspace/.local/bin' >> /workspace/.stack/config.yaml

RUN cabal update && stack update
