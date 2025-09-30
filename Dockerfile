FROM ubuntu:22.04
RUN apt-get update
RUN apt-get install -y locales locales-all
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y -qq --no-install-recommends \
    build-essential \
    cmake \
    clang \
    mono-complete \
    openjdk-8-jdk \
    ruby \
    gcc-multilib \
    g++-multilib \
    python3 \
    python3-pip \
    z3
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y -qq --no-install-recommends \
    ghc \
    alex
ADD https://downloads.haskell.org/~cabal/cabal-install-3.8.1.0-rc1/cabal-install-3.8.0.20220526-x86_64-linux-deb10.tar.xz /tmp/
RUN tar -xf /tmp/cabal-install-3.8.0.20220526-x86_64-linux-deb10.tar.xz -C /usr/local/bin/
RUN cabal update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y -qq --no-install-recommends \
    vim \
    nano \
    emacs-nox
RUN pip install \
    numpy \
    matplotlib
COPY . /mocheqos
WORKDIR /mocheqos
RUN cp -r aux/ /root/.cabal/
RUN mv experiments/ experiments.bak/ && \
    mv wiki/ wiki.bak/
ENV PATH="$PATH:/root/.cabal/bin"
RUN cabal install
ENTRYPOINT \
    cp -r experiments.bak/* experiments/ && \
    cp -r wiki.bak/* wiki/ && \
    /bin/bash
