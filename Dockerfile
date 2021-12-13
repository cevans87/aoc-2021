ARG AOC21_PLATFORM=$BUILDPLATFORM
ARG AOC21_UBUNTU_TAG
FROM --platform=${AOC21_PLATFORM} ubuntu:${AOC21_UBUNTU_TAG} AS system
ARG AOC21_USER
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
        ca-certificates \
        git \
        liburing-dev \
        m4 \
        opam \
        python3 \
        rsync \
        sudo \
    && rm -rf /var/lib/apt/lists/* \
    && useradd -l -m -U -G sudo -s /bin/bash ${AOC21_USER} \
    && echo "${AOC21_USER} ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers
CMD [ "/bin/bash" ]

FROM --platform=${AOC21_PLATFORM} system AS user
ARG AOC21_OCAML_VERSION
ARG AOC21_USER
USER ${AOC21_USER}
WORKDIR /home/${AOC21_USER}
RUN git clone https://github.com/BranchTaken/Hemlock.git \
    && opam init \
        --bare \
        --disable-sandboxing \
        --dot-profile /home/${AOC21_USER}/.bashrc \
        --reinit \
        --shell-setup \
        --yes \
    && opam switch create ${AOC21_OCAML_VERSION} \
    && opam install -y \
        ocaml-lsp-server \
        ocp-indent \
        utop \
    && opam install -y ./Hemlock/bootstrap
CMD [ "/bin/bash" ]
