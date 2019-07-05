FROM ubuntu:18.04

# Fix "Couldn't register with accessibility bus" error message
ENV NO_AT_BRIDGE=1

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y curl

# Install npm
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -

# basic stuff
RUN echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf \
    && apt-get update && apt-get install \
    bash \
    build-essential \
    dbus-x11 \
    fontconfig \
    git \
    gzip \
    wget \
    language-pack-en-base \
    libgl1-mesa-glx \
    make \
    sudo \
    tar \
    unzip \
    python3 \
    python3-pip \
    python3-dev \
    astyle \
    nodejs \
# su-exec
    && git clone https://github.com/ncopa/su-exec.git /tmp/su-exec \
    && cd /tmp/su-exec \
    && make \
    && chmod 770 su-exec \
    && mv ./su-exec /usr/local/sbin/ \
# Cleanup
    && apt-get autoremove \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*

COPY asEnvUser /usr/local/sbin/

# Only for sudoers
RUN chown root /usr/local/sbin/asEnvUser \
    && chmod 700  /usr/local/sbin/asEnvUser

# ^^^^^^^ Those layers are shared ^^^^^^^

# Install python dependencies
RUN pip3 install --upgrade pip && \
    sudo -H pip3 install tensorflow pandas pylint youtube-dl \
    yapf autopep8 jedi flake8 rope_py3k

# Install npm dependencies
RUN npm install -g jshint

# Install cpplint
RUN wget 'https://raw.githubusercontent.com/google/styleguide/gh-pages/cpplint/cpplint.py' -O /usr/local/bin/cpplint.py && \
    chmod a+x /usr/local/bin/cpplint.py

# Emacs
RUN apt-get update && apt-get install software-properties-common \
    && apt-add-repository ppa:kelleyk/emacs \
    && apt-get update && apt-get install emacs26 \
# Cleanup
    && apt-get purge software-properties-common \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*

ENV UNAME=emacser \
    GNAME=emacs \
    UHOME=/home/emacs \
    UID=1000 \
    GID=1000 \
    WORKSPACE=/mnt/workspace \
    SHELL=/bin/bash

# Install source code pro font
RUN mkdir -p /usr/share/fonts && \
    cd /usr/share/fonts && \
    wget 'https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.tar.gz' && \
    tar xzf 1.050R-it.tar.gz && \
    rm 1.050R-it.tar.gz && \
    fc-cache -f -v

RUN mkdir -p $UHOME/org
RUN mkdir -p $UHOME/.emacs.d/semanticdb

COPY .gitconfig_ub $UHOME/.gitconfig

COPY . $UHOME/.emacs.d

RUN asEnvUser emacs -nw -batch -u ${UNAME} -q -kill

WORKDIR "${WORKSPACE}"

ENTRYPOINT ["asEnvUser"]
CMD ["bash", "-c", "emacs; /bin/bash"]

# docker build -t emacs -f emacs.Dockerfile . && docker run -it -v /tmp/.X11-unix:/tmp/.X11-unix:ro -e DISPLAY="unix$DISPLAY" -e UNAME="gros" -e GNAME="emacsers" -e UID="$(id -u)" -e GID="$(id -g)" -v /tmp/test:/mnt/workspace emacs emacs
