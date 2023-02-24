FROM ubuntu:22.04

RUN apt update

RUN apt install -y make
RUN apt install -y default-jdk
RUN apt install -y scala
RUN apt install -y ocaml

RUN apt install -y git tmux

RUN mkdir -p /home/app

ADD "./init.sh" "/home/app"

WORKDIR "/home/app"

CMD ["./keep_alive.sh"]
