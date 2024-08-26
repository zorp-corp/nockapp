FROM rust:1.80.1-bookworm AS build

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y libclang-dev

WORKDIR /usr/src/nockapp
COPY Cargo.lock .
COPY Cargo.toml .
COPY rust-toolchain.toml .

RUN mkdir crown
COPY crown/Cargo.toml crown

RUN mkdir http-app
COPY http-app/Cargo.toml http-app

RUN mkdir choo
COPY choo/Cargo.toml choo

RUN cargo vendor > config.toml

COPY ./crown/ crown/
COPY ./choo/ choo/
COPY ./http-app/ http-app/

RUN cargo build --release -p http-app

# -----------------
# Final Stage
# -----------------

FROM debian:stable-slim
COPY --from=build /usr/src/nockapp/target/release/http-app /bin
CMD /bin/http-app serve-message hi

