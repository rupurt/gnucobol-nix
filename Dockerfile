# syntax = docker/dockerfile:1.4
FROM nixos/nix:latest AS builder
WORKDIR /tmp/build
RUN mkdir /tmp/output
RUN mkdir /tmp/nix-store-closure
RUN mkdir /tmp/nix-store-closure-gcsort
COPY flake.lock flake.lock
COPY flake.nix flake.nix
COPY packages packages
# CI copies nix config for cachix & gha magic cache
COPY ./*.config/nix /root/.config/nix
COPY ./*.config/cachix /root/.config/cachix
# gnucobol
RUN \
  --mount=type=cache,target=/nix,from=nixos/nix:latest,source=/nix \
  --mount=type=cache,target=/root/.cache \
  --mount=type=bind,target=/tmp/build \
  <<EOF
  nix \
    --extra-experimental-features "nix-command flakes" \
    --option filter-syscalls false \
    --show-trace \
    --log-format raw \
    build .#gnucobol --out-link /tmp/output/gnucobol
  cp -R $(nix-store -qR /tmp/output/gnucobol-bin) /tmp/nix-store-closure
EOF
# gnucobol-db
RUN \
  --mount=type=cache,target=/nix,from=nixos/nix:latest,source=/nix \
  --mount=type=cache,target=/root/.cache \
  --mount=type=bind,target=/tmp/build \
  <<EOF
  nix \
    --extra-experimental-features "nix-command flakes" \
    --option filter-syscalls false \
    --show-trace \
    --log-format raw \
    build .#gnucobol-db --out-link /tmp/output/gnucobol-db
  cp -R $(nix-store -qR /tmp/output/gnucobol-db-bin) /tmp/nix-store-closure
EOF
# gnucobol-visam
RUN \
  --mount=type=cache,target=/nix,from=nixos/nix:latest,source=/nix \
  --mount=type=cache,target=/root/.cache \
  --mount=type=bind,target=/tmp/build \
  <<EOF
  nix \
    --extra-experimental-features "nix-command flakes" \
    --option filter-syscalls false \
    --show-trace \
    --log-format raw \
    build .#gnucobol-visam --out-link /tmp/output/gnucobol-visam
  cp -R $(nix-store -qR /tmp/output/gnucobol-visam-bin) /tmp/nix-store-closure
EOF
# gnucobol-vbisam
RUN \
  --mount=type=cache,target=/nix,from=nixos/nix:latest,source=/nix \
  --mount=type=cache,target=/root/.cache \
  --mount=type=bind,target=/tmp/build \
  <<EOF
  nix \
    --extra-experimental-features "nix-command flakes" \
    --option filter-syscalls false \
    --show-trace \
    --log-format raw \
    build .#gnucobol-vbisam --out-link /tmp/output/gnucobol-vbisam
  cp -R $(nix-store -qR /tmp/output/gnucobol-vbisam-bin) /tmp/nix-store-closure
EOF
# gcsort
RUN \
  --mount=type=cache,target=/nix,from=nixos/nix:latest,source=/nix \
  --mount=type=cache,target=/root/.cache \
  --mount=type=bind,target=/tmp/build \
  <<EOF
  nix \
    --extra-experimental-features "nix-command flakes" \
    --option filter-syscalls false \
    --show-trace \
    --log-format raw \
    build .#gcsort --out-link /tmp/output/gcsort
  cp -R $(nix-store -qR /tmp/output/gcsort) /tmp/nix-store-closure-gcsort
  cp -R $(nix-store -qR /tmp/output/gcsort) /tmp/nix-store-closure
EOF
# esqloc
RUN \
  --mount=type=cache,target=/nix,from=nixos/nix:latest,source=/nix \
  --mount=type=cache,target=/root/.cache \
  --mount=type=bind,target=/tmp/build \
  <<EOF
  nix \
    --extra-experimental-features "nix-command flakes" \
    --option filter-syscalls false \
    --show-trace \
    --log-format raw \
    build .#esqloc --out-link /tmp/output/esqloc
  cp -R $(nix-store -qR /tmp/output/esqloc) /tmp/nix-store-closure
EOF

FROM alpine
WORKDIR /app
COPY --from=builder /tmp/nix-store-closure-gcsort /nix/store-gcsort
COPY --from=builder /tmp/nix-store-closure /nix/store
COPY --from=builder /tmp/output/ /app/
