#!/usr/bin/env bash
set -euo pipefail

IMAGE_NAME="mcp-build:latest"
ARTIFACTS_DIR="./artifacts"

# Build the Docker image
docker build -t "${IMAGE_NAME}" .

# Prepare artifacts directory on the host
sudo rm -rf "${ARTIFACTS_DIR}"
mkdir -p "${ARTIFACTS_DIR}"

chmod a+rw "${ARTIFACTS_DIR}"

# Run the image and copy artifacts out to the host
docker run --rm \
  -v "${ARTIFACTS_DIR}:/host-artifacts" \
  "${IMAGE_NAME}" \
  bash -lc "set -euo pipefail; cp -r /artifacts/* /host-artifacts/; chmod -R a+rw /host-artifacts/*"
