#!/usr/bin/env bash
# Consolidated release + publish script (image-only, no jq)
# Defaults: ghcr.io / SmartModelling / smartmodelv2
set -euo pipefail

REPO_DEFAULT="SmartModelling/SmartModelV2"
REGISTRY_DEFAULT="ghcr.io"
OWNER_DEFAULT="smartmodelling"
IMAGE_DEFAULT="smartmodelv2"
DOCKERFILE_DEFAULT="docker/Dockerfile.config"
REMOTE_DEFAULT="origin"

# Resolve repository root so script can be run from any cwd
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

print() { printf "%s\n" "$*"; }
err()  { printf "ERROR: %s\n" "$*" >&2; exit 1; }
info() { printf "ℹ %s\n" "$*"; }

usage() {
  cat <<EOF
Usage: $0 [--version X.Y.Z | --bump {patch,minor,major}] [options]

Options:
  --version X.Y.Z      Use explicit semantic version (accepts v-prefix)
  --bump patch|minor|major  Increment latest git tag's semver
  --no-push            Don't push Docker image to registry
  --no-release         Don't create GitHub Release (skip API call)
  --dockerfile PATH    Dockerfile to build (default: $DOCKERFILE_DEFAULT)
  --repo OWNER/NAME    Repo (default: $REPO_DEFAULT)
  --dry-run            Print actions without executing
  --no-upstream        Do not push git tag/release to remote named 'org_upstream'
  -h, --help           Show this help
EOF
  exit 0
}

is_semver() { [[ "$1" =~ ^v?[0-9]+\.[0-9]+\.[0-9]+$ ]]; }

bump_version() {
  local ver=$1; local part=$2
  IFS='.' read -r major minor patch <<< "${ver#v}"
  case "$part" in
    major) ((major++, minor=0, patch=0)) ;;
    minor) ((minor++, patch=0)) ;;
    patch) ((patch++)) ;;
    *) err "invalid bump: $part" ;;
  esac
  echo "${major}.${minor}.${patch}"
}

DO_PUSH=true
DO_RELEASE=true
DOCKERFILE="$DOCKERFILE_DEFAULT"
REPO="$REPO_DEFAULT"
DRY_RUN=false
NO_UPSTREAM=false
VERSION=""
BUMP=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --version) VERSION="$2"; shift 2 ;;
    --bump) BUMP="$2"; shift 2 ;;
    --no-push) DO_PUSH=false; shift ;;
    --no-release) DO_RELEASE=false; shift ;;
    --dockerfile) DOCKERFILE="$2"; shift 2 ;;
    --repo) REPO="$2"; shift 2 ;;
    --dry-run) DRY_RUN=true; shift ;;
    --no-upstream) NO_UPSTREAM=true; shift ;;
    -h|--help) usage ;;
    *) err "Unknown arg: $1" ;;
  esac
done

GITHUB_TOKEN="${GITHUB_TOKEN:-}"
GHCR_PAT="${GHCR_PAT:-}"
GHCR_USER="${GHCR_USER:-}"
REMOTE="${REMOTE:-$REMOTE_DEFAULT}"

get_latest_tag() { git describe --tags --abbrev=0 2>/dev/null || echo "v0.0.0"; }

if [ -n "$BUMP" ]; then
  latest=$(get_latest_tag)
  new=$(bump_version "$latest" "$BUMP")
  VERSION="$new"
fi

if [ -z "$VERSION" ]; then err "Missing version. Use --version or --bump."; fi
VERSION="${VERSION#v}"
if ! is_semver "v$VERSION"; then err "Invalid semver: $VERSION"; fi
TAG="v$VERSION"
IMAGE_TAG="$TAG"

REGISTRY="${REGISTRY:-$REGISTRY_DEFAULT}"
OWNER="${OWNER:-$OWNER_DEFAULT}"
IMAGE_NAME="${IMAGE_NAME:-$IMAGE_DEFAULT}"
FULL_IMAGE="${REGISTRY}/${OWNER}/${IMAGE_NAME}:${IMAGE_TAG}"
LATEST_IMAGE="${REGISTRY}/${OWNER}/${IMAGE_NAME}:latest"

run() {
  if [ "$DRY_RUN" = true ]; then
    printf "DRY RUN: %s\n" "$*"
  else
    eval "$@"
  fi
}

if [ "$(git status --porcelain)" != "" ]; then err "Working directory not clean. Commit or stash changes."; fi
if git rev-parse "$TAG" >/dev/null 2>&1; then err "Tag $TAG already exists."; fi

info "Preparing release $TAG -> $FULL_IMAGE"
info "Dockerfile: $DOCKERFILE"
info "Project root: $PROJECT_ROOT"

# build using repository root as the context so script works from any cwd
run "docker build -f '$PROJECT_ROOT/$DOCKERFILE' -t '$FULL_IMAGE' '$PROJECT_ROOT'"
run "docker tag '$FULL_IMAGE' '$LATEST_IMAGE'"

if [ "$DO_PUSH" = true ]; then
  TOKEN="${GHCR_PAT:-$GITHUB_TOKEN}"
  if [ -z "$TOKEN" ]; then err "No token found. Set GHCR_PAT (local) or ensure GITHUB_TOKEN is available (CI)."; fi

  info "Logging into $REGISTRY"
  run "printf '%s' '$TOKEN' | docker login '$REGISTRY' -u '${GHCR_USER:-$OWNER}' --password-stdin"

  info "Pushing $FULL_IMAGE"
  run "docker push '$FULL_IMAGE'"

  info "Pushing $LATEST_IMAGE"
  run "docker push '$LATEST_IMAGE'"

  run "docker logout '$REGISTRY'"
else
  info "--no-push specified — skipping push"
fi

info "Creating git tag $TAG"
run "git tag -a '$TAG' -m 'Release $TAG'"
info "Pushing tag to remote $REMOTE"
run "git push '$REMOTE' '$TAG'"

# If an org_upstream remote exists, also push the tag there unless explicitly skipped
if [ "$NO_UPSTREAM" != true ]; then
  if git remote get-url org_upstream >/dev/null 2>&1; then
    info "org_upstream remote found — pushing tag to org_upstream"
    run "git push org_upstream '$TAG'"
  else
    info "No 'org_upstream' remote configured — skipping org push"
  fi
else
  info "--no-upstream specified — skipping push to org_upstream"
fi

if [ "$DO_RELEASE" = true ]; then
  GH_API_TOKEN="${GITHUB_TOKEN:-$GHCR_PAT}"
  if [ -z "$GH_API_TOKEN" ]; then err "No token for GitHub API available (set GITHUB_TOKEN or GHCR_PAT)."; fi

  if git remote get-url "$REMOTE" >/dev/null 2>&1; then
    remote_url=$(git remote get-url "$REMOTE")
    if [[ "$remote_url" =~ github.com[:/]+([^/]+/[^/.]+) ]]; then
      REPO="${BASH_REMATCH[1]}"
    fi
  fi

  info "Creating GitHub Release for $REPO $TAG"
  read -r -d '' post_data <<EOF || true
{"tag_name":"$TAG","name":"$TAG","body":"Release $TAG","draft":false,"prerelease":false}
EOF

  if [ "$DRY_RUN" = true ]; then
    printf "DRY RUN: would POST release for %s with payload:\n%s\n" "$REPO" "$post_data"
  else
    curl -sS -X POST \
      -H "Authorization: token $GH_API_TOKEN" \
      -H "Accept: application/vnd.github+json" \
      "https://api.github.com/repos/$REPO/releases" \
      -d "$post_data" \
      | sed -n '1p'
  fi
else
  info "--no-release specified — skipping GitHub Release creation"
fi

info "Done — $TAG"
