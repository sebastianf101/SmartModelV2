#!/usr/bin/env bash
set -euo pipefail

REPO_DEFAULT="SmartModelling/SmartModelV2"
REGISTRY="ghcr.io"
OWNER="smartmodelling"
IMAGE_NAME="smartmodelv2"
DOCKERFILE_DEFAULT="docker/Dockerfile.config"
REMOTE="origin"
UPSTREAM="org_upstream"

# Resolve repo root
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
  --remote NAME        Primary git remote (default: origin)
  --upstream NAME      Secondary git remote (default: org_upstream)
  --dry-run            Print actions without executing
  --force              Delete existing tag/release and recreate (use with caution)
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
FORCE=false
VERSION=""
BUMP=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --version) VERSION="$2"; shift 2 ;;
    --bump) BUMP="$2"; shift 2 ;;
    --no-push) DO_PUSH=false; shift ;;
    --no-release) DO_RELEASE=false; shift ;;
    --dockerfile) DOCKERFILE="$2"; shift 2 ;;
    --remote) REMOTE="$2"; shift 2 ;;
    --upstream) UPSTREAM="$2"; shift 2 ;;
    --dry-run) DRY_RUN=true; shift ;;
    --force) FORCE=true; shift ;;
    -h|--help) usage ;;
    *) err "Unknown arg: $1" ;;
  esac
done

GITHUB_TOKEN="${GITHUB_TOKEN:-}"
GHCR_PAT="${GHCR_PAT:-}"
GHCR_USER="${GHCR_USER:-}"

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

REGISTRY="${REGISTRY:-ghcr.io}"
OWNER="${OWNER:-smartmodelling}"
IMAGE_NAME="${IMAGE_NAME:-smartmodelv2}"
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
if git rev-parse "$TAG" >/dev/null 2>&1; then
  if [ "$FORCE" = true ]; then
    info "Tag $TAG exists — --force specified: removing existing tag locally and on remotes"
    run "git tag -d '$TAG' || true"
    info "Deleting tag $TAG from remote '$REMOTE'"
    run "git push '$REMOTE' --delete '$TAG' || true"
    if git remote get-url "$UPSTREAM" >/dev/null 2>&1; then
      info "Deleting tag $TAG from upstream remote '$UPSTREAM'"
      run "git push '$UPSTREAM' --delete '$TAG' || true"
    fi
  else
    err "Tag $TAG already exists. Use --force to replace it."
  fi
fi

info "Preparing release $TAG -> $FULL_IMAGE"
info "Dockerfile: $DOCKERFILE"
info "Project root: $PROJECT_ROOT"

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

# push tag to both configured remotes (primary then upstream)
info "Pushing tag to remote '$REMOTE'"
run "git push '$REMOTE' '$TAG'"

if git remote get-url "$UPSTREAM" >/dev/null 2>&1; then
  info "Pushing tag to upstream remote '$UPSTREAM'"
  run "git push '$UPSTREAM' '$TAG'"
else
  info "Upstream remote '$UPSTREAM' not configured — skipping upstream push"
fi

if [ "$DO_RELEASE" = true ]; then
  GH_API_TOKEN="${GITHUB_TOKEN:-$GHCR_PAT}"
  if [ -z "$GH_API_TOKEN" ]; then err "No token for GitHub API available (set GITHUB_TOKEN or GHCR_PAT)."; fi

  repos_to_release=()
  for r in "$REMOTE" "$UPSTREAM"; do
    if git remote get-url "$r" >/dev/null 2>&1; then
      remote_url=$(git remote get-url "$r")
      if [[ "$remote_url" =~ github.com[:/]+([^/]+/[^/.]+) ]]; then
        repos_to_release+=("${BASH_REMATCH[1]}")
      fi
    fi
  done

  # dedupe and fallback
  IFS=$'\n' unique_repos=($(printf "%s\n" "${repos_to_release[@]}" | awk '!seen[$0]++'))
  if [ ${#unique_repos[@]} -eq 0 ]; then
    unique_repos=("$REPO_DEFAULT")
  fi

  read -r -d '' post_data <<EOF || true
{"tag_name":"$TAG","name":"$TAG","body":"Release $TAG","draft":false,"prerelease":false}
EOF

  for target_repo in "${unique_repos[@]}"; do
    if [ "$FORCE" = true ]; then
      info "--force: checking for existing GitHub Release for ${target_repo} $TAG and deleting if present"
      if [ "$DRY_RUN" = true ]; then
        printf "DRY RUN: would delete existing GitHub Release for %s %s\n" "$target_repo" "$TAG"
      else
        release_json=$(curl -sS -H "Authorization: token $GH_API_TOKEN" "https://api.github.com/repos/${target_repo}/releases/tags/${TAG}" || true)
        release_id=$(printf '%s' "$release_json" | sed -n 's/.*"id":[[:space:]]*\([0-9][0-9]*\).*/\1/p' | head -n1 || true)
        if [ -n "$release_id" ]; then
          info "Deleting GitHub release id $release_id for ${target_repo} $TAG"
          curl -sS -X DELETE -H "Authorization: token $GH_API_TOKEN" "https://api.github.com/repos/${target_repo}/releases/${release_id}" || true
        fi
      fi
    fi
    info "Creating GitHub Release for ${target_repo} $TAG"
    if [ "$DRY_RUN" = true ]; then
      printf "DRY RUN: would POST release for %s with payload:\n%s\n" "$target_repo" "$post_data"
    else
      curl -sS -X POST \
        -H "Authorization: token $GH_API_TOKEN" \
        -H "Accept: application/vnd.github+json" \
        "https://api.github.com/repos/${target_repo}/releases" \
        -d "$post_data" \
        | sed -n '1p'
    fi
  done
else
  info "--no-release specified — skipping GitHub Release creation"
fi

info "Done — $TAG"
