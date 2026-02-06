#!/bin/bash
set -e

# --- Configuration ---
# Update these variables according to your GitHub username and container image name.
USERNAME="sferro-besmart"
IMAGE="smartmodelv2"

# List of tags to retain (do not delete these image versions)
KEEP_TAGS=("latest" "stable" "10.3.1" "10.3.2")

# --- Dry Mode Option ---
DRY_MODE=false

# Parse command-line arguments for dry run flag.
while [[ "$1" != "" ]]; do
    case $1 in
        --dry|-d)
            DRY_MODE=true
            echo "Running in dry mode: No deletions will actually occur."
            ;;
        *)
            echo "Usage: $0 [--dry|-d]"
            exit 1
            ;;
    esac
    shift
done

# --- Retrieve Package Versions ---
# For an organization package, change the endpoint to /orgs/<org_name>/packages/container/${IMAGE}/versions
versions=$(gh api -H "Accept: application/vnd.github.v3+json" "/users/${USERNAME}/packages/container/${IMAGE}/versions")

# --- Process Each Version ---
echo "$versions" | jq -c '.[]' | while read version; do
    version_id=$(echo "$version" | jq -r '.id')
    # Get the list of tags for this version. It's assumed that tags are stored in metadata.container.tags.
    tags=$(echo "$version" | jq -r '.metadata.container.tags | join(" ")')

    # Check if any of the tags is in the whitelist.
    keep=false
    for tag in $tags; do
      for keep_tag in "${KEEP_TAGS[@]}"; do
        if [ "$tag" = "$keep_tag" ]; then
          keep=true
          break 2
        fi
      done
    done
    
    if [ "$keep" = true ]; then
      echo "Keeping version id ${version_id} with tags: ${tags}"
    else
      if [ "$DRY_MODE" = true ]; then
        echo "[Dry Mode] Would delete version id ${version_id} with tags: ${tags}"
      else
        echo "Deleting version id ${version_id} with tags: ${tags}"
        gh api --method DELETE -H "Accept: application/vnd.github.v3+json" "/users/${USERNAME}/packages/container/${IMAGE}/versions/${version_id}"
      fi
    fi
done
