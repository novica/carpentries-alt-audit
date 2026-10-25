#!/usr/bin/env bash
# Shallow-clone (or update) every lesson in lessons.txt into lessons/.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p lessons
grep -vE '^\s*(#|$)' lessons.txt | while read -r repo; do
  dir="lessons/${repo#*/}"
  if [ -d "$dir/.git" ]; then
    git -C "$dir" pull -q --depth 1
  else
    git clone -q --depth 1 "https://github.com/$repo.git" "$dir"
  fi
done
