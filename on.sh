#!/usr/bin/env zsh
set -euxo pipefail
initdb -U loiter -W /Users/h/loiter/db
createdb loiter -U loiter
