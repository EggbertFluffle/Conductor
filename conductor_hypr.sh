#!/usr/bin/env bash

set -euo pipefail

CONDUCTOR_BIN="${CONDUCTOR_BIN:-$HOME/programs/haskell/Programming_Language_Design/Conductor/dist-newstyle/build/x86_64-linux/ghc-9.6.7/conductor-0.1.0.0/x/conductor/build/conductor/conductor}"
SNIPPET_FILE="${SNIPPET_FILE:-$HOME/programs/haskell/Programming_Language_Design/Conductor/conductor.snippet}"
HYPRCTL_BIN="${HYPRCTL_BIN:-hyprctl}"

function get_screen_dimensions() {
    local monitor
    monitor=$("$HYPRCTL_BIN" monitors -j | jq '.[] | select(.focused == true)')
    local width height
    width=$(echo "$monitor" | jq -r '.width')
    height=$(echo "$monitor" | jq -r '.height')
    echo "{\"width\": $width, \"height\": $height}"
}

function get_windows() {
    "$HYPRCTL_BIN" clients -j
}

function get_window_ids() {
    local windows_json="$1"
    echo "$windows_json" | jq -r '[.[] | .address | sub("^0x"; "") | tonumber]'
}

function apply_transform() {
    local addr=$1
    local x=$2
    local y=$3
    local w=$4
    local h=$5

    "$HYPRCTL_BIN" dispatch movewindow "$addr" "$x,$y"
    "$HYPRCTL_BIN" dispatch resizeactive "$addr" "${w}x${h}"
}

function main() {
    if [[ ! -x "$CONDUCTOR_BIN" ]]; then
        echo "Error: Conductor binary not found or not executable: $CONDUCTOR_BIN" >&2
        exit 1
    fi

    if [[ ! -f "$SNIPPET_FILE" ]]; then
        echo "Error: Snippet file not found: $SNIPPET_FILE" >&2
        exit 1
    fi

    local screen size_json windows_json window_ids snippet

    screen=$(get_screen_dimensions)
    size_json="$screen"

    windows_json=$(get_windows)
    window_ids=$(get_window_ids "$windows_json")

    snippet=$(cat "$SNIPPET_FILE" | jq -Rs . | sed 's/\\n/\\\\n/g')

    local input_json
    input_json=$(jq -n \
        --arg start "start" \
        --arg snippet "$snippet" \
        --argjson max_depth 25 \
        --argjson screen "$size_json" \
        --argjson ids "$window_ids" \
        '{
            starting_variable: $start,
            snippet: $snippet,
            max_depth: $max_depth,
            params: [],
            screen_size: $screen,
            window_ids: $ids
        }')

    local output
    output=$(echo "$input_json" | "$CONDUCTOR_BIN")

    local placements ignored
    placements=$(echo "$output" | jq -c '.placements // []')
    ignored=$(echo "$output" | jq -c '.ignored // []')

    if [[ "$ignored" != "[]" ]] && [[ "$ignored" != "null" ]]; then
        echo "Ignored windows: $ignored" >&2
    fi

    local addr
    for window in $(echo "$placements" | jq -c '.[]' 2>/dev/null); do
        local id x y w h
        id=$(echo "$window" | jq -r '.id')
        x=$(echo "$window" | jq -r '.transform.x')
        y=$(echo "$window" | jq -r '.transform.y')
        w=$(echo "$window" | jq -r '.transform.width')
        h=$(echo "$window" | jq -r '.transform.height')

        addr=$(printf "0x%x" "$id")

        apply_transform "$addr" "$x" "$y" "$w" "$h"
    done
}

main "$@"