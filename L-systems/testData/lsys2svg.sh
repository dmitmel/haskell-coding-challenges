#!/usr/bin/env bash

output_dir=./svg

if [ ! -d "$output_dir" ]; then
    echo "Creating output directory"
    mkdir "$output_dir"
fi

while read -r -d '' file; do
    echo "Rendering $file"
    output="$output_dir"/"$(basename "$file" | cut -d'.' -f1)".svg
    L-systems svg "$file" 1000x1000 -o "$output" "$@"
done < <(find . -maxdepth 1 -type f -iname '*.json' -print0)
