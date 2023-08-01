#!/usr/bin/env bash

for d in $(find . -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | xargs); do
	[[ "${d}" == ".git" ]] && continue;
	echo "Stowing ${d}"
	stow -R "${d}" -t "${HOME}" || break;
done
