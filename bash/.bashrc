if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

export HISTSIZE=10000
export HISTFILESIZE=-1
export HISTTIMEFORMAT="[%F %T] "
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

export GPG_TTY="$(tty)"
export EDITOR="emacsclient -t"
export LANG=en_US.UTF-8
export SSH_AUTH_SOCK="/run/user/1000/gnupg/S.gpg-agent.ssh"
export XDG_PICTURES_DIR="/home/jake/Pictures"
export PATH="${PATH}:${HOME}/.local/bin/"
export ANSIBLE_NOCOWS=1

export DOCKER_BUILDKIT=1
export PYTHONDONTWRITEBYTECODE=1

source ~/.bash_alias

try_calendar

stty -ixon

if [[ -d "${HOME}/.dotfiles/.git" ]] && [[ -n $(git -C ~/.dotfiles status -s) ]]; then
	RED='\033[0;31m'
	GREEN='\033[0;32m'
	NC='\033[0m'
	BOLD=$(tput bold)
	NORM=$(tput sgr0)
	echo -e "[${RED}!!!${NC}] dotfiles need a commit or cleanup"
fi

# if tmux is running and we're not currently in a tmux session
if tmux ls >/dev/null 2>&1 && [[ -z "${TMUX}" ]]; then
	tmux attach
fi
