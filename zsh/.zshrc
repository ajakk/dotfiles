# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
#zstyle :compinstall filename '/home/jake/.zshrc'

autoload zmv
autoload -Uz compinit
compinit
# End of lines configured by compinstall
# The following lines were added by jake

if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

# Start X on tty1 if logged in on tty1
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	exec xinit -- vt01;
fi

[[ -n "$XTERM_VERSION" ]] && [[ $TERM == "xterm-256color" ]] && transset --id "$WINDOWID" >/dev/null

export GPG_TTY=$(tty)
export PATH=$PATH:~/.local/bin
export MANPATH=$MANPATH:~/.local/share/man
export XDG_DATA_DIRS=$XDG_DATA_DIRS:~/.local/share
export EDITOR=vim
export LANG=en_US.UTF-8
export SSH_AUTH_SOCK="/run/user/1000/gnupg/S.gpg-agent.ssh"

export PYTHONDONTWRITEBYTECODE=1

source ~/.zsh_alias

try_calendar

if [[ -n $(git -C ~/.dotfiles status -s) ]]; then
	RED='\033[0;31m'
	GREEN='\033[0;32m'
	NC='\033[0m'
	BOLD=$(tput bold)
	NORM=$(tput sgr0)
	echo -e "[${RED}!!!${NC}] dotfiles need a commit or cleanup"
fi
