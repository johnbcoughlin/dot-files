# Aliases, obviously
source ~/.aliases

# Command prompt stuff
git_branch() {
   ref=$(git symbolic-ref HEAD 2> /dev/null | cut -d'/' -f3)
   echo $ref
}

setopt prompt_subst
export PS1='[%n:%/] ($(git_branch)) >> '


plugins=(rvm nvm)
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

source ~/.cargo/env


source ~/.zshrc.local

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/jack/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/jack/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/jack/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/jack/google-cloud-sdk/completion.zsh.inc'; fi
