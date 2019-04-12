source $HOME/.profile

# Aliases, obviously
source ~/.aliases

# Command prompt stuff
git_branch() {
   ref=$(git symbolic-ref HEAD 2> /dev/null | cut -d'/' -f3)
   echo $ref
}

setopt prompt_subst
export PS1='[%n:%/] ($(git_branch)) >> '

export PATH=$PATH:/Users/jack/src/inscriptive/vendor/bucktool/bin
export PATH=$PATH:/usr/local/appengine/appengine-java-sdk-1.9.54/bin/
export PATH=$PATH:/Users/jack/Library/Python/2.7/bin
export PATH=$PATH:/Users/jack/src/flutter/bin
export PATH=$PATH:/Applications/MATLAB_R2018a.app/bin
export PATH=$PATH:/Users/jack/bin

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/jack/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/jack/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/jack/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/jack/google-cloud-sdk/completion.zsh.inc'; fi

export AF_PATH=/usr/local
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$AF_PATH/lib

