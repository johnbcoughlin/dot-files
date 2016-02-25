# Aliases, obviously
source ~/.aliases

# Command prompt stuff
git_branch() {
   ref=$(git symbolic-ref HEAD 2> /dev/null | cut -d'/' -f3)
   echo $ref
}

setopt prompt_subst
export PS1='[%n:%/] ($(git_branch)) >> '

