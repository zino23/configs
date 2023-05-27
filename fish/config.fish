# Base16 Shell
if status --is-interactive
    set BASE16_SHELL "$HOME/.config/base16-shell/"
    source "$BASE16_SHELL/profile_helper.fish"
end

if status is-interactive
    # Commands to run in interactive sessions can go here
end

set -gx LANG en_US.UTF-8

# Homebrew
set -gx HOMEBREW_NO_AUTO_UPDATE 1
set -gx HOMEBREW_AUTO_UPDATING 0
set -gx HOMEBREW_UPDATE_PREINSTALL 0 

# for pkg-config to find libxml2 
set -gx PKG_CONFIG_PATH "/usr/local/opt/libxml2/lib/pkgconfig"

set -gx EDITOR emacsclient

# abbreviations
abbr -aU ec emacsclient -n

abbr -aU rs rsync -chavzP --stats

abbr -aU gh git help
abbr -aU gc git checkout
abbr -aU gcb git checkout -b
abbr -aU gs git status

abbr -aU gr grep -n

# alias
alias gm="~/gitlab/edgeworker/v8_build/v8/tools/dev/gm.py"
alias v8gen="~/gitlab/edgeworker/v8_build/v8/tools/dev/v8gen.py"

# set -gx MANPAGER "sh -c 'col -bx | bat -l man -p'"
# set -gx BAT_THEME base16-256

set -gx RUST_BACKTRACE 1

set -gx GOOS darwin
set -gx GOPROXY "https://goproxy.cn,direct"
set -gx GO111MODULE on
set -gx GOPATH ~/go

# colored man pages
# Solarized Dark & Green highlight
# set -eg man_blink -o red
# set -eg man_bold -o green
# set -eg man_standout -b black 93a1a1
# set -eg man_underline -u 93a1a1

# Source: http://unix.stackexchange.com/a/147
# More info: http://unix.stackexchange.com/a/108840
setenv LESS_TERMCAP_mb $(tput bold; tput setaf 2) # green
setenv LESS_TERMCAP_md $(tput bold; tput setaf 6) # cyan
setenv LESS_TERMCAP_me $(tput sgr0)
setenv LESS_TERMCAP_so $(tput bold; tput setaf 3; tput setab 4) # yellow on blue
setenv LESS_TERMCAP_se $(tput rmso; tput sgr0)
setenv LESS_TERMCAP_us $(tput smul; tput bold; tput setaf 7) # white
setenv LESS_TERMCAP_ue $(tput rmul; tput sgr0)
setenv LESS_TERMCAP_mr $(tput rev)
setenv LESS_TERMCAP_mh $(tput dim)
setenv LESS_TERMCAP_ZN $(tput ssubm)
setenv LESS_TERMCAP_ZV $(tput rsubm)
setenv LESS_TERMCAP_ZO $(tput ssupm)
setenv LESS_TERMCAP_ZW $(tput rsupm)

set -Ux PYENV_ROOT $HOME/.pyenv
set -U fish_user_paths $PYENV_ROOT/bin $fish_user_paths
pyenv init - | source

# For compilers to find libffi you may need to set:
set -gx LDFLAGS "-L/usr/local/opt/libffi/lib"
set -gx CPPFLAGS "-I/usr/local/opt/libffi/include"

# For pkg-config to find libffi you may need to set:
set -gx PKG_CONFIG_PATH "/usr/local/opt/libffi/lib/pkgconfig"

## let's fish
function display_fish_user_paths -d 'Display contents $fish_user_paths with indexes'
    echo $fish_user_paths | tr " " "\n" | nl
end

## remove fish $PATH
function erase_fish_user_paths -a idx
    set --erase --universal fish_user_paths[$idx]
    display_fish_user_paths
end

function test_args -a a1 a2 a3 -d "Test arguments"
    echo -e "\
a1: $a1
a2: $a2
a3: $a3"
end

## $fish_user_paths manipulating examples
# > echo $fish_user_paths | tr " " "\n" | nl
#      1	/root/Android/Sdk/platform-tools
#      2	/root/Android/Sdk/tools
#      3	/home/explosic4/.fzf/bin

# > set --erase --universal fish_user_paths[1]
# > echo $fish_user_paths | tr " " "\n" | nl
#      1	/root/Android/Sdk/tools
#      2	/home/explosic4/.fzf/bin

# > set --erase --universal fish_user_paths[1]
# > echo $fish_user_paths | tr " " "\n" | nl
#      1	/home/explosic4/.fzf/bin

# openssl
# For compilers to find openssl@3 you may need to set:
set -gx LDFLAGS "-L/usr/local/opt/openssl@3/lib"
set -gx CPPFLAGS "-I/usr/local/opt/openssl@3/include"

# For pkg-config to find openssl@3 you may need to set:
set -gx PKG_CONFIG_PATH "/usr/local/opt/openssl@3/lib/pkgconfig"

## emacs
set -gx XDG_CONFIG_HOME $HOME/.config

## cmake

## tmux
if test ! -d '~/.tmux/plugins/tpm'
    echo 'start to installing Tmux Plugin Manager tpm...'
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
end
