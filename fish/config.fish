# Base16 Shell
if test ! -f "$HOME/.config/base16-shell/profile_helper.fish"
    echo "Download base16-shell ..."
    git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
end

if status --is-interactive
    set BASE16_SHELL "$HOME/.config/base16-shell"
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

# Abbreviations
abbr ec emacsclient -n
abbr rs rsync -chavzP --stats
abbr gh git help
abbr gc git checkout
abbr gcb git checkout -b
abbr gs git status
abbr gr grep -n

# Use emacs keybindings.
fish_default_key_bindings

# Manual path
set -l os_name (uname -s)
set -l cpu_arch (uname -m)
if test $os_name = "Darwin"
    # homebrew is installed to /opt/homebrew/ for macos running on arm64
    if test $cpu_arch = "arm64"
        set -gax MANPATH /opt/homebrew/share/man
        eval "$(/opt/homebrew/bin/brew shellenv)"
    end
end

# Colored man pages
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

## Let's fish
function display_fish_user_paths -d 'Display contents $fish_user_paths with indexes'
    echo $fish_user_paths | tr " " "\n" | nl
end

## Remove fish $PATH
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

## Emacs
set -gx XDG_CONFIG_HOME $HOME/.config

switch $(uname -s)
    case Darwin
        set -gx EMACS_SOCKET_NAME "$TMPDIR/emacs$(id -u)/server"
        set -gx EDITOR "emacsclient --socket-name $EMACS_SOCKET_NAME"
end

## Tmux
# it is sad that tilde `~` does not expand within quotes
if test ! -d "$HOME/.tmux/plugins/tpm"
    echo 'Start to installing Tmux Plugin Manager tpm ...'
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
end

## VTerm
function vterm_printf
    if begin
            [ -n "$TMUX" ]; and string match -q -r "screen|tmux" "$TERM"
        end
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

set fish_greeting

source $XDG_CONFIG_HOME/fish/env.fish
