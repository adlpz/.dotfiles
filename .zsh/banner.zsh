_print_zsh_banner() {
    echo -e `host -t txt istheinternetonfire.com` | cut -f 2 -d '"'| sed "s/[\];\s*/\n/g"
}
