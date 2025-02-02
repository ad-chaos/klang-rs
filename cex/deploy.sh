CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER=/opt/homebrew/opt/x86_64-linux-gnu-binutils/bin/x86_64-linux-gnu-ld \
    cargo build --release --target x86_64-unknown-linux-musl

echo "Deploying ..."

scp ./target/x86_64-unknown-linux-musl/release/cex khazad-dum:~

ssh khazad-dum bash <<EOF
    kill -9 $(pgrep cex) >/dev/null 2>&1
    ./cex >/dev/null 2>&1 &
EOF

echo "Done!"
