cabal clean
cp -r . tmp/
rm -rf tmp/prepare-artifact.sh
rm -rf tmp/.git
rm -rf tmp/.gitignore
rm -rf tmp/experiments/.env
mkdir -p mocheqos-artifact/code
mv tmp/* mocheqos-artifact/code/
rm -r tmp/
cp README mocheqos-artifact/
cp LICENSE mocheqos-artifact/
mv mocheqos-artifact/code/mocheqos-docker-image.tar mocheqos-artifact/
zip -r mocheqos-artifact.zip mocheqos-artifact/* -x '**/.DS_Store'
rm -rf mocheqos-artifact
