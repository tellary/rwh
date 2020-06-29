set -x

nix-build
aws s3 mb s3://ean13
pushd result/bin/findean13-ui.jsexe/
ls * | xargs -I{} -n 1 -P 10 aws s3 cp {} s3://ean13/{}
ls * | xargs -I{} -n 1 -P 10 aws s3api put-object-acl --bucket ean13 --key {} --acl public-read
popd
