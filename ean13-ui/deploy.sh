set -x

nix-build
BUCKET=ean13
aws s3 mb s3://$BUCKET
pushd result/bin/findean13-ui.jsexe/
ls * | xargs -I{} -n 1 -P 10 aws s3 cp {} s3://$BUCKET/{} --metadata-directive REPLACE --cache-control max-age=60
ls * | xargs -I{} -n 1 -P 10 aws s3api put-object-acl --bucket $BUCKET --key {} --acl public-read
popd
