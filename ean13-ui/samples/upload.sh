set -x

aws s3 mb s3://ean13-samples
ls * | xargs -I{} -n 1 -P 10 aws s3 cp {} s3://ean13-samples/{}
ls * | xargs -I{} -n 1 -P 10 aws s3api put-object-acl --bucket ean13-samples --key {} --acl public-read
aws s3api put-bucket-cors --bucket ean13-samples --cors-configuration file://cors.json
