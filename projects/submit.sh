set -e

EMAIL=$1
TOKEN=$2

function fail_usage
{
    echo "USAGE: $0 <email> <token>"
    exit 1
}

if [ $# -eq 0 ]; then
    fail_usage
fi

sbt compile test scalafix "submit $EMAIL $TOKEN"
