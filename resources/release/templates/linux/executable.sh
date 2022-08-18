#!/bin/bash

SOURCE="${BASH_SOURCE[0]}"

while [ -h "$SOURCE" ]
do
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done

DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

JAR="$DIR/%%FILENAME%%.jar"

echo "Starting Omnia %%VERSION%%.."

java -jar $JAR
