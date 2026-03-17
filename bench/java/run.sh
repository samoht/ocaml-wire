#!/usr/bin/env bash
# Build and run the CLCW polling benchmark.
#
# Usage:
#   ./run.sh                     # hand-written Java only
#   ./run.sh path/to/tmtc.jar    # also run dariol83/ccsds benchmarks
#
# The dariol83/ccsds jar can be obtained from Maven Central:
#   groupId    : eu.dariolucia.ccsds
#   artifactId : eu.dariolucia.ccsds.tmtc
#   version    : 1.0.0  (or latest)
#
# Quick download:
#   mvn dependency:copy -Dartifact=eu.dariolucia.ccsds:eu.dariolucia.ccsds.tmtc:1.0.0 \
#       -DoutputDirectory=.
#
# Or grab the jar directly:
#   curl -LO https://repo1.maven.org/maven2/eu/dariolucia/ccsds/eu.dariolucia.ccsds.tmtc/1.0.0/eu.dariolucia.ccsds.tmtc-1.0.0.jar

set -euo pipefail
cd "$(dirname "$0")"

JAR="${1:-}"

if [ -n "$JAR" ]; then
    echo "==> Compiling with dariol83/ccsds: $JAR"
    javac -cp "$JAR" BenchClcw.java
    echo "==> Running"
    java -cp ".:$JAR" -server -XX:+UseCompressedOops BenchClcw
else
    echo "==> Compiling (hand-written baseline only)"
    javac BenchClcw.java
    echo "==> Running"
    java -cp . -server -XX:+UseCompressedOops BenchClcw
fi
