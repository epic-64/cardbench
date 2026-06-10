#!/bin/bash
# Run all tests: JVM (bloop) and JS (sbt with fullOptJs).
set -e

echo "Running JVM tests with bloop…"
bloop test jvm

echo "Running JS tests with sbt…"
sbt -J-Xmx4G 'project js' 'set Test/scalaJSStage := FullOptStage' 'test'

echo "All tests passed!"
