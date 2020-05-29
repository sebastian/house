#! /usr/bin/env bash

set -e

echo "Creating production release for RaspberryPi"
dotnet publish -r linux-arm -o build -c Release src/Web/Web.fsproj

echo "Pushing changes to RaspberryPi"
scp -r build/* pi@10.0.0.150:/home/pi/house/

echo "Restarting service"
ssh pi@10.0.0.150 sudo systemctl restart house
