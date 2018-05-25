echo
echo "## Creating the executable"
make arm-executable

echo
echo "## Stopping service"
ssh chip@chip.local 'echo chip | sudo -S systemctl stop house'

echo
echo "## Copying the executable to the chip"
scp .stack-work/dist/arm-linux/Cabal-2.0.1.0/build/house/house chip@chip.local:/home/chip/

echo
echo "## Restarting the service"
ssh chip@chip.local 'echo "chip" | sudo -S systemctl start house'
