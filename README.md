# House

## Description

An application for automatically controlling the lighting
in a flat that is kitted out with Philips Hue lightbulbs.

It makes some simplifying assumptions:
- there should be a [Hue motion sensor](https://www.usa.philips.com/c-p/046677464608/hue-motion-sensor) in every room
- the motion sensor should have the same name as the room it is in

Also, unless you like to get up at 6:00 and go to bed at 22:00 you should
alter what is considered night in the `isAtNight` function in `src/main.hs`.
In the same file also tweak what rooms are next to which ones in the
`neighbouringRooms` functions.

Once that is done you should be "good to go".

## Building

The application is written in Haskell. You should have [stack
installed](https://docs.haskellstack.org/en/stable/README/) in order to build and run it.

- `stack build` builds the application
- `stack exec house <hue username>` runs the application locally

I am running my application on a small [chip](https://getchip.com/pages/chip)
which has an arm7 processor. Arm executables are generated in a docker image.
The first build takes a brutally long time. Incremental builds aren't fast,
but fast enough that I can live with it. To build the arm executable you:

- need to have docker installed and running on your machine
- create the build environment with `make build-container`
- create the arm executable with `make arm-executable` (this is the only command you will need once you have run the
  `make build-container` once.

## Running

__NOTE: This system is NOT designed to be run on a server that is available
from the internet!__

You can configure systemd on your home server to run the house light control
system as follows:

Create the equivalent of the following `house.service` unit file
at `/etc/systemd/system`:

```
[Unit]
Description=House light control
Documentation=https://github.com/sebastian/House
After=network.target
Wants=network.target

[Service]
Type=simple
ExecStart=/home/chip/house <password>
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

Then run:

```
systemctl enable house
systemctl start house
```

## Utilities

The system serves up two web interfaces:

- monitoring endpoint at port `8000`
- and a _super simple_ interface showing some more system state at port `8023`

To build a new executable and upload it to a chip in it's default configuration, you can use `make update`.
