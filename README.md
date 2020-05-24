# House

## Description

An application for automatically controlling the lighting
in a flat that is kitted out with Philips Hue lightbulbs.

It makes some simplifying assumptions:
- there should be a [Hue motion sensor](https://www.usa.philips.com/c-p/046677464608/hue-motion-sensor) in every room
- the motion sensor should have the same name as the room it is in

Also, unless you like to get up at 6:00 and go to bed at 22:00 you should
dig into the system and change the settings.

### What it does

It will turn on the lights in any room where it has seen movement, as well
as the neighboring rooms, in case I am moving there.

## Building

It's a fairly standard dotnet core application.

```
dotnet restore
dotnet run
```

Should be all that is required.

It expects that you have dotnet core version `3.1.200` or newer.

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

# History

I believe this is the third incarnation of the system that I have built.
I use it as a way to play with different languages.

If you are interested, you can dig through the history to find a version
written in Elixir, and one in Haskell too.

The current F# one is a simplification of the previous versions. I was trying
to be too smart (which made for some rather odd light schedules).
The current F# implementation tries to do less.
