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
- username=<HUE-PASSWORD> port=5000 dotnet run --project src/Web/
```

Should be all that is required.

It expects that you have dotnet core version `3.1.200` or newer.

## Preparing the "server"

I deploy the system on a RaspberryPi 3b. The machine is actually
overkill for the purpose, but that is beside the point. Anything
similar that you have available should do well. 

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
ExecStart=/home/pi/house/Web
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

You subsequently need to configure your username and the port you the 
service to make its web interface available on (default is 5000).

You do this by running `systemctl edit house` on the host that will run
the system. This will create a configuration overlay that you can ammend
to make look something like this:

```
[Service]
Environment="username=<HUE USERNAME>"
Environment="PORT=80"
```

Then run:

```
systemctl enable house
systemctl start house
```

## Deploying

I have created a utility script called [publish-to-pi.sh](publish-to-pi.sh).
You will have to tweak it to suit your needs, but it will do the following:

- build a version of this app targeting an ARM linux box
- assume you are running your RaspberryPi at port 10.0.0.150 and upload
  the build artifact to to `/home/pi/house`
- restart the service

# History

I believe this is the third incarnation of the system that I have built.
I use it as a way to play with different languages.

If you are interested, you can dig through the history to find a version
written in Elixir, and one in Haskell too.

The current F# one is a simplification of the previous versions. I was trying
to be too smart (which made for some rather odd light schedules).
The current F# implementation tries to do less.
