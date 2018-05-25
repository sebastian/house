# House

A haskell application for controlling the lights in my house.
Very much a work in progress (and in fact the first Haskell
program I have ever written... expect some rough edges).

This version replaces a previous version written in Elixir.

## Running

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
