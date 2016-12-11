# House

An Elixir application controlling the lights in my house.
It is as of yet very much a work in progress, and not something
worth trying by anyone but myself.

It is run on a [chip](https://getchip.com/pages/chip) which is more than
capable of running an app like this. Unfortunately compiling and making
releases is a bit of a pain. Once I manage to get docker running on it properly,
I will release this image as a docker container instead.

Until then building releases on the chip will have to do.

__NOTE__:
I couldn't get a decent nodejs version installed on my `chip`, so all the
assets are statically compiled and included in this repo. Not elegant, but it works.

## Gotchas

### Port 80

To enable the app to listen to port `80`, you currently have to run as `root`...
God save us all...

### Configuring

Create a secrets file for your installation, and adapt the values:

```
cp config/secret.exs-sample config/secret.exs
vim config/secret.exs
```

As root, enable the service file (notice symbolic links won't work):

```
su - root
cd /etc/systemd/system
ln /path/to/house/folder/house.service house.service
systemctl enable house
```

### Running

Do a `make release` to generate a release.
Then a `systemctl start house` starts the app.

You can use `systemctl status house` for status, and `systemctl stop house` to stop it.

If you have changed the service file, run `systemctl daemon-reload` to reload it.
