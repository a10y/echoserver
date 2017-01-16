# echoserver

Implementation of simple echo service, see [RFC 862](https://tools.ietf.org/html/rfc862).

Building
--------

```bash
stack setup && stack build
```


Usage
-----

You can optionally provide a port to listen on, otherwise the server will starup on a random port:

```bash
stack exec -- echoserver # chooses random port
stack exec -- echoserver 8099 # starts on 8099
```
