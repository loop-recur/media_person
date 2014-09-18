## Media person

Haskell media server that crops

### Building on os x

Although this project uses sqlite to hold its job queue,
the haskell library involved requires the zookeeper C bindings
to build. So we'll install those first.

Download the [zookeeper source](http://www.motorlogy.com/apache/zookeeper/stable/zookeeper-3.4.6.tar.gz) and untar it.

```sh
cd zookeeper-3.4.6/src/c
./configure
make && make install
```

Now clone the media person repo. The job queue haskell package
is a hackage candidate at the time of this writing so it must be
pulled as a git submodule. Do the following

```sh
git submodule update --recursive
```

Now let's build media person.

```sh
cabal install -j --reorder-goals --disable-documentation --enable-test --extra-include-dirs=/usr/local/include/zookeeper
```
