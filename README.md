## Media person

Haskell media server that crops

### API

#### POST /uploads

Example

```sh
curl -i -X POST -F img=@/path/to/image.jpg localhost:5050/uploads
```

Responds with HTTP 201 and `Location` header with url of the asset.

#### POST /conversions

Use post format `x-www-form-urlencoded` with parameters:

<table>
<thead>
  <tr>
    <th>Param</th>
    <th>Meaning</th>
    <th>Examples</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>url</td>
    <td>path to the image to convert</td>
    <td>/foo123.png</td>
  </tr>
  <tr>
    <td>command</td>
    <td>comma-separated command line flags to imagemagick convert</td>
    <td>-resize,10x10</td>
  </tr>
</tbody>
</table>

Responds with HTTP 201 and `Location` header with url of the asset.

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
git submodule update --init
cabal sandbox add-source haskell-jobqueue/jobqueue
```

Now let's build media person.

```sh
cabal install -j --reorder-goals --disable-documentation --enable-test --extra-include-dirs=/usr/local/include/zookeeper
```
