## Media person

Media server that saves and converts images and videos.

### API

#### POST /uploads

Example

```sh
curl -i -X POST -F img=@/path/to/image.jpg localhost:5050/uploads
```

Responds with HTTP 201 and `Location` header with url of the asset. For
[Fine Uploader](http://fineuploader.com/) support, specify

```http
Accept: text/vnd.fineuploader+plain
```

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

#### POST /compressions

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
    <td>path to the video to compress</td>
    <td>/foo123.mov</td>
  </tr>
  <tr>
    <td>targetFormat</td>
    <td>The target format, duh</td>
    <td>One of "h264", "ogg" (or "jpeg" for screenshot)</td>
  </tr>
</tbody>
</table>

Responds with HTTP 201 and `Location` header with url of the asset.
However the actual compression is added to a job queue and may take
a while. Poll the created resource to know when it is complete.

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

Image conversion relies on ImageMagick's `convert` command, and video
compression relies on `ffmpeg`. You'll need to install them to run
the server locally.

```sh
brew install imagemagick --build-from-source
brew install ffmpeg --with-theora
```

Then the haskelly goodness.

```sh
cabal sandbox init
cabal install -j --reorder-goals --disable-documentation

cabal run
```

Now let's build media person.

```sh
cabal install -j --reorder-goals --disable-documentation --enable-test --extra-include-dirs=/usr/local/include/zookeeper
```
