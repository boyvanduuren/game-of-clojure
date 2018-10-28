# game-of-clojure

Conway's Game of Life in Clojure. See it play at
[vanduuren.xyz][vanduuren_xyz].

## Overview

Naive implementation of Game of Life written during one of the worst stand-by
duties of my life. Written to replace the front page of
[vanduuren.xyz][vanduuren_xyz] because that was never updated anyway.

## Development

To get an interactive development environment, first you'll want to install
clojure and figwheel (see https://rigsomelight.com/figwheel-main-template/).
Then you can run:

    clojure -A:fig:build

This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    rm -rf target/public

To create a production build run:

	rm -rf target/public
	clojure -A:fig:min


## License

Copyright © 2018 B.C. van Duuren

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

[vanduuren_xyz]: https://vanduuren.xyz
