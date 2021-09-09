# convex-hull-cljs

Finds the convex hull of a list of points and calculates the area.  
(Like [convex-hull [java]](https://github.com/t-gebauer/convex-hull) but in Clojure)

## Usage

1. Go to https://t-gebauer.github.io/convex-hull-cljs
2. Load another set of points
    - Click on the window to load a new random file from examples folder
    - Or drag and drop a text file onto the application window

Files should contain a list of points separated by white space, flattened.

```
4 0
1 1
1 2
3 3
3 1
0 4
```

Suitable files can be found [here](resources/public/examples/).

## Development

This project requires

  - Clojure >= 1.8.0
  - Leiningen >= 2.8.0

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.

## License

Copyright © 2017 T.Gebauer

Distributed under the MIT License.
