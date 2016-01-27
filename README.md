# mixxxcsvlist

Mixxx seems decent for DJing dances and whatnot, but the current stable version
doesn't give the total duration of a playlist which can be annoying.

So I wrote this small program to solve two problems:

- Generate pretty set-lists for posting to a dance scene Facebook group after
  a dance.
- Calculate the total duration of the set list.

It uses CSV files generated from Mixxx to do this. To generate such a CSV:

- Right click on a playlist
- Click "Export Playlist"
- Change "Filter" to "Text CSV"
- Save the file somewhere you'll remember

Then run this program on it:

`mixxxcsvlist /path/to/your/playlist.csv`

## Installation

* `git clone git://github.com/relrod/mixxxcsvlist && cd mixxxcsvlist`
* `cabal install`

Ensure that `~/.cabal/bin/` is in your `$PATH`, and you should be good to go.

## License

BSD-2.
