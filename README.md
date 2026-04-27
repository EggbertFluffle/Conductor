# Conductor

Conductor is a DSL designed to unify how we describe dynamic window management. It is intended to be platform agnostic meaning that, as long as a desktop platform implements and interface to Conductor, one snippet of Conductor could work in any Wayland compositor just as it would on any X11 window manager.

## Supported platforms

- [Mezzaluna](https://github.com/MezzalunaWM/Mezzaluna) -> [conductor.mez](https://github.com/EggbertFluffle/conductor.dwl)
- [dwl](https://codeberg.org/dwl/dwl) -> [conductor.dwl](https://github.com/EggbertFluffle/conductor.dwl)

## Installation

```sh
git clone https://github.com/EggbertFluffle/Conductor
cd ./Conductor
cabal build
```

## Building Platform Support

Learning how to interface with Conductor is as easy as knowing how to call external processes and parse JSON using your platform of choice's extension or source language. First, the JSON schemes for interacting with Conductor. The binary expects a JSON object containing a list of window ids, the screen size, a list of parameters, and a string representation of the Conductor snippet. A simple example lies below.

```JSON 
{
    "starting_variable": "start",
    "snippet": "start = full [|, param] stack\\nstack = full (-) stack\\n",
    "max_depth": 25,
    "params": [ 0.5 ],
    "screen_size": {
        "y": 0,
        "x": 0,
        "height": 515,
        "width": 848
    },
    "window_ids": [ 928221424, 928397360, 928134512 ]
}
```

This should be passed to conductor via `stdin`, after which the binary will parse and arrange the windows, returning their transformations in another JSON object to `stdout`. This is when it is up to the interface to read the returned JSON and properly represent it in the desktop stack of choice. The return JSON contains a simple list of each window id with its associated rectangular transform.

```json
{
    "ignored": [],
    "placements": [
        {
            "id": 928221424,
            "transform": {
                "height": 515,
                "width": 424,
                "x": 0,
                "y": 0
            }
        },
        {
            "id": 928397360,
            "transform": {
                "height": 257,
                "width": 424,
                "x": 424,
                "y": 0
            }
        },
        {
            "id": 928134512,
            "transform": {
                "height": 258,
                "width": 424,
                "x": 424,
                "y": 257
            }
        }
    ]
}
```
