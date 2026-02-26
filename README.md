# calc

A monolithic TUI & CLI calculator written in C, featuring scientific functions, unit and base conversions, matrix operations, and a built-in graph plotter.

## Features
* **CLI Mode (`-c`)**: Evaluate math expressions or perform rapid conversions directly from the terminal.
* **TUI Mode**: A curses-based interface with 7 distinct modes:
  * Calculator & Scientific Math
  * Unit Converter (Length, Weight, Temp, Volume, Speed, Data, Time)
  * Base Converter (DEC, HEX, OCT, BIN)
  * Graph Plotter (with pan and zoom)
  * Matrix Operations (Determinant, Transpose, Inverse, Add, Multiply)
  * GCD / LCM (PGCD / PPCM) Calculator

## Dependencies
* A standard C compiler (`cc`, `gcc`, `clang`)
* `make`
* `ncurses` (specifically `ncursesw` for wide-character support on Linux)

## Installation
Build the binary:
```bash
make

```

Install locally (defaults to `~/.local/bin`):

```bash
make install

```

Install system-wide:

```bash
sudo make install PREFIX=/usr/local

```

## Usage

Launch the TUI by running it without arguments:

```bash
calculator

```

Use the CLI for quick calculations:

```bash
calculator -c "sin(45) + sqrt(2^3+1)"
calculator -c convert 100 -tempc -tempf
calculator -c base 255 dec hex

```

