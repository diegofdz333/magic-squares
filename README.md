# magic-squares

This is a Haskell Project for Parallel Functional Programming. It will count the total number of normal magic squares for a square of size n.

# Install Stack

You will need to be able to run `stack` to run this program. Install it through GHCup in the link here. Follow listed instructions. https://www.haskell.org/ghcup/ 

# Running Program

## Build

To build run `stack build`

## Running Code

To run sequentially run `stack run <mode> <n>`.
To run in parallel run `stack run <mode> <n> -- +RTS -N<threads>`.
For statistics add an `-s` tag.
For logging ad an `-l` tag.

`<n>` must be a positive integer. Anything greater than 4 will take over a day.
`<mode>` can be:
- `seq1` - Original Sequential Algorithm
- `seq2` - Sequential Algorithm with early Reflection / Rotation prunning
- `par1` - Same as `seq1` but parallelized.
- `par2` - More graunular, recurse over each individual element rather than doing chunks of rows and columns at a time.

### Examples

`stack run "par1" 4 -- +RTS -N8`

`stack run "seq2" 3`

### Expected Output

`n = 1 -> 1`
`n = 2 -> 0`
`n = 3 -> 8`
`n = 4 -> 7040`