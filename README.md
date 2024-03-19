# PolyCEDGA

This is a program that will take a given input Chekanov-Eliashberg differential graded algebra (DGA) of a Legendrian knot and compute its "polynomial contact homology."

For reference on contact homology, see https://web.math.princeton.edu/~jc9803/materials/drp/ghiggini_legendrian_contact_homology.pdf.

Recall that the linearized contact homology is obtained from the DGA by restricting only to generators of word-length 1. The *polynomial* contact homology, of degree $n$, is when we restrict to generators of word length $\leq n$.

The program is capable of computing polynomial contact homology in any field of characteristic $p \geq 2$ (where $p$ is prime).

## Usage

To compute homology, one needs to first construct a DGA object, which is done using the function ```buildDGA```

The ```buildDGA``` builds a DGA given the modulus, an array of gradings, and an array of pairs of Strings, where the first String is a generator and the second String is the image of the generator under the boundary map of the DGA.

The ```buildDGA``` function parses this information into a data type DGA.

Once one has a DGA ```A```, one can perform ```polyHomologies A k ``` to compute the kth polynomial contact homology.

There are number of preset DGAs. For instance, here's the DGA for the trefoil over the field of characteristic m.

```Haskell
kTrefoil :: Int -> DGA
kTrefoil m = buildDGA m gs ds where
    gs = [1, 1, 0, 0, 0]
    ds =    [
            ("q1", "T + q5 + q3 + q5*q4*q3"),
            ("q2", "1 + (-1)*q5 + (-1)*q3 + (-1)*q3*q4*q5"),
            ("q3", "0"),
            ("q4", "0"),
            ("q5", "0")
            ]
```
