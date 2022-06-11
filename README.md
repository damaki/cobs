[![codecov](https://codecov.io/gh/damaki/cobs/branch/master/graph/badge.svg?token=3843CRXWFH)](https://codecov.io/gh/damaki/cobs)

# Consistent Overhead Byte Stuffing (COBS)
This repository provides an implementation of a COBS[1] encoder and
decoder using the SPARK programming language.

## License
All files are licensed under the MIT license.

## SPARK
The code adopts the silver level of SPARK adoption[2]. At the silver
level the code is formally verified by the SPARK toolset to have
the following properties:
* no reads of uninitialized variables
* no unintended accesses of global data
* absence of run-time errors (AoRTE):
  * no integer overflow
  * no type range violations
  * no divisions by zero
  * no out-of-bounds array accesses
* all loops are guaranteed to terminate

## Examples
COBS frames can be encoded and decoded via two subprograms:
`COBS.Encode` and `COBS.Decode`. Here's an example to encode
a byte array using COBS:

```Ada
with Ada.Storage_Elements; use Ada.Storage_Elements;
with COBS;

procedure Example
is
   Unencoded : Storage_Array (1 .. 5) := (1, 2, 3, 0, 4);
   Encoded   : Storage_Array (1 .. 7);
   Length    : Storage_Count;
begin
   COBS.Encode (Unencoded, Encoded, Length);

   --  Encoded = (4, 1, 2, 3, 2, 4, 0);
   --  Length  = 7
end Example;
```

The `COBS` package uses Ada's `System.Storage_Elements.Storage_Array`
for its byte array buffers. If custom byte array types are needed, then
the `Generic_COBS` package can be used to instantiate the encoder/decoder
for custom user-defined types.

```Ada
with Interfaces;   use Interfaces;
with Generic_COBS;

package Example
is
   type Byte_Array is array (Positive range <>) of Unsigned_8;

   type My_COBS is new Generic_COBS (Byte       => Unsigned_8,
                                     Index      => Positive,
                                     Byte_Count => Natural,
                                     Byte_Array => Byte_Array);
end Example;
```

## Verification

This project takes a "hybrid verification" approach combining formal
verification and testing.

GNATprove is used to prove absence of run-time errors and some limited
functional properties:
 * the encoder only emits a frame delimiter (zero byte) at the end
   of the encoded frame.

At the time of writing there are 163 checks that are automatically proved.

The unit tests are used to check that the encoder and decoder produce the
correct results for a variety of inputs, with 100% MC/DC source coverage.

### Reproducing the results

#### Proof

To run the proofs:

```sh
cd prove
alr exec -- gnatprove -P ../cobs
```

If you want to see only failed checks, then pass `--report=fail` to `gnatprove`.

#### Tests

To run the unit tests:
```sh
alr build --verification
cd tests
alr build --verification
alr run
```

To run the tests and generate a code coverage report:
```sh
alr build --verification
cd tests
./coverage.sh
```

The report will be generated in `tests/gnatcov_out/index.html`.

## References

[1] Consistent Overheady Byte Stuffing
    https://en.wikipedia.org/wiki/Consistent_Overhead_Byte_Stuffing

[2] Implementation Guidance for the Adoption of SPARK, Adacore and Thales, 2018/09/07
    https://www.adacore.com/books/implementation-guidance-spark