-------------------------------------------------------------------------------
--  Copyright (c) 2020 Daniel King
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.
-------------------------------------------------------------------------------

generic
   type Byte is mod <>;
   type Index is range <>;
   type Byte_Count is range <>;
   type Byte_Array is array (Index range <>) of Byte;
package Generic_COBS
with Pure, SPARK_Mode => On
is
   pragma Compile_Time_Error (Byte'First /= 0,
                              "Byte'First must be 0");

   pragma Compile_Time_Error (Byte'Last <= 1,
                              "Byte'Last must be greater than 1");

   pragma Compile_Time_Error (Byte_Count'First /= 0,
                              "Byte_Count'First must be 0");

   pragma Compile_Time_Error
     (Byte_Count'Pos (Byte_Count'Last) /= Index'Pos (Index'Last),
      "Byte_Count'Last must be equal to Index'Last");

   subtype Positive_Byte_Count is Byte_Count range 1 .. Byte_Count'Last;

   Frame_Delimiter : constant Byte := 0;
   --  COBS uses 0 as the frame delimiter byte.

   procedure Decode (Input  :     Byte_Array;
                     Output : out Byte_Array;
                     Length : out Byte_Count)
     with Global => null,
     Relaxed_Initialization => Output,
     Pre => (
             --  The bounds of the input arrays must not be large enough to
             --  cause a Constraint_Error when reading the 'Length attribute.
             Array_Length_Within_Bounds (Input'First, Input'Last)
             and then Array_Length_Within_Bounds (Output'First, Output'Last)

             --  Cannot decode an empty Input array.
             and then Input'Length > 0

             --  The Output array must be large enough to store all of the
             --  decoded data.
             and then Output'Length >= Input'Length),

     Post => (
              --  The decoded length does not exceed the length of
              --  either array parameter.
              Length <= Output'Length
              and then Length <= Input'Length

              --  Only the first 'Length' bytes of the Output are initialized.
              and then
                (for all I in 0 .. Length - 1 =>
                     Output (Output'First + Index'Base (I))'Initialized)),
     Annotate => (GNATProve, Terminating);
   --  Decodes a COBS-encoded byte array.
   --
   --  @param Input The COBS encoded bytes to be decoded. This may or may not
   --               contain a frame delimiter (zero) byte. If a frame delimiter
   --               is present then this subprogram decodes bytes up to the
   --               frame delimiter. Otherwise, if no frame delimiter byte is
   --               present then the entire input array is decoded.
   --
   --  @param Output The decoded bytes are written to the first "Length" bytes
   --                of this array.
   --
   --  @param Length The length of the decoded frame is written here.

   procedure Encode (Input  :     Byte_Array;
                     Output : out Byte_Array;
                     Length : out Byte_Count)
     with Global => null,
     Relaxed_Initialization => Output,
     Pre => (
             --  The bounds of the input arrays must not be large enough to
             --  cause a Constraint_Error when reading the 'Length attribute.
             Array_Length_Within_Bounds (Input'First, Input'Last)
             and then Array_Length_Within_Bounds (Output'First, Output'Last)

             --  The number of bytes to encode in the Input array must leave
             --  enough headroom for COBS overhead bytes plus frame delimiter.
             and then Input'Length <=
               (Positive_Byte_Count'Last
                - (Max_Overhead_Bytes (Positive_Byte_Count'Last) + 1))

             --  Output array must be large enough to encode the Input array
             --  and the additional overhead bytes.
             and then Output'Length >=
               Input'Length + Max_Overhead_Bytes (Input'Length) + 1),

     Post => (
              --  The length of the output is always bigger than the input
              (Length in Input'Length + 1 .. Output'Length)

              --  Only the first 'Length' bytes of the Output are initialized.
              and then
                Output (Output'First ..
                        Output'First + Index'Base (Length - 1))'Initialized

              --  The last byte in the output is a frame delimiter.
              --  All other bytes before the frame delimiter are non-zero.
              and then
                (for all I in Output'First ..
                              Output'First + Index'Base (Length - 1) =>
                   (if I < Output'First + Index'Base (Length - 1)
                    then Output (I) /= Frame_Delimiter
                    else Output (I) = Frame_Delimiter))),
     Annotate => (GNATProve, Terminating);
   --  Encode a byte array.
   --
   --  The contents of the "Input" array are encoded and written
   --  to the "Output" array, followed by a single frame delimiter. The length
   --  of the complete frame (including the frame delimiter) is output as the
   --  "Length" parameter.
   --
   --  @param Input The bytes to encode.
   --
   --  @param Output The COBS-encoded data is written to the first "Length"
   --                bytes of this array. The last byte is a frame delimiter.
   --
   --  @param Length The length of the encoded frame is written here.

   function Max_Overhead_Bytes (Input_Length : Byte_Count)
                                return Positive_Byte_Count
     with Global => null;
   --  Return the maximum number of overhead bytes that are inserted into
   --  the output during COBS encoding for a given input length.

   function Array_Length_Within_Bounds (First, Last : Index'Base)
                                        return Boolean is
     (Last < First
      or else First > 0
      or else Last < First + Index (Byte_Count'Last));
   --  Check that the length of the given range does not exceed Byte_Count'Last
   --
   --  This check is equivalent to: (Last - First) + 1 <= Byte_Count'Last
   --
   --  The purpose of this check is to ensure that we can take the 'Length
   --  of a Byte_Array without causing an integer overflow. Such an overflow
   --  could happen if the Index type is a signed integer. For example,
   --  consider the following concrete types:
   --
   --     type Index is range -10 .. +10;
   --     subtype Byte_Count is Index range 0 .. Index'Last;
   --
   --  With these types it is possible to construct a Byte_Array whose length
   --  is larger than Byte_Count'Last:
   --
   --     Buffer : Byte_Array (-10 .. +10);
   --
   --  The length of this array is 21 bytes, which does not fit in Byte_Count.
   --
   --  This check constrains the range of the Byte_Array object's range such
   --  that the maximum length does not exceed 10. For example:
   --     Buffer_1 : Byte_Array (-10 .. 10); --  Not OK, 'Length = 21
   --     Buffer_2 : Byte_Array (  1 .. 10); --      OK, 'Length = 10
   --     Buffer_3 : Byte_Array (  0 .. 10); --  Not OK, 'Length = 11
   --     BUffer_4 : Byte_Array (  0 ..  9); --      OK, 'Length = 10
   --     Buffer_5 : Byte_Array ( -5 .. -1); --      OK, 'Length = 5

private

   Maximum_Run_Length : constant Byte_Count := Byte_Count (Byte'Last - 1);
   --  Maximum run of non-zero bytes before an overhead byte is added.
   --
   --  COBS specifies that data is grouped into either 254 non-zero bytes,
   --  or 0-253 non-zero bytes followed by a zero byte. The 254 byte case
   --  allows for the trailing zero byte to be omitted, but for simplicity
   --  this case is not implemented in our encoder (but is supported by
   --  the decoder).

   function Max_Overhead_Bytes (Input_Length : Byte_Count)
                                return Positive_Byte_Count is
     ((Input_Length / Maximum_Run_Length) + 1);

   procedure Encode_Block (Input  :     Byte_Array;
                           Output : out Byte_Array;
                           Length : out Byte_Count)
     with Inline,
     Global => null,
     Relaxed_Initialization => Output,
     Pre => (Array_Length_Within_Bounds (Input'First, Input'Last)
             and then Array_Length_Within_Bounds (Output'First, Output'Last)
             and then Output'Length > Input'Length),
     Post => (Length <= Input'Length + 1
              and then (if Length < Input'Length + 1
                        then Length >= Maximum_Run_Length + 1)
              and then
                Output (Output'First ..
                        Output'First + Index'Base (Length - 1))'Initialized
              and then
                (for all I in Output'First ..
                              Output'First + Index'Base (Length - 1) =>
                    Output (I) /= Frame_Delimiter)),
     Annotate => (GNATProve, Terminating);
   --  Encodes a single block of bytes.
   --
   --  This prepends one overhead byte, then encodes as many bytes as possible
   --  until another overhead byte is needed. Another overhead byte is needed
   --  when another full run of Maximum_Run_Length non-zero bytes is reached.

end Generic_COBS;
