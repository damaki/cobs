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
with AUnit.Assertions;  use AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;

with System.Storage_Elements; use System.Storage_Elements;

with COBS;

package body COBS_Tests
is

   package Test_Caller is new AUnit.Test_Caller (Test);

   -----------------------
   -- Test_Encode_Empty --
   -----------------------

   --  Test the output of Encode when presented with an empty input.
   --  The output should only consist of the overhead byte and frame delimiter.

   procedure Test_Encode_Empty (T : in out Test)
   is
      Input  : constant Storage_Array (1 .. 0) := (others => 0);
      Output :          Storage_Array (1 .. 2);
      Length :          Storage_Count;

      Expected_Output : constant Storage_Array (1 .. 2) :=
        (1 => 1,  --  Overhead byte
         2 => 0); --  Frame delimiter

   begin
      COBS.Encode (Input, Output, Length);

      Assert (Length = 2,                             "Incorrect Length");
      Assert (Output (1 .. Length) = Expected_Output, "Incorrect output");
   end Test_Encode_Empty;

   ----------------------------
   -- Test_Encode_All_Zeroes --
   ----------------------------

   --  Test the output of Encode when presented with an input that consists
   --  of all zeroes.
   --
   --  The output should not contain any additional overhead bytes.

   procedure Test_Encode_All_Zeroes (T : in out Test)
   is
      Input  : constant Storage_Array (1 .. 1000) := (others => 0);
      Output :          Storage_Array (1 .. 2000);
      Length :          Storage_Count;

      Expected_Output : constant Storage_Array (1 .. 1002) :=
        (1      => 1,  --  Overhead byte
         1002   => 0,  --  Frame delimiter
         others => 1); --  Data bytes (encoded)

   begin
      COBS.Encode (Input, Output, Length);

      Assert (Length = 1002,                          "Incorrect Length");
      Assert (Output (1 .. Length) = Expected_Output, "Incorrect output");
   end Test_Encode_All_Zeroes;

   ---------------------------
   -- Test_Encode_No_Zeroes --
   ---------------------------

   --  Test the output of Encode when presented with an input consisting of
   --  no zero bytes.
   --
   --  The output should contain additional overhead bytes
   --  in groups of 254 bytes.

   procedure Test_Encode_No_Zeroes (T : in out Test)
   is
      Input  : constant Storage_Array (1 .. 1000) := (others => 1);
      Output :          Storage_Array (1 .. 2000);
      Length :          Storage_Count;

      Expected_Output : constant Storage_Array (1 .. 1005) :=
        (1      => 255, --  1st overhead byte
         256    => 255, --  2nd overhead byte
         511    => 255, --  3rd overhead byte
         766    => 239, --  4th overhead byte
         1005   => 0,   --  Frame delimiter
         others => 1);  --  Data byte

   begin
      COBS.Encode (Input, Output, Length);

      Assert (Length = 1005,                          "Incorrect Length");
      Assert (Output (1 .. Length) = Expected_Output, "Incorrect output");
   end Test_Encode_No_Zeroes;

   -----------------------------
   -- Test_Decode_Empty_Frame --
   -----------------------------

   --  Test the decoder when presented with an empty frame.
   --
   --  The output should be of length 0.

   procedure Test_Decode_Empty_Frame (T : in out Test)
   is
      Input  : constant Storage_Array (1 .. 1) := (others => 0);
      Output :          Storage_Array (1 .. 1);
      Length :          Storage_Count;

   begin
      COBS.Decode (Input, Output, Length);

      Assert (Length = 0, "Incorrect length");
   end Test_Decode_Empty_Frame;

   ------------------------------------
   -- Test_Decode_No_Frame_Delimiter --
   ------------------------------------

   --  Test the decoder behaviour when the frame delimiter is missing from
   --  the input buffer.
   --
   --  The decoder should continue until the end of the input buffer is reached

   procedure Test_Decode_No_Frame_Delimiter (T : in out Test)
   is
      Input           : constant Storage_Array (1 .. 7) :=
        (6, 1, 2, 3, 4, 5, 5);

      Expected_Output : constant Storage_Array (1 .. 6) :=
        (1, 2, 3, 4, 5, 0);

      Output : Storage_Array (1 .. 10);
      Length : Storage_Count;

   begin
      COBS.Decode (Input, Output, Length);
      Assert (Output (1 .. Length) = Expected_Output, "Failed output");
   end Test_Decode_No_Frame_Delimiter;

   ------------------------------
   -- Test_Decode_Test_Vectors --
   ------------------------------

   --  Test the decoder with some test vectors.

   procedure Test_Decode_Test_Vectors (T : in out Test)
   is
      --  Short case with no replaced bytes.
      Input_1  : constant Storage_Array (1 .. 7) := (6, 1, 2, 3, 4, 5, 0);
      Output_1 : constant Storage_Array (1 .. 5) :=    (1, 2, 3, 4, 5);

      --  Short case with one replaced byte.
      Input_2  : constant Storage_Array (1 .. 7) := (3, 1, 2, 3, 4, 5, 0);
      Output_2 : constant Storage_Array (1 .. 5) :=    (1, 2, 0, 4, 5);

      --  Case where data size is 253 (no extra overhead byte)
      Input_3 : constant Storage_Array (1 .. 255) :=
        (1      => 254, --  Overhead byte
         255    => 0,   --  Frame delimiter
         others => 123);
      Output_3 : constant Storage_Array (1 .. 253) := (others => 123);

      --  Case where data size is 254 (no extra overhead byte)
      Input_4 : constant Storage_Array (1 .. 256) :=
        (1      => 255, --  Overhead byte
         256    => 0,   --  Frame delimiter
         others => 123);
      Output_4 : constant Storage_Array (1 .. 254) := (others => 123);

      --  Case where data size is 255 (1 extra overhead byte)
      Input_5 : constant Storage_Array (1 .. 258) :=
        (1      => 255, --  Overhead byte
         256    => 2,   --  Overhead byte
         258    => 0,   --  Frame delimiter
         others => 123);
      Output_5 : constant Storage_Array (1 .. 255) := (others => 123);

      Output : Storage_Array (1 .. 1000);
      Length : Storage_Count;
   begin

      COBS.Decode (Input_1, Output, Length);
      Assert (Output (1 .. Length) = Output_1, "Failed test vector 1");

      COBS.Decode (Input_2, Output, Length);
      Assert (Output (1 .. Length) = Output_2, "Failed test vector 2");

      COBS.Decode (Input_3, Output, Length);
      Assert (Output (1 .. Length) = Output_3, "Failed test vector 3");

      COBS.Decode (Input_4, Output, Length);
      Assert (Output (1 .. Length) = Output_4, "Failed test vector 4");

      COBS.Decode (Input_5, Output, Length);
      Assert (Output (1 .. Length) = Output_5, "Failed test vector 5");

   end Test_Decode_Test_Vectors;

   ---------------------------------
   -- Test_Encode_Decode_Loopback --
   ---------------------------------

   --  Test that encodes then decodes data of varying length.
   --
   --  The decoded data should match the original data that was encoded.

   procedure Test_Encode_Decode_Loopback (T : in out Test)
   is
      Input   : Storage_Array (1 .. 1000);
      Encoded : Storage_Array (1 .. 2000);
      Decoded : Storage_Array (1 .. 2000);
      Length  : Storage_Count;

   begin
      --  Generate a sequence containing no zeroes

      for I in Input'Range loop
         Input (I) := Storage_Element (I mod Storage_Element'Modulus);

         if Input (I) = 0 then
            Input (I) := Input (I) + 1;
         end if;
      end loop;

      --  Put some zeroes in the data with varying distances between them.

      Input (5)   := 0;
      Input (300) := 0;
      Input (400) := 0;
      Input (401) := 0;
      Input (402) := 0;
      Input (700) := 0;
      Input (999) := 0;

      --  Test varying lengths of input data.

      for I in 1 .. Input'Length loop
         COBS.Encode (Input, Encoded, Length);
         COBS.Decode (Encoded (1 .. Length), Decoded, Length);

         Assert (Length = Input'Length,
                 "Invalid length on iteration:" & Integer'Image (I));

         Assert (Decoded (1 .. Length) = Input,
                 "Decoded data does not match original input on iteration:" &
                 Integer'Image (I));
      end loop;
   end Test_Encode_Decode_Loopback;

   -------------------------------------------
   -- Test_Array_Upper_Limit_Positive_Range --
   -------------------------------------------

   --  Test that Array_Length_Within_Bounds returns True when given the
   --  largest allowed range across positive values.

   procedure Test_Array_Upper_Limit_Positive_Range (T : in out Test) is
   begin
      Assert (COBS.Array_Length_Within_Bounds (1, Storage_Offset'Last),
              "Check unexpectedly failed");
   end Test_Array_Upper_Limit_Positive_Range;

   -------------------------------------------
   -- Test_Array_Upper_Limit_Negative_Range --
   -------------------------------------------

   --  Test that Array_Length_Within_Bounds returns True when given the
   --  largest allowed range across negative values.

   procedure Test_Array_Upper_Limit_Negative_Range (T : in out Test) is
   begin
      Assert (COBS.Array_Length_Within_Bounds (Storage_Offset'First, -2),
              "Check unexpectedly failed");
   end Test_Array_Upper_Limit_Negative_Range;

   ------------------------------------------------
   -- Test_Array_Upper_Limit_Zero_Crossing_Range --
   ------------------------------------------------

   --  Test that Array_Length_Within_Bounds returns True when given the
   --  largest allowed range symmetrically across zero values.

   procedure Test_Array_Upper_Limit_Zero_Crossing_Range (T : in out Test) is
      First : constant Storage_Offset := Storage_Offset'First / 2;
      Last  : constant Storage_Offset := First + Storage_Offset'Last - 1;

   begin
      Assert (COBS.Array_Length_Within_Bounds (First, Last),
              "Check unexpectedly failed");
   end Test_Array_Upper_Limit_Zero_Crossing_Range;

   -------------------------------------------
   -- Test_Bounds_Exceeded_Positive_Range --
   -------------------------------------------

   --  Test that Array_Length_Within_Bounds returns False when given
   --  a positive range that is one past the maximum allowed length.

   procedure Test_Bounds_Exceeded_Positive_Range (T : in out Test) is
   begin
      Assert (not COBS.Array_Length_Within_Bounds (0, Storage_Offset'Last),
              "Check unexpectedly succeeded");
   end Test_Bounds_Exceeded_Positive_Range;

   -------------------------------------------
   -- Test_Bounds_Exceeded_Negative_Range --
   -------------------------------------------

   --  Test that Array_Length_Within_Bounds returns False when given
   --  a negative range that is one past the maximum allowed length.

   procedure Test_Bounds_Exceeded_Negative_Range (T : in out Test) is
   begin
      Assert (not COBS.Array_Length_Within_Bounds (Storage_Offset'First, -1),
              "Check unexpectedly succeeded");
   end Test_Bounds_Exceeded_Negative_Range;

   ------------------------------------------------
   -- Test_Bounds_Exceeded_Zero_Crossing_Range --
   ------------------------------------------------

   --  Test that Array_Length_Within_Bounds returns False when given
   --  a range crossing zero that is one past the maximum allowed length.

   procedure Test_Bounds_Exceeded_Zero_Crossing_Range (T : in out Test) is
      First : constant Storage_Offset := Storage_Offset'First / 2;
      Last  : constant Storage_Offset := First + Storage_Offset'Last;
   begin
      Assert (not COBS.Array_Length_Within_Bounds (First, Last),
              "Check unexpectedly succeeded");
   end Test_Bounds_Exceeded_Zero_Crossing_Range;

   -----------------------------------
   -- Test_Bounds_Check_Empty_Range --
   -----------------------------------

   --  Test that Array_Length_Within_Bounds returns True when given
   --  an empty/null range.

   procedure Test_Bounds_Check_Empty_Range (T : in out Test) is
   begin
      Assert (COBS.Array_Length_Within_Bounds (0, -1),
              "Check unexpectedly failed");
   end Test_Bounds_Check_Empty_Range;

   -----------------------------------
   -- Test_Max_Overhead_Bytes_Empty --
   -----------------------------------

   --  Test Max_Overhead_Bytes with length zero.

   procedure Test_Max_Overhead_Bytes_Empty (T : in out Test) is
      Result : Storage_Count;
   begin
      Result := COBS.Max_Overhead_Bytes (0);
      Assert (Result = 1,
              "Incorrect overhead bytes count:" &
                Storage_Count'Image (Result));
   end Test_Max_Overhead_Bytes_Empty;

   ---------------------------------------------
   -- Test_Max_Overhead_Bytes_Under_One_Block --
   ---------------------------------------------

   --  Test Max_Overhead_Bytes with length of one byte under one block.

   procedure Test_Max_Overhead_Bytes_Under_One_Block (T : in out Test) is
      Result : Storage_Count;
   begin
      Result := COBS.Max_Overhead_Bytes (253);
      Assert (Result = 1,
              "Incorrect overhead bytes count:" &
                Storage_Count'Image (Result));
   end Test_Max_Overhead_Bytes_Under_One_Block;

   ---------------------------------------
   -- Test_Max_Overhead_Bytes_One_Block --
   ---------------------------------------

   --  Test Max_Overhead_Bytes with length of exactly one block.

   procedure Test_Max_Overhead_Bytes_One_Block (T : in out Test) is
      Result : Storage_Count;
   begin
      Result := COBS.Max_Overhead_Bytes (254);
      Assert (Result = 2,
              "Incorrect overhead bytes count:" &
                Storage_Count'Image (Result));
   end Test_Max_Overhead_Bytes_One_Block;

   ----------------------------------------
   -- Test_Max_Overhead_Bytes_Over_Block --
   ----------------------------------------

   --  Test Max_Overhead_Bytes with length of one byte more than one block.

   procedure Test_Max_Overhead_Bytes_Over_Block (T : in out Test) is
      Result : Storage_Count;
   begin
      Result := COBS.Max_Overhead_Bytes (255);
      Assert (Result = 2,
              "Incorrect overhead bytes count:" &
                Storage_Count'Image (Result));
   end Test_Max_Overhead_Bytes_Over_Block;

   ----------------------------------------
   -- Test_Max_Overhead_Bytes_Two_Blocks --
   ----------------------------------------

   --  Test Max_Overhead_Bytes with length of exactly two blocks.

   procedure Test_Max_Overhead_Bytes_Two_Blocks (T : in out Test) is
      Result : Storage_Count;
   begin
      Result := COBS.Max_Overhead_Bytes (254 * 2);
      Assert (Result = 3,
              "Incorrect overhead bytes count:" &
                Storage_Count'Image (Result));
   end Test_Max_Overhead_Bytes_Two_Blocks;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Test_Caller.Create
                      ("Encode an empty array",
                       Test_Encode_Empty'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Encode an array of all zeroes",
                       Test_Encode_All_Zeroes'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Encode an array of no zeroes",
                       Test_Encode_No_Zeroes'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Decode an empty frame",
                       Test_Decode_Empty_Frame'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Decode a frame with a missing frame delimiter",
                       Test_Decode_No_Frame_Delimiter'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Decode test vectors",
                       Test_Decode_Test_Vectors'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Encode/decode loopback",
                       Test_Encode_Decode_Loopback'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test bounds check upper limit (positive range)",
                       Test_Array_Upper_Limit_Positive_Range'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test bounds check upper limit (negative range)",
                       Test_Array_Upper_Limit_Negative_Range'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test bounds check upper limit (zero-crossing range)",
                       Test_Array_Upper_Limit_Zero_Crossing_Range'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test bounds check 1 past limit (positive range)",
                       Test_Bounds_Exceeded_Positive_Range'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test bounds check 1 past limit (negative range)",
                       Test_Bounds_Exceeded_Negative_Range'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test bounds check 1 past limit (zero-crossing range)",
                       Test_Bounds_Exceeded_Zero_Crossing_Range'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test bounds check (empty range)",
                       Test_Bounds_Check_Empty_Range'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test overhead bytes with 0 length",
                       Test_Max_Overhead_Bytes_Empty'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test overhead bytes with just under one block",
                       Test_Max_Overhead_Bytes_Under_One_Block'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test overhead bytes with exactly one block",
                       Test_Max_Overhead_Bytes_One_Block'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test overhead bytes with just over one block",
                       Test_Max_Overhead_Bytes_Over_Block'Access));
      Ret.Add_Test (Test_Caller.Create
                      ("Test overhead bytes with exactly two blocks",
                       Test_Max_Overhead_Bytes_Two_Blocks'Access));
      return Ret;
   end Suite;

end COBS_Tests;
