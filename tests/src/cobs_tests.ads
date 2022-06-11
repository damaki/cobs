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
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package COBS_Tests
is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --------------------
   --  Encode tests  --
   --------------------

   procedure Test_Encode_Empty      (T : in out Test);
   procedure Test_Encode_All_Zeroes (T : in out Test);
   procedure Test_Encode_No_Zeroes  (T : in out Test);

   --------------------
   --  Decode tests  --
   --------------------

   procedure Test_Decode_Empty_Frame (T : in out Test);
   procedure Test_Decode_No_Frame_Delimiter (T : in out Test);
   procedure Test_Decode_Test_Vectors (T : in out Test);

   ---------------------------
   --  Encode/decode tests  --
   ---------------------------

   procedure Test_Encode_Decode_Loopback (T : in out Test);

   ----------------------------------------
   --  Array_Length_Within_Bounds tests  --
   ----------------------------------------

   procedure Test_Array_Upper_Limit_Positive_Range (T : in out Test);
   procedure Test_Array_Upper_Limit_Negative_Range (T : in out Test);
   procedure Test_Array_Upper_Limit_Zero_Crossing_Range (T : in out Test);
   procedure Test_Bounds_Exceeded_Positive_Range (T : in out Test);
   procedure Test_Bounds_Exceeded_Negative_Range (T : in out Test);
   procedure Test_Bounds_Exceeded_Zero_Crossing_Range (T : in out Test);
   procedure Test_Bounds_Check_Empty_Range (T : in out Test);

   --------------------------------
   --  Max_Overhead_Bytes tests  --
   --------------------------------

   procedure Test_Max_Overhead_Bytes_Empty (T : in out Test);
   procedure Test_Max_Overhead_Bytes_Under_One_Block (T : in out Test);
   procedure Test_Max_Overhead_Bytes_One_Block (T : in out Test);
   procedure Test_Max_Overhead_Bytes_Over_Block (T : in out Test);
   procedure Test_Max_Overhead_Bytes_Two_Blocks (T : in out Test);

   ----------------
   -- Test suite --
   ----------------

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end COBS_Tests;
