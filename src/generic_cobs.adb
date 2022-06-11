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
package body Generic_COBS
with Pure, SPARK_Mode => On
is

   ------------
   -- Decode --
   ------------

   procedure Decode (Input  :     Byte_Array;
                     Output : out Byte_Array;
                     Length : out Byte_Count) is
      B          : Byte;
      Code       : Byte := Byte'Last;
      Run_Length : Byte_Count := 0;

   begin
      Length := 0;

      for I in 0 .. Index'Base (Input'Length - 1) loop
         pragma Loop_Invariant (Length <= Byte_Count (I));

         pragma Loop_Invariant
           (for all J in 0 .. Length - 1 =>
              Output (Output'First + Index'Base (J))'Initialized);

         B := Input (Input'First + I);

         exit when B = Frame_Delimiter;

         if Run_Length > 0 then
            Output (Output'First + Index'Base (Length)) := B;
            Length := Length + 1;

         else
            if Code /= Byte'Last then
               Output (Output'First + Index'Base (Length)) := Frame_Delimiter;
               Length                                      := Length + 1;
            end if;

            Code       := B;
            Run_Length := Byte_Count (Byte'Pos (B));
         end if;

         Run_Length := Run_Length - 1;
      end loop;
   end Decode;

   ------------
   -- Encode --
   ------------

   procedure Encode (Input  :     Byte_Array;
                     Output : out Byte_Array;
                     Length : out Byte_Count) is
      Block_Length : Positive_Byte_Count;
      Offset       : Byte_Count;
      Remaining    : Byte_Count;

      Nb_Overhead_Bytes : Positive_Byte_Count with Ghost;

   begin
      --  Encode first block
      Encode_Block (Input, Output, Length);

      Offset            := Length - 1;
      Remaining         := Input'Length - (Length - 1);
      Nb_Overhead_Bytes := 1;

      while Remaining > 0 loop
         pragma Loop_Variant (Decreases => Remaining,
                              Increases => Offset,
                              Increases => Length,
                              Increases => Nb_Overhead_Bytes);

         pragma Loop_Invariant (Offset + Remaining = Input'Length);

         pragma Loop_Invariant (Length = Offset + Nb_Overhead_Bytes);

         pragma Loop_Invariant
           (Nb_Overhead_Bytes < Max_Overhead_Bytes (Offset));

         pragma Loop_Invariant
           (for all I in Output'First ..
                         Output'First + Index'Base (Length - 1) =>
                Output (I)'Initialized);

         pragma Loop_Invariant
           (for all I in Output'First ..
                         Output'First + Index'Base (Length - 1) =>
                Output (I) /= Frame_Delimiter);

         Encode_Block
           (Input  (Input'First  + Index'Base (Offset) .. Input'Last),
            Output (Output'First + Index'Base (Length) .. Output'Last),
            Block_Length);

         Nb_Overhead_Bytes := Nb_Overhead_Bytes + 1;

         Length    := Length    + Block_Length;
         Offset    := Offset    + (Block_Length - 1);
         Remaining := Remaining - (Block_Length - 1);
      end loop;

      pragma Assert
        (for all I in Output'First ..
                      Output'First + Index'Base (Length - 1) =>
             Output (I) /= Frame_Delimiter);

      Output (Output'First + Index'Base (Length)) := Frame_Delimiter;

      Length := Length + 1;
   end Encode;

   --------------------
   --  Encode_Block  --
   --------------------

   procedure Encode_Block (Input  :     Byte_Array;
                           Output : out Byte_Array;
                           Length : out Byte_Count) is
      B : Byte;

      Code : Positive_Byte_Count  := 1;
      --  Keeps track of the length of the current run of non-zero octets.
      --  Note that this can be 1 more than Max_Run_Size since this is
      --  1 more than the current run length (it includes the overhead octet
      --  in its count).

      Code_Pos : Index   := Output'First;
      --  The position in 'Output' of the previous overhead/zero octet.
      --  When a run of up to 254 non-zero octets is completed (and thus the
      --  length of the run is now known), then the byte at this position in
      --  'Output' is updated with the run length.

   begin
      Length := 1; --  Initial overhead octet is appended.

      if Input'Length > 0 then
         for I in Byte_Count range 0 .. Input'Length - 1 loop
            pragma Warnings
              (Off, """Output"" may be referenced before it has a value",
               Reason => "Initialization of Output is guaranteed via proof");

            pragma Loop_Invariant (Code in 1 .. Maximum_Run_Length + 1);

            pragma Loop_Invariant
              (Code = Length - Byte_Count (Code_Pos - Output'First));

            pragma Loop_Invariant
              (Code_Pos in Output'First ..
                           Output'First + Index'Base (Length) - 1);

            pragma Loop_Invariant (Length = Byte_Count (I + 1));

            --  All bytes written so far are initialized, except the
            --  one at Code_Pos which is written later.
            pragma Loop_Invariant
              (for all I in Output'First ..
                            Output'First + Index'Base (Length) - 1 =>
                   (if I /= Code_Pos then Output (I)'Initialized));

            --  The frame delimiter is never written to the output.
            pragma Loop_Invariant
              (for all I in Output'First ..
                            Output'First + Index'Base (Length) - 1 =>
                   (if I /= Code_Pos then Output (I) /= Frame_Delimiter));

            pragma Warnings (On);

            --  Stop when a complete block is reached.
            exit when Code = Maximum_Run_Length + 1;

            B := Input (Input'First + Index'Base (I));
            if B /= Frame_Delimiter then
               --  Copy non-zero byte

               Output (Output'First + Index'Base (Length)) := B;

               Code   := Code   + 1;
               Length := Length + 1;

            else
               --  Replace zero byte

               Output (Code_Pos) := Byte'Val (Code);
               Code_Pos          := Output'First + Index'Base (Length);
               Length            := Length + 1;
               Code              := 1;
            end if;
         end loop;
      end if;

      Output (Code_Pos) := Byte'Val (Code);
   end Encode_Block;

end Generic_COBS;
