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
pragma SPARK_Mode (On);

with Generic_COBS;
with System.Storage_Elements;

--  This is an instantiation of the COBS encoder/decoder based on
--  the types defined in System.Storage_Elements.
--
--  This instantiation is only valid for systems that have a
--  8-bit Storage_Element type.
package COBS is new Generic_COBS
  (Byte       => System.Storage_Elements.Storage_Element,
   Index      => System.Storage_Elements.Storage_Offset,
   Byte_Count => System.Storage_Elements.Storage_Count,
   Byte_Array => System.Storage_Elements.Storage_Array) with Pure;
