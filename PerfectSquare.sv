`default_nettype none

 // add a single BCD digit to another
module BCDOneDigitAdd
  (input logic [3:0] A, B,
  input logic Cin,
  output logic [3:0] Sum,
  output logic Cout);
  // create internal 5-bit temp_sum
  // 4-bits for the digit, plus a carry
  logic [4:0] temp_sum;

  // get the temporary sum A + B + carryIn
  assign temp_sum = A + B + Cin;

  // use temp_sum to get the final sum and carryOut
  always_comb begin

    if (temp_sum >= 10) begin
      Sum = temp_sum - 4'd10; // sum = temp_sum without carry
      Cout = 1'b1; // carry is 1 since temp_sum >= 10
    end
    else begin
      Sum = temp_sum; // sum = temp_sum
      Cout = 1'b0; // carry is 0 since temp_sum <= 10
    end
  end

endmodule: BCDOneDigitAdd

// test BCDOneDigitAddS
// module BCDOneDigitAdd_test();
// logic [3:0] A,B,Sum;
// logic Cin, Cout;
// BCDOneDigitAdd DUT(.A(A), .B(B),. Sum(Sum), .Cin(Cin), .Cout(Cout));
// initial begin
// $monitor($time,,
// "A=%b, B=%b, Sum=%b, Cin=%b, Cout=%b",
// A,B,Sum,Cin,Cout);
// A=4'b0001;
// B=4'b0000;
// Cin=1;
// #10 A=4'b1000; //10
// B=4'b0010;
// Cin=0;
// #10 A=4'b1000; //17
// B=4'b1000;
// Cin=1;
// #10 A=4'b0011; //15
// B=4'b0100;
// Cin=0;
// end
// endmodule: BCDOneDigitAdd_test
// add two 2-BCD digit numbers together

module BCDThreeDigitAdd
  (input logic [3:0] A, B, C,
  output logic [7:0] Sum);

  // create first adder output wires
  logic [3:0] ab, abc_no_carry;
  logic temp_carry1, temp_carry2;

  // get first sum: A + B
  BCDOneDigitAdd ad0(.A, .B, .Sum(ab), .Cin('0), .Cout(temp_carry1));
  BCDOneDigitAdd ad1(.A(ab), .B(C), .Cin('0),
                .Sum(abc_no_carry), .Cout(temp_carry2));
  logic [3:0] tens;

  assign tens = temp_carry1 + temp_carry2;
  assign Sum = {tens,abc_no_carry};

  endmodule: BCDThreeDigitAdd

// test BCDThreeDigitAdd - CHECK TEST CASES!!!
// module BCDThreeDigitAdd_test();
// logic [3:0] A,B,C;
// logic [7:0] Sum;
// BCDThreeDigitAdd DUT(.A(A), .B(B), .C(C), .Sum(Sum));
// initial begin
// $monitor($time,,
// "A=%d, B=%d, C=%d, Sum=%b",
// A, B, C, Sum);
// A=4'b1000;
// B=4'b1000;
// C=4'b0000;
// #10 A=4'b1000; //12
// B=4'b0010;
// C=4'b0010;
// #10 A=4'b1000; //24
// B=4'b1000;
// C=4'b1000;
// #10 A=4'b0011; //15
// B=4'b0100;
// C=4'b1000;
// #10 A=4'b1000; //15
// B=4'b0100;
// C=4'b0111;
// end
// endmodule: BCDThreeDigitAdd_test
// compare two values and decide if they are equal

module Comparator
  (input logic [7:0] A, B,
  output logic equal);

  // check if A = B
  assign equal = (A == B);

  endmodule: Comparator
//make test cases for comparator
// module Comparator_test();
// logic [3:0] A, B;
// logic equal;
// Comparator C(.A(A), .B(B), .equal(equal));
// initial begin
// $monitor($time,,
// "A=%b, B=%b, equal=%b",
// A,B,equal);
// #10 A=4'd5;
// B=4'd5;
// #10 A=4'd5;
// B=4'd6;
// #10 A=4'd0;
// B=4'd0;
// #10 $finish;
// end
// endmodule: Comparator_test
// compare three values and decide if they are all equal

module ThreeValueComparator
  (input logic [7:0] A, B, C,
  output logic equal);
  always_comb begin
    // check if A = B = C
    if(A==B && A==C) begin
      equal = 1;
    end
    else begin
      equal = 0;
    end
  end

endmodule: ThreeValueComparator

// // test ThreeValueComparator
// module ThreeValueComparator_test();
// logic [3:0] A, B, C;
// logic equal;
// ThreeValueComparator TVC(.A(A), .B(B), .C(C), .equal(equal));
// initial begin
// $monitor($time,,
// "A=%b, B=%b, C=%b, equal=%b",
// A,B,C,equal);
// #10 A=4'd5;
// B=4'd5;
// C=4'd5;
// #10 A=4'd5;
// B=4'd6;
// C=4'd5;
// #10 A=4'd6;
// B=4'd5;
// C=4'd5;
// #10 A=4'd5;
// B=4'd5;
// C=4'd6;
// #10 $finish;
// end
// endmodule: ThreeValueComparator_test
// compare all row, column, and diagonal sums

module FullComparator
  (input logic [7:0] c1,c2,c3,
  input logic [7:0] r1,r2,r3,
  input logic [7:0] d1,d2,
  output logic equal);
  // create some internal lines to connect the sub-comparators
  logic c,r,d,cdr;
  // check that all columns are equal
  ThreeValueComparator col(.equal(c),.A(c1),.B(c2),.C(c3));
  // check that all rows are equal
  ThreeValueComparator row(.equal(r),.A(r1),.B(r2),.C(r3));
  // check that both diagonals are equal
  Comparator diag(.equal(d),.A(d1),.B(d2));
  // check that all columns = all rows = all diagonals, by commutativity
  ThreeValueComparator all(.equal(cdr),.A(c1),.B(r1),.C(d1));
  // check that all previous checks have passed and all is equal

  always_comb begin

    if(cdr && r && c && d)begin
      equal = 1'b1;
    end
    else begin
      equal = 1'b0;
    end
  end

endmodule: FullComparator

// //test cases for FullComparator
// module FullComparator_test();
// logic [7:0] c1,c2,c3;
// logic [7:0] r1,r2,r3;
// logic [7:0] d1,d2;
// logic equal;
// FullComparator FC(.equal, .c1(c1), .c2(c2), .c3(c3), .r1(r1),
// .r2(r2), .r3(r3), .d1(d1), .d2(d2));
// initial begin
// $monitor($time,,
// "c1=%b, c2=%b, c3=%b, r1=%b, r2=%b, r3=%b, d1=%b, d2=%b, equal=%b",
// c1,c2,c3,r1,r2,r3,d1,d2,equal);
// #10 c1=4'd4;
// c2=4'd4;
// c3=4'd4;
// r1=4'd4;
// r2=4'd4;
// r3=4'd4;
// d1=4'd4;
// d2=4'd4;
// #10 c1=4'd4;
// c2=4'd4;
// c3=4'd4;
// r1=4'd5;
// r2=4'd5;
// r3=4'd5;
// d1=4'd6;
// d2=4'd6;
// #10 c1=4'd4;
// c2=4'd4;
// c3=4'd4;
// r1=4'd4;
// r2=4'd5;
// r3=4'd4;
// d1=4'd4;
// d2=4'd4;
// #10 $finish;
// end
// endmodule:FullComparator_test
// make sums, then compare them to see if they are equal

module EquivalenceChecker
  (input logic [35:0] values,
  output logic [7:0] magic_constant,
  output logic equal);

  // define internal sum variables
  logic [7:0] r1, r2, r3, c1, c2, c3, d1, d2;
  logic carry1, carry2, carry3, carry4, carry5, carry6, carry7, carry8;

  // get row sums
  BCDThreeDigitAdd row1(.A(values[3:0]), .B(values[7:4]),
  .C(values[11:8]), .Sum(magic_constant));
  BCDThreeDigitAdd row2(.A(values[15:12]), .B(values[19:16]),
  .C(values[23:20]), .Sum(r2));
  BCDThreeDigitAdd row3(.A(values[27:24]), .B(values[31:28]),
  .C(values[35:32]), .Sum(r3));

  // get column sums
  BCDThreeDigitAdd col1(.A(values[3:0]), .B(values[15:12]),
  .C(values[27:24]), .Sum(c1));
  BCDThreeDigitAdd col2(.A(values[7:4]), .B(values[19:16]),
  .C(values[31:28]), .Sum(c2));
  BCDThreeDigitAdd col3(.A(values[11:8]), .B(values[23:20]),
  .C(values[35:32]), .Sum(c3));

  // get diagonal sums
  BCDThreeDigitAdd diag1(.A(values[3:0]), .B(values[19:16]),
  .C(values[35:32]), .Sum(d1));
  BCDThreeDigitAdd diag2(.A(values[27:24]), .B(values[19:16]),
  .C(values[11:8]), .Sum(d2));

  // compare sums to see whether they are all equal; set equal accordingly
  FullComparator fc0(.r1(magic_constant), .r2, .r3, .c1, .c2,
  .c3, .d1, .d2, .equal);

  endmodule: EquivalenceChecker
// test EquivalenceChecker
// module EquivalenceChecker_test
// ();
// // create internal wires, instantiate DUT
// logic [35:0] values;
// logic [7:0] magic_constant;
// logic equal;
// EquivalenceChecker ec0(.values, .magic_constant, .equal);
// // test desired cases
// initial begin
// // start monitor to see results
// $monitor("values: %b, magic constant: %b, equal: %b",
// values, magic_constant, equal);
// // test case 1: 0000_0000_0000_0000_0000_0000_0000_0000_0000
// // expect to see magic_constant 0, equal 1 (even if invalid)
// #10 values = 36'b0000_0000_0000_0000_0000_0000_0000_0000_0000;
// // test case 2: 0100_1001_0010_0011_0101_0111_1000_0001_0110
// // expect to see magic_constant 1111, equal 1
// #10 values = 36'b0100_1001_0010_0011_0101_0111_1000_0001_0110;
// end
// endmodule: EquivalenceChecker_test
// validate a single cell's BCD input

module OneDigitInputValidator
  (input logic [3:0]D,
  input logic [8:0] found_in,
  output logic [8:0] found_out);
    always_comb begin
    // check if a previous value was invalid
    // depending on the value of D, check whether it has been seen before
    // if so, make input invalid; otherwise, write that it has been seen
    //case (invalid_in)
    case (D)

      4'd1: begin
        if (found_in[0] == 1) begin
          found_out=found_in;
        end
        else begin
        found_out = found_in+9'b000_000_001;
        end
      end

      4'd2: begin
        if (found_in[1] == 1) begin
          found_out=found_in;
        end
        else begin
          found_out = found_in+9'b000_000_010;
        end
      end

      4'd3: begin
        if (found_in[2] == 1)begin
          found_out=found_in;
        end
        else begin
          found_out = found_in+9'b000_000_100;
        end
      end

      4'd4: begin
        if (found_in[3] == 1)begin
          found_out=found_in;
        end
        else begin
          found_out = found_in+9'b000_001_000;
        end
      end

      4'd5: begin
        if (found_in[4] == 1)begin
          found_out=found_in;
        end
        else begin
          found_out = found_in+9'b000_010_000;
        end
      end

      4'd6: begin
        if (found_in[5] == 1)begin
          found_out=found_in;
        end
        else begin
          found_out = found_in+9'b000_100_000;
        end
      end

      4'd7: begin
        if (found_in[6] == 1)begin
          found_out=found_in;
        end
        else begin
          found_out = found_in+9'b001_000_000;
        end
      end

      4'd8: begin
        if (found_in[7] == 1)begin
          found_out=found_in;
        end
        else begin
          found_out = found_in+9'b010_000_000;
        end
      end

      4'd9: begin
        if (found_in[8] == 1)begin
          found_out=found_in;
        end
        else begin

          found_out = found_in+9'b100_000_000;
        end
      end

      default: begin
        found_out = found_in;
      end
    endcase
  end

endmodule: OneDigitInputValidator

// test OneDigitInputValidator
// module OneDigitInputValidator_test();
// logic [3:0]D;
// logic invalid_in;
// logic [8:0] found_in;
// logic invalid_out;
// logic [8:0] found_out;
// OneDigitInputValidator ODIV(.D(D),.invalid_in(invalid_in),
// .found_in(found_in),
// .invalid_out(invalid_out),
// .found_out(found_out));
// initial begin
// $monitor($time,,
// "D=%b,invalid_in=%b,found_in=%b,invalid_out=%b,found_out=%b",
// D,invalid_in,found_in,invalid_out,found_out);
// D=4'b0010;
// invalid_in=1'b0;
// found_in=9'b011_000_000;
// #10 D=4'b0001;
// invalid_in=1'b0;
// found_in=9'b011_000_001;
// #10 D=4'b0001;
// invalid_in=1'b0;
// found_in=9'b111_111_110;
// #10 D=4'b0001;
// invalid_in=1'b1;
// found_in=9'b011_000_000;
// #10 D=4'b0001;
// invalid_in=1'b1;
// found_in=9'b011_000_001;
// #10 D=4'b0001;
// invalid_in=1'b0;
// found_in=9'b000_000_000;
// #10 D=4'b0000;
// invalid_in=1'b0;
// found_in=9'b000_000_000;
// end
// endmodule: OneDigitInputValidator_test

module InputValidator
  (input logic [35:0] values,
  output logic invalid);
  // create wires for one-digit validator chaining
  logic [8:0] found1, found2, found3, found4, found5, found6, found7,
              found8, found9;
  // chain multiple one-digit validators to validate all values
  OneDigitInputValidator iv0(
                            .D(values[3:0]),
                            .found_in('0),
                            .found_out(found1)
                            );
  OneDigitInputValidator iv1(
                            .D(values[7:4]),
                            .found_in(found1),
                            .found_out(found2)
                            );
  OneDigitInputValidator iv2(
                            .D(values[11:8]),
                            .found_in(found2),
                            .found_out(found3)
                            );
  OneDigitInputValidator iv3(
                            .D(values[15:12]),
                            .found_in(found3),
                            .found_out(found4)
                            );
  OneDigitInputValidator iv4(
                            .D(values[19:16]),
                            .found_in(found4),
                            .found_out(found5)
                            );
OneDigitInputValidator iv5(
                            .D(values[23:20]),
                            .found_in(found5),
                            .found_out(found6)
                            );
  OneDigitInputValidator iv6(
                            .D(values[27:24]),
                            .found_in(found6),
                            .found_out(found7)
                            );
  OneDigitInputValidator iv7(
                            .D(values[31:28]),
                            .found_in(found7),
                            .found_out(found8)
                            );
  OneDigitInputValidator iv8(
                            .D(values[35:32]),
                            .found_in(found8),
                            .found_out(found9)
                            );
  assign invalid =(found9===9'b111_111_111) ? 0:1;

endmodule: InputValidator

// test InputValidator
// module InputValidator_test();
// logic [35:0] values;
// logic invalid;
// InputValidator DUT(.values(values),.invalid(invalid));
// initial begin
// $monitor($time,,
// "values=%b, invalid=%b",
// values,invalid);
// values=36'd0;
// #10 values=36'b0001_0010_0011_0100_0101_0110_0111_1000_1001;
// #10 values=36'b0011_1000_0100_0011_0001_1100_1001_0110_0000;
// #10 values=36'b1000_1100_0100_0010_0001_1010_1001_0110_0000;
// #10 values=36'b1000_0100_0010_0001_0011_0111_1111_1001_0000;
// #10 values=36'b1000_1000_1000_1000_1000_1000_1000_1000_1000;
// #10 values=36'b1001_0001_0010_0011_0100_0101_0110_0111_1000;

// #10 values=36'b1101_0001_0010_0011_0100_0101_0110_0111_1000;
// end
// endmodule: InputValidator_test
// full implementation module: combine validate input and compare sum modules

module IsMagic
  (input logic [3:0] num1, num2, num3, //top row, L to R
  input logic [3:0] num4, num5, num6, //middle row
  input logic [3:0] num7, num8, num9, //bottom row
  output logic [7:0] magic_constant, //2 BCD digits
  output logic it_is_magic);

  // define internal logic wires
  logic equal, invalid;

  // make input numbers --> values
  logic [35:0] values;
  assign values = ({num1, num2, num3, num4, num5, num6, num7, num8, num9});

  // instantiate EquivalenceChecker and InputValidator and connect them
  EquivalenceChecker ec0(.values, .equal, .magic_constant);
  InputValidator iv0(.values, .invalid);

  // setup the logic for whether it is magic or not
  assign it_is_magic = (~invalid) & (equal);
  endmodule: IsMagic

// test IsMagic
// module IsMagic_test
// (); // no inputs and outputs; using bench style 2 (self-contined)
// // create internal wires, instantiate DUT
// logic [3:0] num1, num2, num3, num4, num5, num6, num7, num8, num9;
// logic [7:0] magic_constant;
// logic it_is_magic;
// IsMagic im0(.num1, .num2, .num3,
// .num4, .num5, .num6,
// .num7, .num8, .num9, .magic_constant, .it_is_magic);
// // test desired cases
// initial begin
// // start monitor to see results
// $monitor($time,
// " %b_%b_%b_%b_%b_%b_%b_%b_%b, constant: %b, magic: %b",
// num1, num2, num3,
// num4, num5, num6,
// num7, num8, num9,
// magic_constant, it_is_magic);
// // test case 1: 6 1 8 7 5 3 2 9 4
// // expect to see magic_constant 1111, it_is_magic 1
// #10 num1 = 4'd6;
// num2 = 4'd1;
// num3 = 4'd8;
// num4 = 4'd7;
// num5 = 4'd5;
// num6 = 4'd3;
// num7 = 4'd2;
// num8 = 4'd9;
// num9 = 4'd4;
// // test case 2: 8 1 6 3 5 7 4 9 2
// // expect to see magic_constant 1111, it_is_magic 1
// #10 num1 = 4'd8;
// num2 = 4'd1;
// num3 = 4'd6;
// num4 = 4'd3;

// num5 = 4'd5;
// num6 = 4'd7;
// num7 = 4'd4;
// num8 = 4'd9;
// num9 = 4'd2;
// // test case 3: 6 7 2 1 5 9 8 3 4
// // expect to see magic_constant 1111, it_is_magic 1
// #10 num1 = 4'd6;
// num2 = 4'd7;
// num3 = 4'd2;
// num4 = 4'd1;
// num5 = 4'd5;
// num6 = 4'd9;
// num7 = 4'd8;
// num8 = 4'd3;
// num9 = 4'd4;
// // test case 4: 8 3 4 1 5 9 6 7 2
// // expect to see magic_constant 1111, it_is_magic 1
// #10 num1 = 4'd8;
// num2 = 4'd3;
// num3 = 4'd4;
// num4 = 4'd1;
// num5 = 4'd5;
// num6 = 4'd9;
// num7 = 4'd6;
// num8 = 4'd7;
// num9 = 4'd2;
// // test case 5: 2 7 6 9 5 1 4 3 8
// // expect to see magic_constant 1111, it_is_magic 1
// #10 num1 = 4'd2;
// num2 = 4'd7;
// num3 = 4'd6;
// num4 = 4'd9;
// num5 = 4'd5;
// num6 = 4'd1;
// num7 = 4'd4;
// num8 = 4'd3;
// num9 = 4'd8;
// // test case 6: 4 3 8 9 5 1 2 7 6
// // expect to see magic_constant 1111, it_is_magic 1
// #10 num1 = 4'd4;
// num2 = 4'd3;
// num3 = 4'd8;
// num4 = 4'd9;
// num5 = 4'd5;
// num6 = 4'd1;
// num7 = 4'd2;
// num8 = 4'd7;
// num9 = 4'd6;
// // test case 7: 82 9 4 7 5 3 6 1 8
// // expect to see magic_constant 1111, it_is_magic 1
// #10 num1 = 4'd2;
// num2 = 4'd9;
// num3 = 4'd4;
// num4 = 4'd7;
// num5 = 4'd5;
// num6 = 4'd3;
// num7 = 4'd6;
// num8 = 4'd1;
// num9 = 4'd8;
// // test case 8: 4 9 2 3 5 7 8 1 6
// // expect to see magic_constant 1111, it_is_magic 1
// #10 num1 = 4'd4;
// num2 = 4'd9;
// num3 = 4'd2;

// num4 = 4'd3;
// num5 = 4'd5;
// num6 = 4'd7;
// num7 = 4'd8;
// num8 = 4'd1;
// num9 = 4'd6;
// // test case 9: edge case; all 5's
// // expect to see magic_constant 15 (1111), it_is_magic 0
// #10 num1 = 4'd5;
// num2 = 4'd5;
// num3 = 4'd5;
// num4 = 4'd5;
// num5 = 4'd5;
// num6 = 4'd5;
// num7 = 4'd5;
// num8 = 4'd5;
// num9 = 4'd5;
// // test case 10: edge case; all 0's
// // expect to see magic_constant 0, it_is_magic 0
// #10 num1 = 4'd0;
// num2 = 4'd0;
// num3 = 4'd0;
// num4 = 4'd0;
// num5 = 4'd0;
// num6 = 4'd0;
// num7 = 4'd0;
// num8 = 4'd0;
// num9 = 4'd0;
// // test case 11: test case 2, -1 in all spots
// // expect to see magic_constant 12 (1100), it_is_magic 0
// #10 num1 = 4'd7;
// num2 = 4'd0;
// num3 = 4'd5;
// num4 = 4'd2;
// num5 = 4'd4;
// num6 = 4'd6;
// num7 = 4'd3;
// num8 = 4'd8;
// num9 = 4'd1;
// // test case 12: test case 2, +1 in all spots
// // expect to see magic_constant 15 (1111 overfow), it_is_magic 0
// #10 num1 = 4'd9;
// num2 = 4'd2;
// num3 = 4'd7;
// num4 = 4'd4;
// num5 = 4'd6;
// num6 = 4'd8;
// num7 = 4'd5;
// num8 = 4'd10;
// num9 = 4'd3;
// // test case 13: test case 2, swapping 2 random cells
// // expect to see magic_constant (15), it_is_mgic 0
// #10 num1 = 4'd8;
// num2 = 4'd1;
// num3 = 4'd6;
// num4 = 4'd3;
// num5 = 4'd5;
// num6 = 4'd7;
// num7 = 4'd2; // swapped
// num8 = 4'd9;
// num9 = 4'd4; // swapped
// #10 $finish;
// end

// endmodule: IsMagic_test

// helper module for enabling all LEDs if the square is magical
module MagicLEDs
  (input logic it_is_magic,
  output logic [7:0] LEDG);
  always_comb begin
    if (it_is_magic) LEDG = 8'b1111_1111;
    else LEDG = 8'b0000_0000;
  end

endmodule: MagicLEDs

// helper module for displaying a BCD digit via 7-segment display
module BCDToSevenSegment
  (input logic [3:0] BCD,
  output logic [6:0] seg);
  // recall that 7-segment displays are active low; 0's for ON, 1's for OFF
  always_comb begin
    case (BCD)

      4'd0: seg = 7'b100_0000;
      4'd1: seg = 7'b111_1001;
      4'd2: seg = 7'b010_0100;
      4'd3: seg = 7'b011_0000;
      4'd4: seg = 7'b001_1001;
      4'd5: seg = 7'b001_0010;
      4'd6: seg = 7'b000_0010;
      4'd7: seg = 7'b111_1000;
      4'd8: seg = 7'b000_0000;
      4'd9: seg = 7'b001_1000;

      default: seg = 7'b111_1111;
    endcase
  end

endmodule: BCDToSevenSegment

// ChipInterface - interface IsMagic and FPGA IO
module ChipInterface
  (output logic [6:0] HEX7, HEX6, // magic_constant
  HEX5, HEX4, HEX3, HEX2, HEX1, HEX0,
  output logic [7:0] LEDG,
  input logic [17:0] SW,
  input logic [3:0] KEY,
  input logic CLOCK_50); // needed for enter_9_bcd

  logic [3:0] num1, num2, num3,
  num4, num5, num6,
  num7, num8, num9;
  logic [7:0] magic_constant;
  logic it_is_magic;

  enter_9_bcd e(.entry(SW[3:0]),
  .selector(SW[7:4]),
  .enableL(KEY[0]),
  .zeroL(KEY[2]),
  .set_defaultL(KEY[1]),
  .clock(CLOCK_50),
  .*);

  IsMagic im(.*);

  // setup LEDs to light up if it_is_magic
  MagicLEDs ml0(.it_is_magic, .LEDG);
  
  // output BCD digits into the segment display
  BCDToSevenSegment b0(.BCD(magic_constant[3:0]), .seg(HEX6)); // LSB "1's"
  BCDToSevenSegment b1(.BCD(magic_constant[7:4]), .seg(HEX7)); // MSB "10's"

endmodule : ChipInterface


module enter_9_bcd
  (input logic [3:0] entry,
  input logic [3:0] selector,
  input logic enableL, zeroL, set_defaultL, clock,
  output logic [3:0] num1, num2, num3, num4, num5, num6, num7, num8, num9);
  logic enableL_async, enableL_sync;
  logic zeroL_async, zeroL_sync;
  logic set_defaultL_async, set_defaultL_sync;
  
  // 2FF Synchronization
  always_ff @(posedge clock) begin
    enableL_async <= enableL;
    enableL_sync <= enableL_async;
    zeroL_async <= zeroL;
    zeroL_sync <= zeroL_async;
    set_defaultL_async <= set_defaultL;
    set_defaultL_sync <= set_defaultL_async;
  end

  always_ff @(posedge clock) begin
    if (~zeroL_sync) begin
      num1 <= 4'b0000;
      num2 <= 4'b0000;
      num3 <= 4'b0000;
      num4 <= 4'b0000;
      num5 <= 4'b0000;
      num6 <= 4'b0000;
      num7 <= 4'b0000;
      num8 <= 4'b0000;
      num9 <= 4'b0000;
    end

    else if (~set_defaultL_sync) begin
      num1 <= 4'b1000;
      num2 <= 4'b0001;
      num3 <= 4'b0110;
      num4 <= 4'b0011;
      num5 <= 4'b0101;
      num6 <= 4'b0111;
      num7 <= 4'b0100;
      num8 <= 4'b1001;
      num9 <= 4'b0010;
    end

    else if (~enableL_sync)
      unique case (selector)
        4'b0001: num1 <= entry;
        4'b0010: num2 <= entry;
        4'b0011: num3 <= entry;
        4'b0100: num4 <= entry;
        4'b0101: num5 <= entry;
        4'b0110: num6 <= entry;
        4'b0111: num7 <= entry;
        4'b1000: num8 <= entry;
        4'b1001: num9 <= entry;
      endcase
  end

endmodule: enter_9_bcd
