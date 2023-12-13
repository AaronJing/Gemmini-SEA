package sea_fp_lib

import chisel3._
import chisel3.util._

class floatTofloat(in_expWidth: Int, in_mntWidth: Int, out_expWidth: Int, out_mntWidth: Int) extends Module
{
  val io = IO(new Bundle {
    val a = Input(UInt((in_expWidth + out_mntWidth + 1).W))
    val o = Output(UInt((out_expWidth + out_mntWidth + 1).W))
  })
    val output_width = out_expWidth + out_mntWidth + 1
    val input_width = in_expWidth + in_mntWidth + 1
    // passing sign
    io.o(output_width-1) := io.a(input_width-1)
    // exponent adjustment
    val biasIN = ((1 << (in_expWidth - 1)) - 1).U
    val biasOUT = ((1 << (out_expWidth - 1)) - 1).U
    val MAXEXPOUT = ((1 << out_expWidth) - 1).U
    val MAXEXPIN = ((1 << in_expWidth) - 1).U
    val expIn = io.a(input_width - 2, in_mntWidth)
    val adjustedExp = expIn.zext - biasIN.zext + biasOUT.zext
    if (in_expWidth > out_expWidth) {
        io.o(output_width-2, out_mntWidth) := Mux(adjustedExp < 0.S, 0.U, Mux(adjustedExp >= MAXEXPOUT.zext, MAXEXPOUT, adjustedExp))
    } else if (in_expWidth < out_expWidth) {
        io.o(output_width-2, out_mntWidth) := adjustedExp
    } else {
        io.o(output_width-2, out_mntWidth) := io.a(input_width-2, in_mntWidth)
    }
    // mantissa adjustment
    if (in_mntWidth > out_mntWidth) {
        io.o(out_mntWidth-1, 0) := io.a(in_mntWidth-1, in_mntWidth-out_mntWidth)
    } else if (in_mntWidth < out_mntWidth) {
        io.o(out_mntWidth-1, 0) := io.a(in_mntWidth-1, 0) ## 0.U((out_mntWidth-in_mntWidth).W)
    } else {
        io.o(out_mntWidth-1, 0) := io.a(in_mntWidth-1, 0)
    }
    // special case handle
    

    val mntAzero = (~io.a(in_mntWidth-1,0).orR)
    val Azero = (~expIn.orR) & mntAzero
    when(Azero){
        io.o := 0.U
    }
}
