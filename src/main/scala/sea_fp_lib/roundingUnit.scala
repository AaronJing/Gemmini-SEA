package sea_fp_lib

import chisel3._
import chisel3.util._

class roundingUnit(expWidth: Int, mntWidth: Int) extends Module
{
  val io = IO(new Bundle {
    // mntWidth + 1 + GRS bits
    val norm_sum = Input(UInt((mntWidth + 4).W))
    val round = Input(UInt(2.W))
    val o_sgn = Input(UInt(1.W))
    val o_exp2 = Input(UInt(expWidth.W))
    val flag_zero2 = Input(UInt(1.W))
    val flag_inf2 = Output(UInt(1.W))
    val o_exp3 = Output(UInt(expWidth.W))
    val normalized_norm_sum_rounding = Output(UInt((mntWidth + 1).W))
  })

  // 5. Rounding
  // https://pages.cs.wisc.edu/~david/courses/cs552/S12/handouts/guardbits.pdf
  // rounding: Different Modes
  // round==0 : Round to nearest
  // round==1 : towards zero a.k.a truncation
  // round==2 : towards positive infinity
  // round==3 : towards negative infinity
  // hidden1 mnt GRS == p+3 bits
  val MAXEXP = ((1 << expWidth) - 1).U
  val p = mntWidth + 1
  val M_LSB = io.norm_sum(3)
  // Guard bit
  val G = io.norm_sum(2)
  // round OR sticky to be used in RB
  val RS = io.norm_sum(0) | io.norm_sum(1)
  val rounding = io.round
  val RB = Wire(UInt())
  when(rounding === 0.U) {
    // to nearest
    RB := G & (M_LSB|RS)
  }.elsewhen(rounding === 1.U){
    // towards zero
    RB := 0.U
  }.elsewhen(rounding === 2.U){
    // towards postive inf
    RB := (G|RS) & (~io.o_sgn)
  }.otherwise{
    // towards neg inf
    RB := (G|RS) & io.o_sgn
  }
  // add rounding bit
  // total bits are p+1
  val norm_sum_rounding = io.norm_sum(p+2,3) +& RB
  // get carry from rounding
  val carryFromNSM = norm_sum_rounding(p)
  // total bit are p
  io.normalized_norm_sum_rounding := Mux(carryFromNSM, norm_sum_rounding.head(p), norm_sum_rounding(p-1,0))
  val o_exp3 = io.o_exp2 + carryFromNSM
  io.o_exp3 := o_exp3
  io.flag_inf2 := (o_exp3 === MAXEXP) & ~(io.flag_zero2)
}
