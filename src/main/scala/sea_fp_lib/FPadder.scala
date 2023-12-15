package sea_fp_lib

import chisel3._
import chisel3.util._

class FPadder(expWidth: Int, mntWidth: Int, no_round: Boolean, unsigned: Boolean) extends Module
{
  val io = IO(new Bundle {
    val a = Input(UInt((expWidth + mntWidth + 1).W))
    val b = Input(UInt((expWidth + mntWidth + 1).W))
    val o = Output(UInt((expWidth + mntWidth + 1).W))
    val op = Input(Bool())
    val round = Input(UInt(2.W))
  })
    val MAXEXP = ((1 << expWidth) - 1).U
    val p = mntWidth +1
    val adderFrontend = if (!unsigned) Module(new fullFPadder(expWidth, mntWidth, no_round)) else Module(new samesignedFPadder(expWidth, mntWidth, no_round))
    val o_exp3 = Wire(UInt(expWidth.W))
    val normalized_norm_sum_rounding = Wire(UInt(p.W))
    if (!no_round) {
        val roundUnit = Module(new roundingUnit(expWidth, mntWidth))
        // P+3 from adder front end
        roundUnit.io.norm_sum := adderFrontend.ionorm_sum
        roundUnit.io.round := io.round
        roundUnit.io.o_sgn := adderFrontend.ioo_sgn
        roundUnit.io.o_exp2 := adderFrontend.ioo_exp2
        roundUnit.io.flag_zero2 := adderFrontend.ioflag_zero2
        adderFrontend.ioflag_inf2 := roundUnit.io.flag_inf2
        o_exp3 := roundUnit.io.o_exp3
        normalized_norm_sum_rounding := roundUnit.io.normalized_norm_sum_rounding
    } else {
        adderFrontend.ioflag_inf2 := 0.B
        o_exp3 := adderFrontend.ioo_exp2
        normalized_norm_sum_rounding := adderFrontend.ionorm_sum(mntWidth+3, 3)
    }
    

    adderFrontend.ioa := io.a
    adderFrontend.iob := io.b
    adderFrontend.ioop := io.op
    
    val cond = adderFrontend.iocond 
    val o_mnt = Wire(UInt((p).W))
    val o_exp4 = Wire(UInt(expWidth.W))
    when(cond === 0.U){
      o_mnt := normalized_norm_sum_rounding
      o_exp4 := o_exp3
    }.elsewhen(cond === 1.U){
      o_mnt := 0.U(p.W)
      o_exp4 := 0.U(expWidth.W)
    }.elsewhen(cond === 2.U){
      o_mnt := 0.U(p.W)
      o_exp4 := MAXEXP
    }.elsewhen(cond >= 4.U){
      o_mnt := 3.U(2.W) ## 0.U((p-2).W)
      o_exp4 := MAXEXP
    }.otherwise{
      o_mnt := Fill(p, 1.U)
      o_exp4 := MAXEXP
    }
    io.o := (adderFrontend.ioo_sgn) ## o_exp4 ## o_mnt(p-2,0)
}
