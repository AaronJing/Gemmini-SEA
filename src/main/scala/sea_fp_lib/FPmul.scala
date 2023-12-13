package sea_fp_lib

import chisel3._
import chisel3.util._

class FPmul(expWidth: Int, mntWidth: Int) extends Module
{
  val io = IO(new Bundle {
    val a = Input(UInt((expWidth + mntWidth + 1).W))
    val b = Input(UInt((expWidth + mntWidth + 1).W))
    val o = Output(UInt((expWidth + mntWidth + 1).W))
    val round = Input(UInt(2.W))
  })
  val total_width = mntWidth + expWidth + 1
  val p = mntWidth + 1
  val bias = ((1 << (expWidth - 1)) - 1).U
  val MAXEXP = ((1 << expWidth) - 1).U
  // extract sign
  val a_sgn = io.a(total_width - 1)
  val b_sgn = io.b(total_width - 1)
  // extract exponent
  val a_exp = io.a(total_width - 2, mntWidth)
  val b_exp = io.b(total_width - 2, mntWidth)
  // append hidden bits
  val a_mnt = (1.U(1.W) ## io.a(mntWidth-1,0))
  val b_mnt = (1.U(1.W) ## io.b(mntWidth-1,0)) 

    // I follow the special case listed here
  // http://steve.hollasch.net/cgindex/coding/ieeefloat.html
  // if mnt is all zero
  val mntAzero = (~a_mnt(mntWidth-1, 0).orR)
  val mntBzero = (~b_mnt(mntWidth-1, 0).orR)
  // if exp is all one
  val expAone = a_exp.andR
  val expBone = b_exp.andR
  // if input is zero
  val Azero = (~a_exp.orR) & mntAzero
  val Bzero = (~b_exp.orR) & mntBzero
  val Inzero = Azero | Bzero
  // if input is inf
  val Ainf = expAone & mntAzero
  val Binf = expBone & mntBzero
  // both inf
  val IninfN = Ainf & Binf
  // one inf
  val IninfO = Ainf | Binf
  // if input is nan
  val Anan = expAone & a_mnt(mntWidth-1, 0).orR
  val Bnan = expBone & b_mnt(mntWidth-1, 0).orR
  // if one is nan
  val Innan = Anan | Bnan

  val flag_zero0 = Inzero
  // if both input is inf and is subtracting
  // if one is nan, always nan
  val flag_nan0 = Innan || IninfN
  val flag_inf0 = IninfO

  val flag_zero1 = Wire(Bool())
  val flag_zero2 = Wire(Bool())
  val flag_nan = flag_nan0
  val flag_inf = Wire(Bool())
  val flag_inf1 = Wire(Bool())
  val flag_inf2 = Wire(Bool())
  val flag_inf3 = Wire(Bool())


  val flag_zero = flag_zero0 | flag_zero1 | flag_zero2
  flag_inf := flag_inf0 | flag_inf1 | flag_inf2 | flag_inf3

  // 1. OUTPUT SIGN COMPUTATION
  val oSgn = Wire(Bool())
  oSgn :=  a_sgn ^ b_sgn

  // 2. exponent addition
  val oExp1 = a_exp.zext + b_exp.zext - bias.zext
  
  // 2.1 check special case
  flag_zero1 := (oExp1 < 0.S)
  flag_inf1 := (oExp1 >= (0.U##MAXEXP).asSInt())

  // 3. mnt mul
  val mntmul = a_mnt * b_mnt

  // 4. normalization step
  val norm_mntmul = mntmul << ~mntmul(2*p-1)
  val oExp2 = oExp1 + (mntmul(2*p-1).asUInt).zext

  // 4.1 check special cases
  flag_inf2 := (oExp2 === (0.U##MAXEXP).asSInt())

  // 5. rounding

  // wire rb0 = R & (M0|S);               // rounding bit
  val M0 = norm_mntmul(p)
  val R = norm_mntmul(p-1)
  val S = norm_mntmul(p-2, 0).orR

  val rb0 = R&(M0|S)

  val R2 = S | R 
  val rb2 = R2 & ~(oSgn)

  val rb3 = R2 & oSgn

  val rounding = io.round

  val RB = Wire(UInt())
  when(rounding === 0.U) {
    // to nearest
    RB := rb0
  }.elsewhen(rounding === 1.U){
    // towards zero
    RB := 0.U
  }.elsewhen(rounding === 2.U){
    // towards postive inf
    RB := rb2
  }.otherwise{
    // towards neg inf
    RB := rb3
  }

  val rounded_mnt = norm_mntmul(2*p-1, p) +& RB

  // normalize rounding
  val norm_rounded_mnt = Mux(rounded_mnt(p), rounded_mnt(p, 1), rounded_mnt(p-1, 0))
  // adjust exponent
  val oExp3 = oExp2 + (rounded_mnt(p).asUInt()).zext
  // special cases
  flag_inf3 := (oExp3 === (0.U##MAXEXP).asSInt())
  flag_zero2 := (oExp3 < 0.S)

  val cond = flag_nan ## flag_inf ## flag_zero
  val o_mnt = Wire(UInt((p).W))
  val o_exp4 = Wire(UInt(expWidth.W))
  val o_Sgn = Wire(Bool())
  when(cond === 0.U){
      o_mnt := norm_rounded_mnt(p-2, 0)
      o_exp4 := oExp3(expWidth-1, 0).asUInt()
      o_Sgn := oSgn
  }.elsewhen(cond === 1.U){
      o_mnt := 0.U(p.W)
      o_exp4 := 0.U(expWidth.W)
      o_Sgn := oSgn
  }.elsewhen(cond === 2.U){
      o_mnt := 0.U(p.W)
      o_exp4 := MAXEXP
      o_Sgn := oSgn
  }.elsewhen(cond >= 4.U){
      o_mnt := 3.U(2.W) ## 0.U((p-2).W)
      o_exp4 := MAXEXP
      o_Sgn := 0.B
  }.otherwise{
      o_mnt := Fill(p, 1.U)
      o_exp4 := MAXEXP
      o_Sgn := oSgn
  }
  io.o := (o_Sgn) ## o_exp4 ## o_mnt(p-2,0)

}
