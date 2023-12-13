package sea_fp_lib

import chisel3._
import chisel3.util._

class samesignedFPadder(expWidth: Int, mntWidth: Int, no_round_optimization: Boolean) extends Module with adder_intf
{


    val ioa =           IO(Input(UInt((expWidth + mntWidth + 1).W)))
    val iob =           IO(Input(UInt((expWidth + mntWidth + 1).W)))
    val ioop =          IO(Input(Bool()))
    val ioflag_inf2 =   IO(Input(Bool()))
    val ioo_sgn =       IO(Output(UInt(1.W)))
    val iocond =        IO(Output(UInt(3.W)))
    val ioo_exp2 =      IO(Output(UInt(expWidth.W)))
    val ionorm_sum =    IO(Output(UInt((mntWidth+4).W)))
    val ioflag_zero2 =  IO(Output(Bool()))

  val total_width = expWidth+mntWidth+1
  val p = mntWidth + 1
  val bias = (1 << (expWidth - 1)) - 1
  val MAXEXP = ((1 << expWidth) - 1).U
  // extract sign
  val a_sgn = ioa(total_width - 1)
  val b_sgn = iob(total_width - 1)
  // extract exponent
  val a_exp = ioa(total_width - 2, mntWidth)
  val b_exp = iob(total_width - 2, mntWidth)
  // append hidden bits
  val a_mnt = (1.U(1.W) ## ioa(mntWidth-1,0))
  val b_mnt = (1.U(1.W) ## iob(mntWidth-1,0)) 

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
  val Inzero = Azero & Bzero
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
  val flag_zero2 = 0.B
  val flag_nan = flag_nan0
  val flag_inf = Wire(Bool())
  val flag_inf1 = Wire(Bool())
  val flag_inf2 = ioflag_inf2


  val flag_zero = flag_zero0 | flag_zero1 | flag_zero2
  flag_inf := flag_inf0 | flag_inf1 | flag_inf2

  // 1. EXPONENT SUBTRACTION
  // bitwdith of diff_exp is now 0 after zero extend
  val diff_exp = a_exp.zext - b_exp.zext
  // check if bexp greater than aexp
  val alb_exp = diff_exp < 0.S
  // magnitude of exp
  val diff_exp_mag = Mux(alb_exp, -diff_exp, diff_exp).asUInt
  // get dominate exp
  val o_exp1 = Mux(alb_exp, b_exp, a_exp)
  // swap mantissa
  val a_mnts = Mux(alb_exp, b_mnt, a_mnt)
  val b_mnts = Mux(alb_exp, a_mnt, b_mnt)

  
  val o_exp_add = Wire(UInt(expWidth.W))
  if (!no_round_optimization){
    //https://pages.cs.wisc.edu/~markhill/cs354/Fall2008/notes/flpt.apprec.html
    //    1.XXXXXXXXXXXXXXXXXXXXXXX   0   0   0
    //    ^         ^                 ^   ^   ^
    //    |         |                 |   |   |
    //    |         |                 |   |   -  sticky bit (s)
    //    |         |                 |   -  round bit (r)
    //    |         |                 -  guard bit (g)
    //    |         -  23 bit mantissa from a representation
    //    -  hidden bit





    // Hassaan's implementation
    // total bitwidth = p + (p+1)
    //val shifted_b_mnts_2pw = ((b_mnts & Fill(p, ~diff_exp_mag(expWidth-1, 5).orR)) ## 0.U((p+1).W)) >> diff_exp_mag(4,0)

    // my implementation 1, that hidden bit of shifted is at the sticky bit of unshifted
    // total bitwidth = p + (p+1)
    //    1.XXXXXXXXXXXXXXXXXXXXXXX   0   0   0
    //                                        1.XXXXXXXXXXXXXXXXXXXXXXX   0   0   0
    //val shifted_b_mnts_2pw = (b_mnts ## 0.U((p+2).W)) >> diff_exp_mag

    // my implementation 2, that hidden bit of shifted is at the round bit of unshifted
    // total bitwidth = p + (p+2)
    //    1.XXXXXXXXXXXXXXXXXXXXXXX   0   0   0
    //                                    1.XXXXXXXXXXXXXXXXXXXXXXX   0   0   0
    val shifted_b_mnts_2pw = (b_mnts ## 0.U((p+1).W)) >> diff_exp_mag
    // bitwidth = p 
    val shifted_b_mnts = shifted_b_mnts_2pw(2 * p, p+1)
    // rounding unit
    val G1 = shifted_b_mnts_2pw(p)
    val R1 = shifted_b_mnts_2pw(p - 1)
    //val S1 = shifted_b_mnts_2pw(p - 2, 0).orR || (diff_exp_mag(expWidth - 1, 5).orR && !Azero && !Bzero)
    val S1 = shifted_b_mnts_2pw(p - 2, 0).orR || (diff_exp_mag > ((mntWidth+1)*2+2).U)
    // 2. COMPLEMENTING A IF IT IS SUBTRACTION
    val complemented_a_mnts = a_mnts 
    // 3. PERFORM ADDITION OR SUBTRACTION
    // wire [p-1+1:-3] Sum1 = 	{Op_perf, complemented_a_mnts, {3{Op_perf}}} + {shifted_b_mnts, G1, R1, S1} + Op_perf;
    // total bits p+4
    // carry bit + (hidden and mnts) + GRS bits
    // the last Op_perf is from two's complementing
    val sum1 = Cat(0.U(1.W), complemented_a_mnts, Fill(3, 0.U(1.W))) +& Cat(shifted_b_mnts, G1, R1, S1) 
    // get the most significant bit of sum1, it is carry when addition and Sign when subtraction
    val carrySignBit = sum1(p+3)

    flag_zero1 := (sum1 === 0.U)
    // 4. NORMALIZING
    // if it is addition, we need to shift the result right by 1 if there is carry 
    val norm_sum_add = Mux(carrySignBit.asBool(), sum1(p+3,2) ## (sum1(1)|sum1(0)), sum1(p+2, 0))
    // adjusting exponent 
    o_exp_add := Mux(carrySignBit.asBool(), o_exp1 + 1.U, o_exp1)
    // if it is infinite after adjusting the number
    flag_inf1 := (o_exp_add >= MAXEXP) 

    // epilogue of normalizing
    // p+3 bits
    val norm_sum = norm_sum_add

    val o_exp2 = o_exp_add

    val o_sgn = a_sgn

    val cond = flag_nan ## flag_inf ## flag_zero 
    ioflag_zero2 := flag_zero2
    iocond := cond
    assert(a_sgn === b_sgn)
    ioo_sgn := a_sgn
    ioo_exp2 := o_exp_add
    ionorm_sum := norm_sum_add
  } else {
    val shifted_b_mnts = b_mnts >> diff_exp_mag
    // bitwidth = p 

    // 2. COMPLEMENTING A IF IT IS SUBTRACTION
    val complemented_a_mnts = a_mnts 
    // 3. PERFORM ADDITION OR SUBTRACTION

    // total bits p+1
    // the last Op_perf is from two's complementing
    val sum1 = 0.U ## complemented_a_mnts +& shifted_b_mnts
    //val sum1 = 0.U ## shifted_b_mnts
    // val sum1 = 0.U((p+1).W)
    // get the most significant bit of sum1, it is carry when addition and Sign when subtraction
    val carrySignBit = sum1(p)

    flag_zero1 := (sum1 === 0.U)
    // 4. NORMALIZING
    // if it is addition, we need to shift the result right by 1 if there is carry 
    val norm_sum_add = Mux(carrySignBit.asBool(), sum1(p, 1), sum1(p-1, 0))
    // adjusting exponent 
    o_exp_add := Mux(carrySignBit.asBool(), o_exp1 + 1.U, o_exp1)
    // if it is infinite after adjusting the number
    flag_inf1 := (o_exp_add >= MAXEXP) 

    // epilogue of normalizing
    // p+3 bits
    val norm_sum = norm_sum_add ## 0.U(3.W)

    val o_exp2 = o_exp_add

    val o_sgn = a_sgn

    val cond = flag_nan ## flag_inf ## flag_zero 
    ioflag_zero2 := flag_zero2
    iocond := cond
    ioo_sgn := o_sgn
    ioo_exp2 := o_exp_add
    // ioo_exp2 := 0.U
    ionorm_sum := norm_sum_add ## 0.U(3.W)
  }
}
