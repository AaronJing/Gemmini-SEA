
package sea_fp_lib
import chisel3._
import chisel3.util._

trait MantMulInterface extends Module {
  def mntWidth : Int 
  val io = IO(new Bundle {
    // with hidden bits -> mntWidth + 1
       val a = Input(UInt((mntWidth+1).W)) 
       val b = Input(UInt((mntWidth+1).W)) 
       val c = Output(UInt(((mntWidth+1)*2).W)) 
      }
  )
}
//import Chisel._
// not included hidden bit
case class MantMulMIT(mntWidth: Int) extends Module with MantMulInterface
{
  val lgsz = log2Ceil(mntWidth)
  // get log number
  val lgA = io.a(mntWidth-1, 0)
  val lgB = io.b(mntWidth-1, 0)
  val mantSumWithCarry = lgA+&lgB
  val Carry = mantSumWithCarry(mntWidth)
  val mantSum = mantSumWithCarry(mntWidth-1, 0)
  val extendedmantSum = Cat(Cat(1.U,mantSum), 0.U((mntWidth+1).W)) >> (~Carry)
  io.c := extendedmantSum

}
case class MantMulMBM(mntWidth: Int) extends Module with MantMulInterface
{
  val lgA = io.a(mntWidth-1, 0)
  val lgB = io.b(mntWidth-1, 0)
  val UncorrmantSumWithCarry = lgA+&lgB
  val Carry = UncorrmantSumWithCarry(mntWidth)
  val corrTerm = 10.U(7.W) >> Carry
  val CorrmantSum = if (mntWidth+1 == 8) {
    val UpdatedPart = UncorrmantSumWithCarry(mntWidth-1, mntWidth-7) +& corrTerm
    UpdatedPart
  } else if (mntWidth+1 > 8) {
    val UpdatedPart = UncorrmantSumWithCarry(mntWidth-1, mntWidth-7) +& corrTerm
    Cat(UpdatedPart, UncorrmantSumWithCarry(mntWidth-7-1,0))
  } else  {
    val UpdatedPart = UncorrmantSumWithCarry(mntWidth-1, 0) +& corrTerm(6, 7-mntWidth)
    UpdatedPart
  }
  val CornerCase = Carry & CorrmantSum(mntWidth)
  val mantSum = Mux(CornerCase, Cat(0.U(1.W), UncorrmantSumWithCarry(mntWidth-1,0)), CorrmantSum)
  val extendedmantSum = Cat(Seq(mantSum(mntWidth), ~mantSum(mntWidth), mantSum(mntWidth-1,0), 0.U((mntWidth+1).W))) >> ~Carry
  io.c := extendedmantSum
}
case class MantMulREALM(mntWidth: Int) extends Module with MantMulInterface
{
  val patchLen = 8
  val lgsz = log2Ceil(mntWidth)
  assert(patchLen == 8)
  val bitsnumPerPatch = log2Ceil(patchLen)

  // get log number
  // Without hiddden bits
  val lgA = io.a(mntWidth-1, 0)
  val lgB = io.b(mntWidth-1, 0)
  // uncorrected mant sum
  val UncorrmantSumWithCarry = lgA+&lgB
  val Carry = UncorrmantSumWithCarry(mntWidth)
  // REALM lookUp Table method
  // bitsnumPerPatch <= mntWidth
  val lookup_X = lgA(mntWidth-1,mntWidth-1-bitsnumPerPatch)
  val lookup_Y = lgB(mntWidth-1,mntWidth-1-bitsnumPerPatch)
  // lookup table
  val lookupT = VecInit(0.U(4.W), 1.U(4.W),  1.U(4.W),  2.U(4.W),  2.U(4.W),  3.U(4.W), 3.U(4.W), 2.U(4.W),
                        1.U(4.W), 2.U(4.W),  4.U(4.W),  5.U(4.W),  7.U(4.W),  8.U(4.W), 8.U(4.W), 3.U(4.W),
                        1.U(4.W), 4.U(4.W),  6.U(4.W),  9.U(4.W), 11.U(4.W), 12.U(4.W), 8.U(4.W), 3.U(4.W),
                        2.U(4.W), 5.U(4.W),  9.U(4.W), 12.U(4.W), 14.U(4.W), 11.U(4.W), 7.U(4.W), 2.U(4.W),
                        2.U(4.W), 7.U(4.W), 11.U(4.W), 14.U(4.W), 12.U(4.W),  9.U(4.W), 5.U(4.W), 2.U(4.W),
                        3.U(4.W), 8.U(4.W), 12.U(4.W), 11.U(4.W),  9.U(4.W),  6.U(4.W), 4.U(4.W), 1.U(4.W),
                        3.U(4.W), 8.U(4.W),  8.U(4.W),  7.U(4.W),  5.U(4.W),  4.U(4.W), 2.U(4.W), 1.U(4.W),
                        2.U(4.W), 3.U(4.W),  3.U(4.W),  2.U(4.W),  2.U(4.W),  1.U(4.W), 1.U(4.W), 0.U(4.W))
  val lookupT_out = lookupT(Cat(lookup_X, lookup_Y))
  val corrTerm = Cat(Cat(0.U(2.W), lookupT_out), 0.U(1.W)) >> Carry 


  val CorrmantSum = UncorrmantSumWithCarry(mntWidth-1, 0) +& corrTerm
  val CornerCase = Carry & CorrmantSum(mntWidth)
  val mantSum = Mux(CornerCase, Cat(0.U(1.W), UncorrmantSumWithCarry(mntWidth-1, 0)),CorrmantSum)
  val extendedmantSum = Cat(Cat(~mantSum(mntWidth), mantSum(mntWidth-1, 0)), 0.U((mntWidth+1).W)) >> (~Carry)
  io.c := extendedmantSum

}
// approx 0 MIT
// approx 1 MBM
// approx 2 REALM work in progress
class ApproxMulFN(expWidth: Int, mntWidth: Int, approx: Int) extends Module
{
  val io = IO(new Bundle {
       val a = Input(UInt((expWidth + mntWidth + 1).W)) 
       val b = Input(UInt((expWidth + mntWidth + 1).W)) 
       val c = Output(UInt((expWidth + mntWidth + 1).W)) 
      }
    )
  val sign_a = io.a(expWidth+mntWidth)
  val sign_b = io.b(expWidth+mntWidth)
  val exp_a = io.a(expWidth + mntWidth - 1, mntWidth)
  val exp_b = io.b(expWidth + mntWidth - 1, mntWidth)
  val mnt_a = io.a(mntWidth-1, 0)
  val mnt_b = io.b(mntWidth-1, 0)
  val aZero = ~exp_a.orR 
  val bZero = ~exp_b.orR 
  val inZero = aZero | bZero
  val aInf = exp_a.andR
  val bInf = exp_b.andR
  val inInf = aInf | bInf

  val aNan = aInf & mnt_a.orR
  val bNan = bInf & mnt_b.orR
  val inNan = aNan | bNan


  // exception from input side
  // zero
  val flag_zero1 = inZero & ~inInf
  //val flag_zero1 = 1.B
  // nan
  val flag_nan = inZero & inInf | inNan
  // inf
  val flag_inf1 = ~inZero & inInf

  // sign computation
  val oSign = sign_a ^ sign_b
  // exp computtion
  val oExp1 = Cat(0.U, exp_a).zext + Cat(0.U, exp_b).zext - Cat(0.U, ((scala.math.pow(2, expWidth-1).toInt-1).U)).zext
  //val oExp1 = exp_a.zext + exp_b.zext - ((scala.math.pow(2, expWidth-1).toInt-1).U).zext
  val flag_zero2 = oExp1 <= 0.S 
  val flag_inf2 = oExp1 > ((scala.math.pow(2, expWidth).toInt-1).U).zext
  // mantisaa computation
  val mnt_a_h = Cat(1.U, mnt_a)
  val mnt_b_h = Cat(1.U, mnt_b)
 
 //val mntMulUnit: Module {def io : { def a : UInt; def b : UInt; def C : UInt}} = if (approx == 0) { 
 //  Module(new MantMulMIT(mntWidth))
 //} else if (approx == 1) {
 //  Module(new MantMulMBM(mntWidth))
 //} else if (approx == 2) {
 //  Module(new MantMulREALM(mntWidth))
 //}
  //val mntMulResult = WireInit(0.U((mntWidth+1)*2))
  val mntMulResult = if (approx == 0) {
    val mntMulUnit = Module(new MantMulMIT(mntWidth))
    mntMulUnit.io.a := mnt_a_h
    mntMulUnit.io.b := mnt_b_h
    mntMulUnit.io.c
  } else if (approx == 1) {
    val mntMulUnit = Module(new MantMulMBM(mntWidth))
    mntMulUnit.io.a := mnt_a_h
    mntMulUnit.io.b := mnt_b_h
    mntMulUnit.io.c
  
  } else{
    val mntMulUnit = Module(new MantMulREALM(mntWidth))
    mntMulUnit.io.a := mnt_a_h
    mntMulUnit.io.b := mnt_b_h
    mntMulUnit.io.c
  
  }
  // val mntMulUnit = Module(MantMulREALM(mntWidth))
  //  mntMulUnit.io.a := mnt_a_h
  //  mntMulUnit.io.b := mnt_b_h
  //  val mntMulResult = mntMulUnit.io.c
  // normalization
  val normMnt = mntMulResult << (~mntMulResult(mntWidth*2+1))
  // adjust exponent 
  val oExp2 = oExp1(expWidth-1, 0).asUInt + mntMulResult(mntWidth*2+1).asUInt
  printf(s"oExp1 = %x", oExp1)
  // check new exponent exception
  val flag_inf3 = oExp2.andR
  // No rounding
  val rounded_mnt = normMnt(mntWidth*2, mntWidth+1)
  // handle exception
  val flag_zero = flag_zero1 | flag_zero2
  val flag_inf = flag_inf1 | flag_inf2 | flag_inf3

  val f_oExp = WireInit(oExp2)
  val f_oMnt = WireInit(rounded_mnt)
  when((~flag_nan)&(~flag_inf)&(~flag_zero)){
    f_oExp := oExp2 
    f_oMnt := rounded_mnt
  }.elsewhen(flag_nan){
    f_oExp := (scala.math.pow(2, expWidth).toInt-1).U
    f_oMnt := Cat(1.U(1.W), 0.U((mntWidth-1).W))
  }.elsewhen(flag_inf & (~flag_zero)){
    f_oExp := (scala.math.pow(2, expWidth).toInt-1).U
    f_oMnt := 0.U(mntWidth.W)
  }.elsewhen(flag_zero){
    f_oExp := 0.U(expWidth.W)
    f_oMnt := 0.U(mntWidth.W)
  }.otherwise{
    f_oExp := (scala.math.pow(2, expWidth).toInt-1).U
    f_oMnt := 5.U(mntWidth.W)
  }
  // final concatenation
  io.c := Cat(oSign& (~flag_nan), Cat(f_oExp, f_oMnt))


}

