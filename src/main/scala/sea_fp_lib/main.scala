package sea_fp_lib

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

class FPadderTOP(expWidth: Int, mntWidth: Int, no_round: Boolean, unsigned: Boolean) extends Module
{
  val io = IO(new Bundle {
    val a = Input(UInt((expWidth + mntWidth + 1).W))
    val b = Input(UInt((expWidth + mntWidth + 1).W))
    val o = Output(UInt((expWidth + mntWidth + 1).W))
    val op = Input(Bool())
    val round = Input(UInt(2.W))
  })
  val FPadderModule = Module(new FPadder(expWidth, mntWidth, no_round, unsigned))
  val a  =        RegInit(0.U((expWidth + mntWidth + 1).W))
  val b  =        RegInit(0.U((expWidth + mntWidth + 1).W))
  val op  =        RegInit(0.U(1.W))
  val round =       RegInit(0.U(2.W))
  a := io.a
  b := io.b 
  op := io.op 
  round := io.round 
  FPadderModule.io.a := a
  FPadderModule.io.b := b
  FPadderModule.io.op := op
  FPadderModule.io.round := round
  val o_t = RegInit(0.U((expWidth + mntWidth + 1).W))
  o_t := FPadderModule.io.o
  io.o := o_t

}
object VerilogMain extends App {
  val mnt =         if (args(1) != null) args(1).toInt else 23
  val exp =         if (args(0) != null) args(0).toInt else 8
  def stringToBoolean(input: String): Boolean = input match {
  case "0" => false
  case "1"   => true
  case _ => false
  }
  val nr =         if (args(0) != null) stringToBoolean(args(2)) else false
  val un =         if (args(1) != null) stringToBoolean(args(3)) else false
  (new ChiselStage).emitVerilog(new FPadderTOP(exp, mnt, nr, un),Array("--target-dir", "generated"))
}
