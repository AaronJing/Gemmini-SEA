// See README.md for license details.
package gemmini

import chisel3._
import chisel3.util._

// TODO update documentation
/**
  * A PE implementing a MAC operation. Configured as fully combinational when integrated into a Mesh.
  * @param width Data width of operands
  */
class PE_sea_ws[T <: Data](samesignedadder: Boolean, approx: Boolean, noround: Boolean, inputType: T, outputType: T, accType: T, df: Dataflow.Value, max_simultaneous_matmuls: Int)
                   (implicit ev: Arithmetic[T]) extends Module { // Debugging variables
  import ev._

  val io = IO(new Bundle {
    val in_a = Input(inputType)
    val in_b = Input(outputType)
    val in_b1 = Input(outputType)
    val in_d = Input(outputType)
    val out_a = Output(inputType)
    val out_b = Output(outputType)
    val out_b1 = Output(outputType)
    val out_c = Output(outputType)

    val in_control = Input(new PEControl(accType))
    val out_control = Output(new PEControl(accType))

    val in_id = Input(UInt(log2Up(max_simultaneous_matmuls).W))
    val out_id = Output(UInt(log2Up(max_simultaneous_matmuls).W))

    val in_last = Input(Bool())
    val out_last = Output(Bool())

    val in_valid = Input(Bool())
    val out_valid = Output(Bool())

    val bad_dataflow = Output(Bool())
  })

  val cType = if (df == Dataflow.WS) inputType else accType

  val a  = io.in_a
  val b  = io.in_b
  val d  = io.in_d
  val c1 = Reg(cType)
  val c2 = Reg(cType)
  val dataflow = io.in_control.dataflow
  val prop  = io.in_control.propagate
  val shift = io.in_control.shift
  val id = io.in_id
  val last = io.in_last
  val valid = io.in_valid

  io.out_a := a
  io.out_control.dataflow := dataflow
  io.out_control.propagate := prop
  io.out_control.shift := shift
  io.out_id := id
  io.out_last := last
  io.out_valid := valid

  val last_s = RegEnable(prop, valid)
  val flip = last_s =/= prop
  val shift_offset = Mux(flip, shift, 0.U)

  // Which dataflow are we using?
  val OUTPUT_STATIONARY = Dataflow.OS.id.U(1.W)
  val WEIGHT_STATIONARY = Dataflow.WS.id.U(1.W)

  // Is c1 being computed on, or propagated forward (in the output-stationary dataflow)?
  val COMPUTE = 0.U(1.W)
  val PROPAGATE = 1.U(1.W)


  io.bad_dataflow := false.B
  // for weight stationary it is b+a*c 
  val b1 = io.in_b1
  val mul_sign = a.isNeg ^ Mux(prop===PROPAGATE, c2.asTypeOf(inputType), c1.asTypeOf(inputType)).isNeg
  val NotSameSign = b.isNeg ^ mul_sign
  // swapping


  // assert(b.isNeg =/= ~b1.isNeg)
  val op2 = Mux(NotSameSign, b1, b) 
  val mac_result = op2.seaMac(approx, noround, samesignedadder, a, Mux(prop===PROPAGATE, c2.asTypeOf(inputType), c1.asTypeOf(inputType)))
  io.out_b1 := Mux(NotSameSign, b, b1)
  val out_b1 = io.out_b1
  io.out_b := mac_result
  when(prop === PROPAGATE) {
    io.out_c := c1
    c1 := d
  }.otherwise {
    io.out_c := c2
    c2 := d
  }
  when (!valid) {
   c1 := c1
   c2 := c2
  }
}

