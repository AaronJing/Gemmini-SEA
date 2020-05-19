/*
package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import freechips.rocketchip.config.Parameters
//have counter 1st 2nd 3rd... row

// TODO this is almost a complete copy of LoadController. We should combine them into one class
// TODO deal with errors when reading scratchpad responses
class StoreController[T <: Data : Arithmetic](config: GemminiArrayConfig[T], coreMaxAddrBits: Int, local_addr_t: LocalAddr)
                     (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmdWithDeps(rob_entries)))

    // val dma = new ScratchpadWriteMemIO(sp_banks, sp_bank_entries, acc_rows)
    val dma = new ScratchpadWriteMemIO(local_addr_t)

    val completed = Decoupled(UInt(log2Up(rob_entries).W))

    val busy = Output(Bool())
  })

  val waiting_for_command :: waiting_for_dma_req_ready :: sending_rows :: Nil = Enum(3)
  val control_state = RegInit(waiting_for_command)

  val stride = RegInit((sp_width / 8).U(coreMaxAddrBits.W))
  val block_rows = meshRows * tileRows
  val row_counter = RegInit(0.U(log2Ceil(block_rows).W))

  val cmd = Queue(io.cmd, st_queue_length) //all the rocc commends into store controller
  val vaddr = cmd.bits.cmd.rs1
  val localaddr = cmd.bits.cmd.rs2.asTypeOf(local_addr_t)
  val config_stride = cmd.bits.cmd.rs2
  // implement pool_stride here
  val mstatus = cmd.bits.cmd.status

  val localaddr_plus_row_counter = localaddr + row_counter

  io.busy := cmd.valid
  //Seah: count matrix multiplication time
  val (mvout_time, mvout_time_wrap) = Counter(io.busy, 1024)
  val mvout_counter = WireInit(mvout_time)
  dontTouch(mvout_counter)

  val DoConfig = cmd.bits.cmd.inst.funct === CONFIG_CMD
  val DoStore = !DoConfig // TODO change this if more commands are added

  cmd.ready := false.B

  // Command tracker instantiation
  val nCmds = 2 // TODO make this a config parameter

  val deps_t = new Bundle {
    val rob_id = UInt(log2Up(rob_entries).W)
  }

  val cmd_tracker = Module(new DMAReadCommandTracker(nCmds, block_rows, deps_t))

  // DMA IO wiring
  io.dma.req.valid := (control_state === waiting_for_command && cmd.valid && DoStore && cmd_tracker.io.alloc.ready) ||
    control_state === waiting_for_dma_req_ready ||
    (control_state === sending_rows && row_counter =/= 0.U)
  io.dma.req.bits.vaddr := vaddr + row_counter * stride
  io.dma.req.bits.laddr := localaddr_plus_row_counter
  //implement pool stride here
  io.dma.req.bits.status := mstatus

  // Command tracker IO
  cmd_tracker.io.alloc.valid := control_state === waiting_for_command && cmd.valid && DoStore
  cmd_tracker.io.alloc.bits.bytes_to_read := block_rows.U
  cmd_tracker.io.alloc.bits.tag.rob_id := cmd.bits.rob_id
  cmd_tracker.io.request_returned.valid := io.dma.resp.fire() // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.cmd_id := io.dma.resp.bits.cmd_id // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.bytes_read := 1.U
  cmd_tracker.io.cmd_completed.ready := io.completed.ready

  val cmd_id = RegEnableThru(cmd_tracker.io.alloc.bits.cmd_id, cmd_tracker.io.alloc.fire()) // TODO is this really better than a simple RegEnable?
  io.dma.req.bits.cmd_id := cmd_id

  io.completed.valid := cmd_tracker.io.cmd_completed.valid
  io.completed.bits := cmd_tracker.io.cmd_completed.bits.tag.rob_id

  /*
  io.completed.valid := false.B
  io.completed.bits := cmd.bits.rob_id
  */


  // Row counter
  when (io.dma.req.fire()) {
    row_counter := wrappingAdd(row_counter, 1.U, block_rows)
  }

  // Control logic
  switch (control_state) {
    is (waiting_for_command) {
      when (cmd.valid) {
        when(DoConfig) {
          stride := config_stride
          cmd.ready := true.B
        }

        .elsewhen(DoStore && cmd_tracker.io.alloc.fire()) {
          control_state := Mux(io.dma.req.fire(), sending_rows, waiting_for_dma_req_ready)
        }
      }
    }

    is (waiting_for_dma_req_ready) {
      when (io.dma.req.fire()) {
        control_state := sending_rows
      }
    }

    is (sending_rows) {
      val last_row = row_counter === 0.U || (row_counter === (block_rows-1).U && io.dma.req.fire())

      // io.completed.valid := last_row

      // when (io.completed.fire()) {
      when (last_row) {
        control_state := waiting_for_command
        cmd.ready := true.B
      }
    }
  }
}

*/
