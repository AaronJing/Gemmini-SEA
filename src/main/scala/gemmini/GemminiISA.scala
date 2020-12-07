package gemmini

import chisel3._

object GemminiISA {
  // funct values
  val CONFIG_CMD = 0.U
  val LOAD2_CMD = 1.U
  val LOAD_CMD = 2.U
  val STORE_CMD = 3.U
  val COMPUTE_AND_FLIP_CMD = 4.U
  val COMPUTE_AND_STAY_CMD = 5.U
  val PRELOAD_CMD = 6.U
  val FLUSH_CMD = 7.U

  val LOOP_WS = 8.U
  val LOOP_WS_CONFIG_BOUNDS = 9.U
  val LOOP_WS_CONFIG_ADDRS_AB = 10.U
  val LOOP_WS_CONFIG_ADDRS_DC = 11.U
  val LOOP_WS_CONFIG_STRIDES_AB = 12.U
  val LOOP_WS_CONFIG_STRIDES_DC = 13.U

  val LOAD3_CMD = 14.U

  // rs1[2:0] values
  val CONFIG_EX = 0.U
  val CONFIG_LOAD = 1.U
  val CONFIG_STORE = 2.U
  val CONFIG_IM2COL = 3.U

  //==========================================================================
  // cisc-gemmini opcodes
  //==========================================================================
  // TODO the numbers here overlap with the LOOP_WS commands
  val CISC_CONFIG  = 10.U(7.W) // same as COMPUTE_AND_FLIP
  val ADDR_AB      = 11.U(7.W)
  val ADDR_CD      = 12.U(7.W)
  val SIZE_MN      = 13.U(7.W)
  val SIZE_K       = 14.U(7.W)
  val RPT_BIAS     = 15.U(7.W)
  val RESET        = 16.U(7.W)
  val COMPUTE_CISC = 17.U(7.W)

  //==========================================================================
  // dataflow configuration
  //==========================================================================
  val GARBAGE_ADDR      = "hffffffff".U(32.W)
}
