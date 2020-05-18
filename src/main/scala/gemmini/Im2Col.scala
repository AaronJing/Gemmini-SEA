package gemmini

import chisel3._
import chisel3.util._
import Util._

class Im2ColReadReq[T <: Data, U <: Data](config: GemminiArrayConfig[T, U]) extends Bundle {

  val addr = new LocalAddr(config.sp_banks, config.sp_bank_entries, config.acc_banks, config.acc_bank_entries)
  //dimension
  val ocol = UInt(8.W)
  val krow = UInt(3.W)
  val icol = UInt(9.W)
  val irow = UInt(9.W)
  val stride = UInt(3.W)
  val channel = UInt(7.W)
  val row_turn = UInt(11.W)
  val kdim2 = UInt(6.W)
  val row_left = UInt(4.W)

  val im2col_cmd = Bool()
  val start_inputting = Bool() //start_inputting_a

  override def cloneType: Im2ColReadReq.this.type = new Im2ColReadReq(config).asInstanceOf[this.type]

}

class Im2ColReadResp[T <: Data, U <: Data](config: GemminiArrayConfig[T, U]) extends Bundle {

  val a_im2col = Vec(config.meshColumns * config.tileColumns, config.inputType)
  val im2col_end = Bool()
  val im2col_turn = UInt(9.W)
  val row_turn = UInt(7.W)

  override def cloneType: Im2ColReadResp.this.type = new Im2ColReadResp(config).asInstanceOf[this.type]

}

class Im2Col[T <: Data, U <: Data](config: GemminiArrayConfig[T, U]) extends Module {
  import config._

  val block_size = meshRows*tileRows

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new Im2ColReadReq(config))) // from ExecuteController
    val resp = Decoupled(new Im2ColReadResp(config)) // to ExecuteController

    /*
    val req = new Bundle {
      val valid = Input(Bool())
      val ready = Output(Bool())
      val bits = Input(new Im2ColReadReq(config))
    }

    val resp = new Bundle {
      val valid = Output(Bool())
      val ready = Input(Bool())
      val bits = Output(new Im2ColReadResp(config))
    }
    */

    val sram_reads = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width)) // from Scratchpad
  })
  /*
    object State extends ChiselEnum {
      val idle, busy = Value
    }
    import State._

    val state = RegInit(idle)
  */
  val req = Reg(new Im2ColReadReq(config))

  //when (io.req.ready && io.req.valid) {
  when(io.req.ready){
    io.sram_reads(req.addr.sp_bank()).req.valid := true.B
    io.sram_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()
  }

  val start_inputting_A = req.start_inputting

  val ocol = io.req.bits.ocol
  val kdim = io.req.bits.krow
  val stride = io.req.bits.stride
  val icol = io.req.bits.icol
  val irow = io.req.bits.irow
  val channel_total = io.req.bits.channel
  val channel_counter = RegInit(0.U(7.W))
  val channel = Mux(channel_counter > block_size.U, block_size.U, channel_counter) //channel > 16, then do DIM of channel first until channel < DIM

  val filter_dim2 = io.req.bits.kdim2
  val im2col_width = channel * filter_dim2
  //val output_dim = ocol*orow
  //Todo: ocol*orow - block_done*block_size
  //Todo: row_turn
  //Todo: turn



  // first do simple case: channel 4, output width: 3x3
  val row_counter = RegInit(0.U(10.W)) // im2col_width
  val column_counter = RegInit(0.U((row_counter.getWidth).W))
  val window_row_counter = RegInit(0.U((row_counter.getWidth).W))
  val window_start_counter = RegInit(0.U((row_counter.getWidth).W))
  val im2col_data = RegInit(0.S.asTypeOf(Vec(block_size, inputType)))
  val valid_reg = RegInit(false.B)
  val im2col_fin_reg = RegInit(false.B) // when 16 rows are all created - to outside
  val im2col_fin = RegInit(false.B) //when reading out necessary row is done
  val sram_resp_valid = WireInit(false.B)
  val im2col_en = WireInit(false.B)
  val sram_read_req = RegInit(true.B)

  val copy_addr = RegInit(io.req.bits.addr) //spad bank address store for sync


  //how much horizonal turn we have to compute (input_channel*kernel_dim/16)
  val turn = Mux(im2col_width(3,0) === 0.U, (im2col_width >> (log2Up(block_size)).U).asUInt(), (im2col_width >> (log2Up(block_size)).U).asUInt + 1.U)

  val ch_per_turn = WireInit(0.U(5.W))


  //Seah: added for more than 16 rows of output
  //how much vertical turn we have to compute (output_dim/16)
  //val row_turn = Mux(output_dim(3,0) === 0.U, (output_dim >> (log2Up(block_size)).U).asUInt - 1.U, (output_dim >> (log2Up(block_size)).U).asUInt()) //im2col height
  val row_turn = io.req.bits.row_turn
  val row_left = io.req.bits.row_left

  when(channel === 1.U){
    ch_per_turn := 16.U
  }.elsewhen(channel === 2.U){
    ch_per_turn := 8.U
  }.elsewhen(channel === 3.U){
    ch_per_turn := 5.U
  }.elsewhen(channel === 4.U){
    ch_per_turn := 4.U
  }.elsewhen(channel === 5.U){
    ch_per_turn := 3.U
  }.elsewhen(channel >= 6.U && channel <= 8.U){
    ch_per_turn := 2.U
  }.otherwise{ch_per_turn := 1.U}


  //im2col state machine
  val nothing_to_do :: waiting_for_im2col :: preparing_im2col :: doing_im2col :: im2col_done :: Nil = Enum(5)
  val im2col_state = RegInit(nothing_to_do)

  val im2col_turn = RegInit(turn)
  val turn_done = turn - im2col_turn
  val modulo_im2col_turn = RegInit(turn_done)
  val im2col_fin_reset = RegInit(false.B)

  val im2col_row_turn_counter = RegInit(0.U((row_turn.getWidth).W))
  //val im2col_row_counter_update = RegInit(false.B)
  val im2col_row_turn_counter_d = RegInit(im2col_row_turn_counter)
  when(im2col_state === waiting_for_im2col){
    im2col_row_turn_counter_d := im2col_row_turn_counter
  }

  val block_done = row_turn - im2col_row_turn_counter
  val modulo_block_done = RegInit(block_done*block_size.U)
  val modulo_block_save = RegInit(block_done*block_size.U)
  val modulo_row_counter = RegInit(row_counter)
  val modulo_row_increment = RegInit(false.B)


  io.resp.bits.a_im2col := im2col_data
  io.resp.valid := valid_reg
  io.resp.bits.im2col_end := im2col_fin
  io.resp.bits.im2col_turn := im2col_turn
  io.resp.bits.row_turn := block_done

  when(io.req.bits.im2col_cmd){
    im2col_en := true.B
    req := io.req.bits
  }.otherwise{im2col_en := false.B} //initialize

  val im2col_en_d = RegNext(im2col_en)

  val sram_read_signals_q = Module(new Queue(new im2colRowSignals, mem_pipeline+1,
    pipe=true))

  val sram_read_valid = io.sram_reads(req.addr.sp_bank()).req.valid && start_inputting_A
  val busy = ((im2col_state === doing_im2col) || (im2col_state === preparing_im2col) || (im2col_state === waiting_for_im2col) || (im2col_en && !im2col_en_d))  //&& sram_read_valid))// or including prepare?

  io.req.ready := busy


  val row_turn_counter_noreset = RegInit(false.B)
  val im2col_fin_pulse = RegNext(sram_read_signals_q.io.deq.bits.im2col_fin_pulse)
  val turn_count_save = RegInit(im2col_turn)
  val channel_wrap = RegInit(0.U((column_counter.getWidth).W)) // for overflowing detection
  val channel_done = ch_per_turn * turn_done + channel_wrap // how many channels were done
  val assign_data_sub = RegInit(channel_done) // to subtract for im2col_reg data assignment

  val window_start_counter_prev = RegInit(0.U((row_counter.getWidth).W)) //to store in which window_start_counter on this phase, and loaded it when the next vertical block starts
  val row_counter_saver = RegInit(0.U((column_counter.getWidth).W)) // count how much channel counted using row_counter when channel < 16
  val window_row_counter_saver = RegInit(0.U((column_counter.getWidth).W)) //save window_row_counter starting point for channel < 16
  val channel_need_reset = RegInit(false.B)

  //im2col state machine
  switch(im2col_state){
    is(nothing_to_do){
      im2col_fin_reset := false.B
      sram_read_req := false.B
      channel_need_reset := false.B
      when(im2col_en){
        channel_counter := channel_counter - block_size.U
        when(channel_counter <= block_size.U) {channel_need_reset := true.B}
        im2col_state := waiting_for_im2col
        im2col_turn := 0.U//io.req.bits.turn
        modulo_im2col_turn := 0.U
        window_start_counter := 0.U
        window_row_counter := 0.U
        row_counter_saver := 0.U //initialize?
        column_counter := 0.U
        row_counter := 0.U
        window_start_counter_prev := 0.U
        req := io.req.bits
      }
    }
    is(waiting_for_im2col){ //default state
      turn_count_save := turn
      when(im2col_turn === 0.U){
        req := io.req.bits
        im2col_turn := turn
        modulo_im2col_turn := 0.U
      }
      when(channel_need_reset){ //when there is a channel number update
        channel_counter := channel_total
        im2col_turn := turn
      }
      im2col_fin_reset := true.B//added for im2col_fin reset pulse
      //added for row overflowing
      when(!row_turn_counter_noreset){
        im2col_row_turn_counter := row_turn
        modulo_block_done := 0.U
        modulo_block_save := 0.U
      }
      sram_read_req := false.B
      copy_addr := io.req.bits.addr
      when(req.start_inputting) { //receive im2col command from instruction
        sram_read_req := true.B
        im2col_fin_reset := false.B
      }
      when(sram_read_valid){
        im2col_state := preparing_im2col // where state transition?
        channel_need_reset := false.B
      }
    }
    is(preparing_im2col){
      assign_data_sub := Mux(channel < block_size.U, turn_done * block_size.U - channel_done * channel, 0.U)
      im2col_fin_reset := false.B//added for im2col_fin reset pulse
      sram_read_req := true.B
      when(sram_resp_valid && !im2col_fin_reset && req.start_inputting){ //added start_inputting (have to check - whether to go im2col done)
        im2col_state := doing_im2col
      }
    }
    is(doing_im2col){
      when(im2col_fin_reg|| !sram_resp_valid){ //when finished
        sram_read_req := false.B
      }

      when(!sram_read_valid && !sram_resp_valid){
        when(turn_count_save - im2col_turn + 1.U === filter_dim2){ //for after finishing 16 channels
          turn_count_save := im2col_turn - 1.U
        }
        when(im2col_turn === 1.U) {
          im2col_state := im2col_done
          im2col_turn := 0.U
          modulo_block_save := modulo_block_done //save to come back
          when(im2col_row_turn_counter === 0.U){
          }.otherwise{
            im2col_row_turn_counter := im2col_row_turn_counter - 1.U // row is outer loop (finish preloaded one first)
          }
        }.elsewhen(im2col_turn === 2.U){
          modulo_block_done := modulo_block_save //have to move backward
          im2col_turn := 1.U
          sram_read_req := true.B
          im2col_state := preparing_im2col
        }.otherwise{
          modulo_block_done := modulo_block_save
          sram_read_req := true.B
          im2col_state := preparing_im2col
          im2col_turn := im2col_turn - 1.U
        }
      }
    }
    is(im2col_done){
      //starting_channel := 0.U
      im2col_fin_reset := true.B //added for im2col_fin reset pulse
      row_turn_counter_noreset := true.B
      row_counter_saver := 0.U
      modulo_row_counter := 0.U //Todo: initialize here?
      modulo_row_increment := false.B
      window_row_counter := 0.U
      window_row_counter_saver := 0.U
      //when(im2col_row_turn_counter_d =/= 0.U){
      when(im2col_row_turn_counter_d =/= 0.U && !im2col_fin) { //added !im2col_fin to sync transition
        im2col_state := waiting_for_im2col //continue doing im2col
        //im2col_row_counter_update := true.B //added for im2col_row_counter sync
      }.elsewhen(im2col_row_turn_counter_d === 0.U){
        im2col_state := nothing_to_do
        row_turn_counter_noreset := false.B //have to reset row_turn_counter
      } //waiting for new im2col
    }

  }

  io.sram_reads.foreach { sr =>
    sr.req.valid := false.B
    sr.req.bits := DontCare
    sr.resp.ready := true.B
  }



  val window_address = WireInit((0.U((row_counter.getWidth).W)))
  window_address := window_start_counter + window_row_counter

  when(im2col_turn === 1.U && im2col_fin_reg) {
    window_start_counter_prev := window_start_counter + stride //changed to stride
    //when(((1.U + block_done) * block_size.U) % ocol === 0.U){
    when(((1.U + block_done) * block_size.U) - modulo_block_done === ocol){
      window_start_counter_prev := window_start_counter + kdim + (stride - 1.U) * icol//input - output_width + 1.U
      modulo_block_done := modulo_block_done + ocol
    }
  }


  io.sram_reads(copy_addr.sp_bank()).req.valid := sram_read_req && !im2col_fin_reg
  io.sram_reads(copy_addr.sp_bank()).req.bits.addr := copy_addr.sp_row() + window_address

  sram_resp_valid := ((im2col_state === doing_im2col) || (im2col_state === preparing_im2col)) && io.sram_reads(copy_addr.sp_bank()).resp.valid && !im2col_fin_reset

  when(im2col_fin_reg && im2col_state =/= im2col_done) {
    row_counter_saver := row_counter_saver + ch_per_turn
    when(channel < block_size.U && modulo_row_increment) {
      modulo_row_counter := modulo_row_counter + kdim
      modulo_row_increment := false.B
    } //added for row_counter related modulo
    when((turn - im2col_turn + 1.U) * (block_size.U - channel * ch_per_turn) - channel * channel_wrap >= channel) {
      channel_wrap := channel_wrap + 1.U
      row_counter_saver := row_counter_saver + ch_per_turn + 1.U
    }
  }.elsewhen(im2col_state === im2col_done){
    channel_wrap := 0.U
  }

  val channel_turn = RegInit(ch_per_turn) //for channel < 16, row_counter count limit
  when(im2col_turn === 1.U){ //on the last turn
    channel_turn := filter_dim2 - channel_done //count only remainder
  }.otherwise{
    channel_turn := Mux(channel === 8.U || channel === 4.U || channel === 2.U, ch_per_turn, ch_per_turn + 1.U)
    when((channel - assign_data_sub) + (Mux(channel === 8.U || channel === 4.U || channel === 2.U, ch_per_turn, ch_per_turn + 1.U) - 1.U) * channel < block_size.U) {
      channel_turn := Mux(channel === 8.U || channel === 4.U || channel === 2.U, ch_per_turn, ch_per_turn + 1.U) + 1.U
    }
  }

  //when(column_counter === block_size.U - 1.U && row_counter === filter_dim2 - 1.U){
  when(column_counter === block_size.U - 1.U && row_counter === channel_turn - 1.U){ //changed for row-wise multiple im2col blocks
    im2col_fin_reg := true.B
  }.elsewhen(channel >= 16.U && column_counter === block_size.U - 1.U){
    im2col_fin_reg := true.B
  }.elsewhen(!start_inputting_A || im2col_fin_reset){
   im2col_fin_reg := false.B
  }.otherwise{ // when tiling, need to make it false again
    im2col_fin_reg := false.B
  }

  //val row_counter_deq = sram_read_signals_q.io.deq.bits.row_counter
  val row_counter_deq_d = RegNext(sram_read_signals_q.io.deq.bits.row_counter) //Seah: changed - 1 clock delayed version
  val column_counter_deq_d = RegNext(sram_read_signals_q.io.deq.bits.column_counter)
  val sram_req_output = io.sram_reads(copy_addr.sp_bank()).resp.bits.data.asTypeOf(Vec(block_size, inputType))
  val column_counter_last_row_block = im2col_row_turn_counter === 0.U && (column_counter === row_left - 1.U)
    //(column_counter + block_done * block_size.U === ocol * orow - 1.U)
  val column_counter_last_row_block_deq = (sram_read_signals_q.io.deq.bits.block_done === row_turn) && (column_counter_deq_d >= row_left)
  //column_counter_deq_d >= (ocol*orow - sram_read_signals_q.io.deq.bits.block_done * block_size.U)

  val req_valid = RegInit(false.B)

  val valid_count = RegInit(0.U(5.W))
  when(!req_valid){
    when(io.req.valid){
      req_valid := true.B
    }
  }.otherwise{
    when(valid_reg){
      valid_count := wrappingAdd(valid_count, 1.U, block_size.U)
      when(valid_count === block_size.U - 1.U){
        req_valid := false.B
        when(io.req.valid){req_valid := true.B}
      }.otherwise{req_valid := true.B}
    }
  }

  //when((!io.req.valid && !start_inputting_A && !sram_resp_valid) || im2col_fin_reset){
  when((!req_valid && !start_inputting_A && !sram_resp_valid) || im2col_fin_reset){
    column_counter := 0.U
    im2col_fin := false.B
    row_counter := 0.U
  }.elsewhen((!sram_resp_valid || !req_valid) && sram_read_req){ //added req_valid for modulo
    //loading previous values after one moving out
    window_start_counter := window_start_counter_prev //vertical
  }.elsewhen(sram_resp_valid && sram_read_req && (req_valid||io.req.valid)){ //starting counting & valid requesting
    when(channel === block_size.U){ // 16 channel cases first (simplest)
      row_counter := 0.U
      column_counter := wrappingAdd(column_counter, 1.U, block_size)
      when(!im2col_fin){
        when(column_counter_last_row_block || column_counter === block_size.U - 1.U){
          im2col_fin := true.B
        //}.elsewhen((column_counter + 1.U + block_done * block_size.U) % ocol === 0.U){
        }.elsewhen((column_counter + 1.U + block_done * block_size.U) - modulo_block_done === ocol){
          window_start_counter := window_start_counter + kdim + (stride - 1.U) * icol
          modulo_block_done := modulo_block_done + ocol
        }.otherwise{window_start_counter := window_start_counter + stride}
      }.otherwise { //when im2col_fin
        window_start_counter := 0.U
      }
    }.otherwise{ //when channel < 16
      column_counter := wrappingAdd(column_counter, 1.U, block_size.U, row_counter === channel_turn - 1.U)
      when(!im2col_fin){
        row_counter := wrappingAdd(row_counter, 1.U, channel_turn) //here?
        when(row_counter === channel_turn - 1.U){
          window_row_counter := window_row_counter_saver //load saved value
          when(column_counter_last_row_block || column_counter === block_size.U - 1.U) {
            im2col_fin := true.B
            row_counter := channel_turn - 1.U
            window_row_counter_saver := window_row_counter //save current window_row_counter to resume next turn
            when((channel_turn - 1.U)*channel + (channel - assign_data_sub) === block_size.U) {
              window_row_counter_saver := window_row_counter + 1.U //need to start next weight element on the next turn (fill up block_size this turn->wrapping)
            }
          //}.elsewhen((column_counter + 1.U + block_done * block_size.U) % ocol === 0.U){
          }.elsewhen((column_counter + 1.U + block_done * block_size.U) - modulo_block_done === ocol){
            window_start_counter := window_start_counter + kdim + (stride - 1.U) * icol
            modulo_block_done := modulo_block_done + ocol
          }.otherwise{window_start_counter := window_start_counter + stride}
        }.otherwise{
          //when((row_counter + 1.U + row_counter_saver) % kcol === 0.U) { //when finishing 1 weight row
          when((row_counter + 1.U + row_counter_saver) - modulo_row_counter === kdim) { //when finishing 1 weight row
            window_row_counter := window_row_counter + icol - kdim + 1.U
            modulo_row_increment := true.B
          }.otherwise{window_row_counter := window_row_counter + 1.U}
        }
      }.otherwise{ //when im2col_fin
        window_start_counter := 0.U
      }
    }
  }.elsewhen(!io.req.valid){
    column_counter := 0.U // when req.valid goes off and ended
    when(column_counter_last_row_block || column_counter === block_size.U - 1.U){
      im2col_fin := true.B
    }
    window_start_counter := 0.U
  }

  when(im2col_state =/= im2col_done) {
    when(im2col_fin_reg) {
      when(channel === block_size.U) {
        when(turn_count_save - im2col_turn + 1.U === filter_dim2) { // when 16 channels finished
          window_row_counter := 0.U
        //}.elsewhen((turn - im2col_turn + 1.U) % kcol === 0.U) { //when finishing 1 weight row
        }.elsewhen((turn - im2col_turn + 1.U) - modulo_im2col_turn === kdim) { //when finishing 1 weight row
          window_row_counter := window_row_counter + icol - kdim + 1.U //copy window_row_counter below
          modulo_im2col_turn := modulo_im2col_turn + kdim
        }.otherwise {
          window_row_counter := window_row_counter + 1.U
        }
      }.otherwise {
        when(im2col_turn === 1.U){
          window_row_counter := 0.U
        }.otherwise{window_row_counter := window_row_counter_saver}
      }
    }
  }
    /*.otherwise{
    window_row_counter := 0.U
    window_row_counter_saver := 0.U
  }*/

  val sram_req_deq_valid_d = RegNext(sram_read_signals_q.io.deq.valid) // or just deq.valid?
  val sram_channel_turn = RegNext(sram_read_signals_q.io.deq.bits.channel_wrap)
  val sram_resp_valid_deq_d = RegNext(sram_read_signals_q.io.deq.bits.sram_resp_valid)

  //making output data valid signal
  when(sram_req_deq_valid_d && sram_resp_valid_deq_d){
    when(im2col_fin_pulse){
      valid_reg := false.B
    }.elsewhen(column_counter_last_row_block_deq && valid_reg){ //if finished earlier
      //Todo: how to deal with big multiplication
      valid_reg := true.B
    }.otherwise {
      valid_reg := true.B
      when(channel < block_size.U) {
        valid_reg := false.B
      when(sram_channel_turn === 1.U){
        when(sram_read_signals_q.io.deq.bits.column_counter =/= 0.U || sram_read_signals_q.io.deq.bits.im2col_fin_pulse) {
          valid_reg := true.B
        }
      }.elsewhen(row_counter_deq_d === sram_channel_turn - 1.U){valid_reg := true.B}
      }
    }
  }.otherwise{
    valid_reg := false.B
  }

  val sram_read_im2col_fin_d = RegNext(sram_read_signals_q.io.deq.bits.im2col_fin)
  val assign_data = row_counter_deq_d * channel//(row_counter_deq_d + channel_done)

  val assign_sub = RegNext(sram_read_signals_q.io.deq.bits.assign_sub)

  val assign_leftover = WireInit(0.U(5.W))
  when(channel < block_size.U && im2col_turn === 1.U){ // on the last turn, empty columns
    assign_leftover := (turn_done + 1.U) * block_size.U - channel * filter_dim2 // no need to compute these
  }.otherwise{assign_leftover := 0.U} //ToDo: initialize? Register?

  when(!sram_read_im2col_fin_d && sram_req_deq_valid_d && sram_resp_valid_deq_d){
    when(channel === block_size.U){
      im2col_data := sram_req_output
    }.otherwise{
      for(i <- 0 until block_size){
        when(i.U < channel){
          when(i.U + assign_data - assign_sub < block_size.U - assign_leftover && i.U + assign_data - assign_sub >= 0.U) {
            im2col_data(i.U + assign_data - assign_sub) := sram_req_output(i.U)
          }
        }
      }
    }
  }.otherwise{
    im2col_data := 0.S.asTypeOf(Vec(block_size, inputType))
  }

  class im2colRowSignals extends Bundle {
    val row_counter = UInt(10.W)
    val column_counter = UInt(10.W)
    val im2col_fin = Bool()
    val im2col_fin_pulse = Bool()
    val im2col_turn = UInt(9.W)
    val block_done = UInt(9.W)
    val start_inputting = Bool()
    val channel_wrap = UInt(10.W)
    val assign_sub = UInt(6.W)
    val sram_resp_valid = Bool()

  }
  sram_read_signals_q.io.enq.valid :=sram_read_valid && io.req.valid
  //sram_read_signals_q.io.enq.valid := sram_read_req && io.req.valid//(sram_resp_valid && sram_read_valid)
  sram_read_signals_q.io.enq.bits.row_counter := row_counter//row_address
  sram_read_signals_q.io.enq.bits.column_counter := column_counter
  sram_read_signals_q.io.enq.bits.im2col_fin := im2col_fin
  sram_read_signals_q.io.enq.bits.im2col_fin_pulse := im2col_fin_reg
  sram_read_signals_q.io.enq.bits.im2col_turn := im2col_turn
  sram_read_signals_q.io.enq.bits.block_done := block_done
  sram_read_signals_q.io.enq.bits.start_inputting := start_inputting_A
  //sram_read_signals_q.io.enq.bits.starting_channel := starting_channel*block_size.U
  sram_read_signals_q.io.enq.bits.channel_wrap := channel_turn
  sram_read_signals_q.io.enq.bits.assign_sub := assign_data_sub
  sram_read_signals_q.io.enq.bits.sram_resp_valid := sram_resp_valid
  when(channel < block_size.U){sram_read_signals_q.io.enq.valid := sram_read_req}

  sram_read_signals_q.io.deq.ready := true.B//sram_resp_valid

 /* Example of how to interface with scratchpad
  io.spad_reads(req.addr.sp_bank()).req.valid := true.B
  io.spad_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()

  io.spad_reads(req.addr.sp_bank()).resp
  */
}