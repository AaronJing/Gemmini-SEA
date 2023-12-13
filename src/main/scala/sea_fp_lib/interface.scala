package sea_fp_lib


import chisel3._
import chisel3.util._


trait adder_intf {
    def ioa           : UInt
    def iob           : UInt
    def ioop          : Bool
    def ioflag_inf2   : Bool
    def ioo_sgn       : UInt
    def iocond        : UInt
    def ioo_exp2      : UInt
    def ionorm_sum    : UInt
    def ioflag_zero2  : Bool 
}
