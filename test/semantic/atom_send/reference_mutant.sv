// REFERENCE — written by hand, to be reviewed by the project owner
//
// mutant.anvil:  send ep.res(*r) >> cycle 1 >> assert "sent" (ep.res!) >> cycle 1
//
// The assertion now sits one cycle after the send completes: _thread_0_events[2]
// (a one-cycle counter after events[1]).  valid is driven only while the send is
// pending (events[0] || syncstate), which is not the case at events[2], so the
// hand translation must be REFUTED.  The atom itself is unchanged (w_ref).
module ref_top(input clk_i, input rst_ni, input _ep_res_ack);
  wire _ep_res_valid; wire [7:0] _ep_res_0;
  atom_send dut(.clk_i(clk_i), .rst_ni(rst_ni),
                ._ep_res_ack(_ep_res_ack), ._ep_res_valid(_ep_res_valid), ._ep_res_0(_ep_res_0));

  property P_ref;
    rst_ni && dut._thread_0_events[2] |-> (dut._ep_res_valid && dut._ep_res_ack);
  endproperty
  ref_verdict: assert property (P_ref);

  wire w_ref = dut._ep_res_valid && dut._ep_res_ack;
endmodule
