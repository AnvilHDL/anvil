// REFERENCE — written by hand, to be reviewed by the project owner
//
// dut.anvil:  send ep.res(*r) >> assert "sent" (ep.res!) >> cycle 1
//
// The assertion is written at the event where the send completes, which in
// the generated SV is _thread_0_events[1]:
//   assign _thread_0_events[1] = (_thread_0_events[0] || _thread_0_event_syncstate_1_q) && _ep_res_ack;
// ep.res! is "res is sent in this cycle" = valid && ack.  The receiver (ack) is
// a free input: no environment assumption is needed, the property holds at the
// completion event by construction.
module ref_top(input clk_i, input rst_ni, input _ep_res_ack);
  wire _ep_res_valid; wire [7:0] _ep_res_0;
  atom_send dut(.clk_i(clk_i), .rst_ni(rst_ni),
                ._ep_res_ack(_ep_res_ack), ._ep_res_valid(_ep_res_valid), ._ep_res_0(_ep_res_0));

  property P_ref;
    rst_ni && dut._thread_0_events[1] |-> (dut._ep_res_valid && dut._ep_res_ack);
  endproperty
  ref_verdict: assert property (P_ref);

  // propositional body: the wire the generated immediate check must equal, every cycle
  wire w_ref = dut._ep_res_valid && dut._ep_res_ack;
endmodule
