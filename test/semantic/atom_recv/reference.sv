// REFERENCE — written by hand, to be reviewed by the project owner
//
// dut.anvil:  let x = recv ep.req >> assert "got" (ep.req? && x == 8'd1) >> cycle 1
//
// The assertion is written at the event where the recv completes.  In the
// generated SV that event is _thread_0_events[1]:
//   assign _thread_0_events[1] = (_thread_0_events[0] || _thread_0_event_syncstate_1_q) && _ep_req_valid;
// ep.req? is "req is received in this cycle" = valid && ack; x is the value on
// the data port in that cycle (_ep_req_0).  The environment is assumed to send 1.
module ref_top(input clk_i, input rst_ni, input _ep_req_valid, input [7:0] _ep_req_0);
  wire _ep_req_ack;
  atom_recv dut(.clk_i(clk_i), .rst_ni(rst_ni),
                ._ep_req_ack(_ep_req_ack), ._ep_req_valid(_ep_req_valid), ._ep_req_0(_ep_req_0));

  env_value: assume property (_ep_req_0 == 8'd1);   // the sender always sends 1

  property P_ref;
    rst_ni && dut._thread_0_events[1] |-> (dut._ep_req_valid && dut._ep_req_ack && dut._ep_req_0 == 8'd1);
  endproperty
  ref_verdict: assert property (P_ref);

  // propositional body: the wire the generated immediate check must equal, every cycle
  wire w_ref = dut._ep_req_valid && dut._ep_req_ack && dut._ep_req_0 == 8'd1;
endmodule
