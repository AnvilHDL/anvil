// REFERENCE — written by hand, to be reviewed by the project owner
//
// dut.anvil (docs/examples/assert/tick3_assert.anvil):
//   cycle 1 >> send ep.tick(*n) >> assert "period" (N N N ep.tick!) >> set n := *n + 8'd1 >> cycle 1
//
// The assertion is written at the event where the send completes; in the SV
// generated from tick3.anvil (docs/examples/assert/tick3.sv) that is
//   assign _thread_0_events[2] = (_thread_0_events[1] || _thread_0_event_syncstate_2_q) && _ep_tick_ack;
// The same bit in tick3_bug.sv (the mutant): the mutant changes the counter
// before the send, not the event numbering.  The consumer is assumed to
// always accept (docs/examples/assert/top.sv), so the period is exactly 3.
// Translation of assert.typ section 2.3, as in docs/examples/assert/README.md.
module ref_top(input clk_i, input rst_ni, input _ep_tick_ack);
  wire _ep_tick_valid; wire [7:0] _ep_tick_0;
  tick3 dut(.clk_i(clk_i), .rst_ni(rst_ni),
            ._ep_tick_ack(_ep_tick_ack), ._ep_tick_valid(_ep_tick_valid), ._ep_tick_0(_ep_tick_0));

  ready: assume property (_ep_tick_ack);   // the consumer always accepts

  property P_ref;
    rst_ni && dut._thread_0_events[2] |-> nexttime nexttime nexttime (dut._ep_tick_valid && dut._ep_tick_ack);
  endproperty
  ref_verdict: assert property (P_ref);
endmodule
