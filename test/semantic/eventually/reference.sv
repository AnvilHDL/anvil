// REFERENCE — written by hand, to be reviewed by the project owner
//
// dut.anvil (docs/examples/assert/echo_assert.anvil, labelled), producer:
//   send ep.req(y) >> assert "requested" (ep.req!) >> assert "answered" (F ep.res?) >> let z = recv ep.res >> ...
//
// The assertion under test ("answered") is written at the event where the
// producer's send of req completes.  The producer is the instance _spawn_0 of
// the top module echo (docs/examples/assert/echo.sv), and in it
//   assign _thread_0_events[1] = (_thread_0_events[0] || _thread_0_event_syncstate_1_q) && _ep_req_ack;
// Assertions add no events: the event vectors of echo.sv (compiled without
// assertions) and of the labelled program are the same size (producer 4,
// consumer 6).  ep.res? in the producer is _ep_res_valid && _ep_res_ack of the
// producer instance.  The system is closed (both ends are spawned), so no
// environment assumption is needed.  F is s_eventually (assert.typ 2.3).
module ref_top(input clk_i, input rst_ni);
  echo dut(.clk_i(clk_i), .rst_ni(rst_ni));

  property P_ref;
    rst_ni && dut._spawn_0._thread_0_events[1] |-> s_eventually (dut._spawn_0._ep_res_valid && dut._spawn_0._ep_res_ack);
  endproperty
  ref_verdict: assert property (P_ref);
endmodule
