// REFERENCE — written by hand, to be reviewed by the project owner
//
// mutant.anvil asserts x == 8'd2 while the environment sends 8'd1: the hand
// translation of the mutant's assertion, which must be REFUTED.  Same event bit
// as reference.sv (the assertion did not move).
module ref_top(input clk_i, input rst_ni, input _ep_req_valid, input [7:0] _ep_req_0);
  wire _ep_req_ack;
  atom_recv dut(.clk_i(clk_i), .rst_ni(rst_ni),
                ._ep_req_ack(_ep_req_ack), ._ep_req_valid(_ep_req_valid), ._ep_req_0(_ep_req_0));

  env_value: assume property (_ep_req_0 == 8'd1);   // the sender always sends 1

  property P_ref;
    rst_ni && dut._thread_0_events[1] |-> (dut._ep_req_valid && dut._ep_req_ack && dut._ep_req_0 == 8'd2);
  endproperty
  ref_verdict: assert property (P_ref);

  wire w_ref = dut._ep_req_valid && dut._ep_req_ack && dut._ep_req_0 == 8'd2;
endmodule
