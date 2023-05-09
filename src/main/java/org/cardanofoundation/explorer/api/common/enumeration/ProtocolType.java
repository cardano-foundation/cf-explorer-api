package org.cardanofoundation.explorer.api.common.enumeration;


import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal_;

public enum ProtocolType {
  MIN_FEE_A(ParamProposal_.MIN_FEE_A),
  MIN_FEE_B(ParamProposal_.MIN_FEE_B),
  MAX_BLOCK_SIZE(ParamProposal_.MAX_BLOCK_SIZE),
  MAX_TX_SIZE(ParamProposal_.MAX_TX_SIZE),
  MAX_BH_SIZE(ParamProposal_.MAX_BH_SIZE),
  KEY_DEPOSIT(ParamProposal_.KEY_DEPOSIT),
  POOL_DEPOSIT(ParamProposal_.POOL_DEPOSIT),
  MAX_EPOCH(ParamProposal_.MAX_EPOCH),
  OPTIMAL_POOL_COUNT(ParamProposal_.OPTIMAL_POOL_COUNT),
  MIN_UTXO_VALUE(ParamProposal_.MIN_UTXO_VALUE),
  MIN_POOL_COST(ParamProposal_.MIN_POOL_COST),
  MAX_TX_EX_MEM(ParamProposal_.MAX_TX_EX_MEM),
  MAX_TX_EX_STEPS(ParamProposal_.MAX_TX_EX_STEPS),
  MAX_BLOCK_EX_MEM(ParamProposal_.MAX_BLOCK_EX_MEM),
  MAX_BLOCK_EX_STEPS(ParamProposal_.MAX_BLOCK_EX_STEPS),
  MAX_VAL_SIZE(ParamProposal_.MAX_VAL_SIZE),
  COINS_PER_UTXO_SIZE(ParamProposal_.COINS_PER_UTXO_SIZE),
  INFLUENCE(ParamProposal_.INFLUENCE),
  MONETARY_EXPAND_RATE(ParamProposal_.MONETARY_EXPAND_RATE),
  TREASURY_GROWTH_RATE(ParamProposal_.TREASURY_GROWTH_RATE),
  DECENTRALISATION(ParamProposal_.DECENTRALISATION),
  PRICE_MEM(ParamProposal_.PRICE_MEM),
  PRICE_STEP(ParamProposal_.PRICE_STEP),
  PROTOCOL_MAJOR(ParamProposal_.PROTOCOL_MAJOR),
  PROTOCOL_MINOR(ParamProposal_.PROTOCOL_MINOR),
  COLLATERAL_PERCENT(ParamProposal_.COLLATERAL_PERCENT),
  MAX_COLLATERAL_INPUTS(ParamProposal_.MAX_COLLATERAL_INPUTS),
  ENTROPY(ParamProposal_.ENTROPY),
  COST_MODEL(ParamProposal_.COST_MODEL);

  private final String fieldName;

  ProtocolType(String fieldName) {
    this.fieldName = fieldName;
  }

  public String getFieldName() {
    return fieldName;
  }
}
