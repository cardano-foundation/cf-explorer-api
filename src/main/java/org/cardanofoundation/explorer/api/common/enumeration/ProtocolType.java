package org.cardanofoundation.explorer.api.common.enumeration;


import java.util.List;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam_;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal_;

public enum ProtocolType {
  MIN_FEE_A(EpochParam_.MIN_FEE_A),
  MIN_FEE_B(EpochParam_.MIN_FEE_B),
  MAX_BLOCK_SIZE(EpochParam_.MAX_BLOCK_SIZE),
  MAX_TX_SIZE(EpochParam_.MAX_TX_SIZE),
  MAX_BH_SIZE(EpochParam_.MAX_BH_SIZE),
  KEY_DEPOSIT(EpochParam_.KEY_DEPOSIT),
  POOL_DEPOSIT(EpochParam_.POOL_DEPOSIT),
  MAX_EPOCH(EpochParam_.MAX_EPOCH),
  OPTIMAL_POOL_COUNT(EpochParam_.OPTIMAL_POOL_COUNT),
  MIN_UTXO_VALUE(EpochParam_.MIN_UTXO_VALUE),
  MIN_POOL_COST(EpochParam_.MIN_POOL_COST),
  MAX_TX_EX_MEM(EpochParam_.MAX_TX_EX_MEM),
  MAX_TX_EX_STEPS(EpochParam_.MAX_TX_EX_STEPS),
  MAX_BLOCK_EX_MEM(EpochParam_.MAX_BLOCK_EX_MEM),
  MAX_BLOCK_EX_STEPS(EpochParam_.MAX_BLOCK_EX_STEPS),
  MAX_VAL_SIZE(EpochParam_.MAX_VAL_SIZE),
  COINS_PER_UTXO_SIZE(EpochParam_.COINS_PER_UTXO_SIZE),
  INFLUENCE(EpochParam_.INFLUENCE),
  MONETARY_EXPAND_RATE(EpochParam_.MONETARY_EXPAND_RATE),
  TREASURY_GROWTH_RATE(EpochParam_.TREASURY_GROWTH_RATE),
  DECENTRALISATION(EpochParam_.DECENTRALISATION),
  PRICE_MEM(EpochParam_.PRICE_MEM),
  PRICE_STEP(EpochParam_.PRICE_STEP),
  PROTOCOL_MAJOR(EpochParam_.PROTOCOL_MAJOR),
  PROTOCOL_MINOR(EpochParam_.PROTOCOL_MINOR),
  COLLATERAL_PERCENT(EpochParam_.COLLATERAL_PERCENT),
  MAX_COLLATERAL_INPUTS(EpochParam_.MAX_COLLATERAL_INPUTS),
  ENTROPY(ParamProposal_.ENTROPY),
  COST_MODEL(EpochParam_.COST_MODEL);

  private final String fieldName;

  ProtocolType(String fieldName) {
    this.fieldName = fieldName;
  }

  public String getFieldName() {
    return fieldName;
  }

  public static ProtocolType valueStringOf(String fieldName) {
    switch (fieldName) {
      case EpochParam_.MIN_FEE_A:
        return MIN_FEE_A;
      case EpochParam_.MIN_FEE_B:
        return MIN_FEE_B;
      case EpochParam_.MAX_BLOCK_SIZE:
        return MAX_BLOCK_SIZE;
      case EpochParam_.MAX_TX_SIZE:
        return MAX_TX_SIZE;
      case EpochParam_.MAX_BH_SIZE:
        return MAX_BH_SIZE;
      case EpochParam_.KEY_DEPOSIT:
        return KEY_DEPOSIT;
      case EpochParam_.POOL_DEPOSIT:
        return POOL_DEPOSIT;
      case EpochParam_.MAX_EPOCH:
        return MAX_EPOCH;
      case EpochParam_.OPTIMAL_POOL_COUNT:
        return OPTIMAL_POOL_COUNT;
      case EpochParam_.MIN_UTXO_VALUE:
        return MIN_UTXO_VALUE;
      case EpochParam_.MIN_POOL_COST:
        return MIN_POOL_COST;
      case EpochParam_.MAX_TX_EX_MEM:
        return MAX_TX_EX_MEM;
      case EpochParam_.MAX_TX_EX_STEPS:
        return MAX_TX_EX_STEPS;
      case EpochParam_.MAX_BLOCK_EX_MEM:
        return MAX_BLOCK_EX_MEM;
      case EpochParam_.MAX_BLOCK_EX_STEPS:
        return MAX_BLOCK_EX_STEPS;
      case EpochParam_.MAX_VAL_SIZE:
        return MAX_VAL_SIZE;
      case EpochParam_.COINS_PER_UTXO_SIZE:
        return COINS_PER_UTXO_SIZE;
      case EpochParam_.INFLUENCE:
        return INFLUENCE;
      case EpochParam_.MONETARY_EXPAND_RATE:
        return MONETARY_EXPAND_RATE;
      case EpochParam_.TREASURY_GROWTH_RATE:
        return TREASURY_GROWTH_RATE;
      case EpochParam_.DECENTRALISATION:
        return DECENTRALISATION;
      case EpochParam_.PRICE_MEM:
        return PRICE_MEM;
      case EpochParam_.PRICE_STEP:
        return PRICE_STEP;
      case EpochParam_.PROTOCOL_MAJOR:
        return PROTOCOL_MAJOR;
      case EpochParam_.PROTOCOL_MINOR:
        return PROTOCOL_MINOR;
      case EpochParam_.COLLATERAL_PERCENT:
        return COLLATERAL_PERCENT;
      case EpochParam_.MAX_COLLATERAL_INPUTS:
        return MAX_COLLATERAL_INPUTS;
      case ParamProposal_.ENTROPY:
      case EpochParam_.EXTRA_ENTROPY:
        return ENTROPY;
      case EpochParam_.COST_MODEL:
        return COST_MODEL;
      default:
        throw new BusinessException(BusinessCode.PROTOCOL_FIELD_NOT_FOUND);
    }
  }

  public static List<ProtocolType> getAll() {
    return List.of(MIN_FEE_A, MIN_FEE_B, MAX_BLOCK_SIZE, MAX_TX_SIZE, MAX_BH_SIZE, KEY_DEPOSIT,
        POOL_DEPOSIT, MAX_EPOCH, OPTIMAL_POOL_COUNT, MIN_UTXO_VALUE, MIN_POOL_COST, MAX_TX_EX_MEM,
        MAX_TX_EX_STEPS, MAX_BLOCK_EX_MEM, MAX_BLOCK_EX_STEPS, MAX_VAL_SIZE, COINS_PER_UTXO_SIZE,
        INFLUENCE, MONETARY_EXPAND_RATE, TREASURY_GROWTH_RATE, DECENTRALISATION, PRICE_MEM,
        PRICE_STEP, PROTOCOL_MAJOR, PROTOCOL_MINOR, COLLATERAL_PERCENT, MAX_COLLATERAL_INPUTS,
        ENTROPY, COST_MODEL);
  }
}
