package org.cardanofoundation.explorer.api.model;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class ProtocolParamUpdate {
  @JsonProperty("minUtxo")
  private BigInteger minUtxo;

  @JsonProperty("maxEpoch")
  private Integer maxEpoch;

  @JsonProperty("extraEntropy")
  private String extraEntropy;

  @JsonProperty("expansionRate")
  private Double expansionRate;

  @JsonProperty("adaPerUtxoByte")
  private BigInteger adaPerUtxoByte;

  @JsonProperty("protocolMajorVer")
  private Integer protocolMajorVer;

  @JsonProperty("protocolMinorVer")
  private Integer protocolMinorVer;

  @JsonProperty("treasuryGrowthRate")
  private Double treasuryGrowthRate;

  @JsonProperty("decentralisationParam")
  private Double decentralisationParam;

  @JsonProperty("costModelsHash")
  private String costModelsHash;
  //  Economic group
  @JsonProperty("minFeeA")
  private Integer minFeeA;

  @JsonProperty("minFeeB")
  private Integer minFeeB;

  @JsonProperty("keyDeposit")
  private BigInteger keyDeposit;

  @JsonProperty("poolDeposit")
  private BigInteger poolDeposit;

  @JsonProperty("minPoolCost")
  private BigInteger minPoolCost;

  @JsonProperty("priceMem")
  private Double priceMem;

  @JsonProperty("priceStep")
  private Double priceStep;
  //  Network group
  @JsonProperty("maxBlockSize")
  private Integer maxBlockSize;

  @JsonProperty("maxTxSize")
  private Integer maxTxSize;

  @JsonProperty("maxBlockHeaderSize")
  private Integer maxBlockHeaderSize;

  @JsonProperty("maxValSize")
  private BigInteger maxValSize;

  @JsonProperty("maxTxExMem")
  private BigInteger maxTxExMem;

  @JsonProperty("maxTxExSteps")
  private BigInteger maxTxExSteps;

  @JsonProperty("maxBlockExMem")
  private BigInteger maxBlockExMem;

  @JsonProperty("maxBlockExSteps")
  private BigInteger maxBlockExSteps;

  @JsonProperty("maxCollateralInputs")
  private Integer maxCollateralInputs;
  //  Technical group
  @JsonProperty("nopt")
  private Integer nopt;

  @JsonProperty("poolPledgeInfluence")
  private Double poolPledgeInfluence;

  @JsonProperty("costModels")
  private Object costModels;

  @JsonProperty("collateralPercent")
  private Double collateralPercent;
  //  governance group
  @JsonProperty("drepVotingThresholds")
  private Object drepVotingThresholds;

  @JsonProperty("poolVotingThresholds")
  private Object poolVotingThresholds;

  @JsonProperty("govActionLifetime")
  private BigInteger govActionLifetime;

  @JsonProperty("govActionDeposit")
  private BigInteger govActionDeposit;

  @JsonProperty("drepDeposit")
  private BigInteger drepDeposit;

  @JsonProperty("drepActivity")
  private BigInteger drepActivity;

  @JsonProperty("committeeMinSize")
  private BigInteger committeeMinSize;

  @JsonProperty("committeeMaxTermLength")
  private BigInteger committeeMaxTermLength;
  //  security group
  @JsonProperty("minFeeRefScriptCostPerByte")
  private BigInteger minFeeRefScriptCostPerByte;
}
