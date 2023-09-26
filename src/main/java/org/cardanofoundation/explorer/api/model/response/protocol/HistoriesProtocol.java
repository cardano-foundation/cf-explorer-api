package org.cardanofoundation.explorer.api.model.response.protocol;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(value = Include.NON_EMPTY)
public class HistoriesProtocol {
  List<EpochChange> epochChanges;
  List<ProtocolHistory> minFeeA;
  List<ProtocolHistory> minFeeB;
  List<ProtocolHistory> maxBlockSize;
  List<ProtocolHistory> maxTxSize;
  List<ProtocolHistory> maxBhSize;
  List<ProtocolHistory> keyDeposit;
  List<ProtocolHistory> poolDeposit;
  List<ProtocolHistory> maxEpoch;
  List<ProtocolHistory> optimalPoolCount;
  List<ProtocolHistory> influence;
  List<ProtocolHistory> monetaryExpandRate;
  List<ProtocolHistory> treasuryGrowthRate;
  List<ProtocolHistory> decentralisation;
  List<ProtocolHistory> entropy;
  List<ProtocolHistory> protocolMajor;
  List<ProtocolHistory> protocolMinor;
  List<ProtocolHistory> minUtxoValue;
  List<ProtocolHistory> minPoolCost;
  List<ProtocolHistory> costModel;
  List<ProtocolHistory> priceMem;
  List<ProtocolHistory> priceStep;
  List<ProtocolHistory> maxTxExMem;
  List<ProtocolHistory> maxTxExSteps;
  List<ProtocolHistory> maxBlockExMem;
  List<ProtocolHistory> maxBlockExSteps;
  List<ProtocolHistory> maxValSize;
  List<ProtocolHistory> collateralPercent;
  List<ProtocolHistory> maxCollateralInputs;
  @JsonProperty("coinsPerUTxOByte")
  List<ProtocolHistory> coinsPerUtxoSize;
}
