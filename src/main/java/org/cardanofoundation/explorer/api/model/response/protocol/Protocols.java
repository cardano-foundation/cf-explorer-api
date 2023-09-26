package org.cardanofoundation.explorer.api.model.response.protocol;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.util.Pair;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;


@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
@Log4j2
public class Protocols {
  @JsonIgnore
  EpochChange epochChange;
  ProtocolHistory minFeeA;
  ProtocolHistory minFeeB;
  ProtocolHistory maxBlockSize;
  ProtocolHistory maxTxSize;
  ProtocolHistory maxBhSize;
  ProtocolHistory keyDeposit;
  ProtocolHistory poolDeposit;
  ProtocolHistory maxEpoch;
  ProtocolHistory optimalPoolCount;
  ProtocolHistory influence;
  ProtocolHistory monetaryExpandRate;
  ProtocolHistory treasuryGrowthRate;
  ProtocolHistory decentralisation;
  ProtocolHistory entropy;
  ProtocolHistory protocolMajor;
  ProtocolHistory protocolMinor;
  ProtocolHistory minUtxoValue;
  ProtocolHistory minPoolCost;
  ProtocolHistory costModel;
  ProtocolHistory priceMem;
  ProtocolHistory priceStep;
  ProtocolHistory maxTxExMem;
  ProtocolHistory maxTxExSteps;
  ProtocolHistory maxBlockExMem;
  ProtocolHistory maxBlockExSteps;
  ProtocolHistory maxValSize;
  ProtocolHistory collateralPercent;
  ProtocolHistory maxCollateralInputs;

  @JsonProperty("coinsPerUtxoByte")
  ProtocolHistory coinsPerUtxoSize;
  LocalDateTime timestamp;

  public boolean equals(Object o, Map<ProtocolType, Pair<Method, Method>> protocolsMethods,
                        List<ProtocolType> protocolTypes) {
    if (this == o) {
      return true;
    }
    if (!(o instanceof Protocols protocols)) {
      return false;
    }
    AtomicBoolean isEqual = new AtomicBoolean(Boolean.TRUE);
    protocolTypes.forEach(protocolType -> {
      if (isEqual.get()) {
        final Method getMethod = protocolsMethods.get(protocolType).getSecond();
        try {
          isEqual.set(Objects.equals(getMethod.invoke(this), getMethod.invoke(protocols)));
        } catch (Exception e) {
          log.error(e.getMessage());
          log.error(e.getLocalizedMessage());
        }
      }
    });

    return isEqual.get();
  }

  @Override
  public int hashCode() {
    return Objects.hash(minFeeA, minFeeB, maxBlockSize, maxTxSize, maxBhSize, keyDeposit,
        poolDeposit,
        maxEpoch, optimalPoolCount, influence, monetaryExpandRate,
        treasuryGrowthRate,
        decentralisation, entropy, protocolMajor, protocolMinor, minUtxoValue,
        minPoolCost, costModel, priceMem, priceStep, maxTxExMem, maxTxExSteps,
        maxBlockExMem, maxBlockExSteps, maxValSize, collateralPercent,
        maxCollateralInputs, coinsPerUtxoSize);
  }

  @JsonIgnore
  public boolean isEmpty() {
    return Objects.isNull(minFeeA) &&
        Objects.isNull(minFeeB) &&
        Objects.isNull(maxBlockSize) &&
        Objects.isNull(maxTxSize) &&
        Objects.isNull(maxBhSize) &&
        Objects.isNull(keyDeposit) &&
        Objects.isNull(poolDeposit) &&
        Objects.isNull(maxEpoch) &&
        Objects.isNull(optimalPoolCount) &&
        Objects.isNull(influence) &&
        Objects.isNull(monetaryExpandRate) &&
        Objects.isNull(treasuryGrowthRate) &&
        Objects.isNull(decentralisation) &&
        Objects.isNull(entropy) &&
        Objects.isNull(protocolMajor) &&
        Objects.isNull(protocolMinor) &&
        Objects.isNull(minUtxoValue) &&
        Objects.isNull(minPoolCost) &&
        Objects.isNull(costModel) &&
        Objects.isNull(priceMem) &&
        Objects.isNull(priceStep) &&
        Objects.isNull(maxTxExMem) &&
        Objects.isNull(maxTxExSteps) &&
        Objects.isNull(maxBlockExMem) &&
        Objects.isNull(maxBlockExSteps) &&
        Objects.isNull(maxValSize) &&
        Objects.isNull(collateralPercent) &&
        Objects.isNull(maxCollateralInputs) &&
        Objects.isNull(coinsPerUtxoSize);
  }
}
