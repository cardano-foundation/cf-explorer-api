package org.cardanofoundation.explorer.api.mapper;

import java.util.HashSet;
import java.util.Set;

import org.springframework.util.CollectionUtils;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.common.enumeration.TxPurposeType;
import org.cardanofoundation.explorer.api.model.request.script.smartcontract.SmartContractFilterRequest;
import org.cardanofoundation.explorer.api.model.response.script.projection.SmartContractTxProjection;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.common.entity.explorer.SmartContractInfo;

@Mapper(componentModel = "spring")
public interface ScriptMapper {

  SmartContractTxResponse fromSmartContractTxProjection(
      SmartContractTxProjection smartContractTxProjection);

  @Mapping(target = "txPurposes", expression = "java(getScriptTxPurpose(smartContractInfo))")
  @Mapping(target = "scriptVersion", source = "type")
  SmartContractFilterResponse fromSCInfoToSCFilterResponse(SmartContractInfo smartContractInfo);

  default Set<TxPurposeType> getScriptTxPurpose(SmartContractInfo smartContractInfo) {
    Set<TxPurposeType> scriptPurposeTypes = new HashSet<>();
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptCert())) {
      scriptPurposeTypes.add(TxPurposeType.CERT);
    }
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptMint())) {
      scriptPurposeTypes.add(TxPurposeType.MINT);
    }
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptReward())) {
      scriptPurposeTypes.add(TxPurposeType.REWARD);
    }
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptSpend())) {
      scriptPurposeTypes.add(TxPurposeType.SPEND);
    }
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptVote())) {
      scriptPurposeTypes.add(TxPurposeType.VOTE);
    }
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptPropose())) {
      scriptPurposeTypes.add(TxPurposeType.PROPOSE);
    }
    return scriptPurposeTypes;
  }

  default void setScriptTxPurpose(SmartContractFilterRequest filterRequest) {
    if (!CollectionUtils.isEmpty(filterRequest.getTxPurpose())) {
      Set<TxPurposeType> txPurposeTypes = filterRequest.getTxPurpose();
      filterRequest.setIsScriptNone(txPurposeTypes.contains(TxPurposeType.NO_TX_PURPOSE));
      filterRequest.setIsScriptAny(txPurposeTypes.contains(TxPurposeType.ANY));
      filterRequest.setIsScriptReward(txPurposeTypes.contains(TxPurposeType.REWARD));
      filterRequest.setIsScriptCert(txPurposeTypes.contains(TxPurposeType.CERT));
      filterRequest.setIsScriptSpend(txPurposeTypes.contains(TxPurposeType.SPEND));
      filterRequest.setIsScriptMint(txPurposeTypes.contains(TxPurposeType.MINT));
      filterRequest.setIsScriptVote(txPurposeTypes.contains(TxPurposeType.VOTE));
      filterRequest.setIsScriptPropose(txPurposeTypes.contains(TxPurposeType.PROPOSE));
    } else {
      filterRequest.setIsScriptAny(true);
    }
  }
}
