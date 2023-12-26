package org.cardanofoundation.explorer.api.mapper;

import java.util.HashSet;
import java.util.Set;

import org.springframework.util.CollectionUtils;

import org.cardanofoundation.explorer.api.model.request.script.smartcontract.SmartContractFilterRequest;
import org.cardanofoundation.explorer.api.model.response.script.projection.SmartContractTxProjection;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.SmartContractInfo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface ScriptMapper {

  SmartContractTxResponse fromSmartContractTxProjection(
      SmartContractTxProjection smartContractTxProjection);

  @Mapping(target = "txPurposes", expression = "java(getScriptTxPurpose(smartContractInfo))")
  @Mapping(target = "scriptVersion", source = "type")
  SmartContractFilterResponse fromSCInfoToSCFilterResponse(SmartContractInfo smartContractInfo);

  default Set<ScriptPurposeType> getScriptTxPurpose(SmartContractInfo smartContractInfo) {
    Set<ScriptPurposeType> scriptPurposeTypes = new HashSet<>();
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptCert())) {
      scriptPurposeTypes.add(ScriptPurposeType.CERT);
    }
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptMint())) {
      scriptPurposeTypes.add(ScriptPurposeType.MINT);
    }
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptReward())) {
      scriptPurposeTypes.add(ScriptPurposeType.REWARD);
    }
    if (Boolean.TRUE.equals(smartContractInfo.getIsScriptSpend())) {
      scriptPurposeTypes.add(ScriptPurposeType.SPEND);
    }
    return scriptPurposeTypes;
  }

  default void setScriptTxPurpose(SmartContractFilterRequest filterRequest) {
    if (!CollectionUtils.isEmpty(filterRequest.getTxPurpose())) {
      if (filterRequest.getTxPurpose().contains(ScriptPurposeType.MINT)) {
        filterRequest.setIsScriptMint(true);
      }
      if (filterRequest.getTxPurpose().contains(ScriptPurposeType.SPEND)) {
        filterRequest.setIsScriptSpend(true);
      }
      if (filterRequest.getTxPurpose().contains(ScriptPurposeType.CERT)) {
        filterRequest.setIsScriptCert(true);
      }
      if (filterRequest.getTxPurpose().contains(ScriptPurposeType.REWARD)) {
        filterRequest.setIsScriptReward(true);
      }
    } else {
      filterRequest.setIsScriptAny(true);
    }
  }
}
