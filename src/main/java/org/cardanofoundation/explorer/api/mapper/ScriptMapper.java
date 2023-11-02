package org.cardanofoundation.explorer.api.mapper;

import org.cardanofoundation.explorer.api.model.response.script.projection.SmartContractTxProjection;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface ScriptMapper {

  SmartContractTxResponse fromSmartContractTxProjection(SmartContractTxProjection smartContractTxProjection);
}
