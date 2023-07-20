package org.cardanofoundation.explorer.api.mapper;

import com.bloxbean.cardano.client.util.HexUtil;
import org.cardanofoundation.explorer.api.model.response.tx.ContractResponse;
import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(componentModel = "spring")
public interface TxContractMapper {

  @Mapping(target = "redeemerBytes", source = "redeemerBytes", qualifiedByName = "bytesToString")
  @Mapping(target = "datumBytesIn", source = "datumBytesIn", qualifiedByName = "bytesToString")
  @Mapping(target = "datumBytesOut", source = "datumBytesOut", qualifiedByName = "bytesToString")
  @Mapping(target = "scriptBytes", source = "scriptBytes", qualifiedByName = "bytesToString")
  ContractResponse fromTxContractProjectionToContractResponse(TxContractProjection txContractProjection);

  @Named("bytesToString")
  default String bytesToString(byte[] bytes) {
    return HexUtil.encodeHexString(bytes);
  }
}