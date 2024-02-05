package org.cardanofoundation.explorer.api.mapper;

import com.bloxbean.cardano.client.util.HexUtil;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import org.cardanofoundation.explorer.api.model.response.tx.TxReferenceInput;
import org.cardanofoundation.explorer.api.projection.ReferenceInputProjection;

@Mapper(componentModel = "spring")
public interface TxReferenceInputMapper {

  @Mapping(target = "datum", source = "datumBytes", qualifiedByName = "bytesToString")
  @Mapping(target = "script", source = "scriptBytes", qualifiedByName = "bytesToString")
  TxReferenceInput fromReferenceInputProjectionTxReferenceInput(
      ReferenceInputProjection projection);

  @Named("bytesToString")
  default String bytesToString(byte[] bytes) {
    return HexUtil.encodeHexString(bytes);
  }
}
