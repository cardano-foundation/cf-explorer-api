package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.pool.PoolCertificateHistory;
import org.cardanofoundation.explorer.api.model.response.pool.TxPoolCertificateHistory;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCertificateProjection;

@Mapper(componentModel = "spring")
public interface PoolCertificateMapper {

  PoolCertificateHistory fromPoolCertificateProjection(
      PoolCertificateProjection poolCertificateProjection);

  @Mapping(target = "actions", ignore = true)
  @Mapping(target = "actionType", ignore = true)
  TxPoolCertificateHistory fromPoolCertificateHistory(
      PoolCertificateHistory poolCertificateHistory);
}
