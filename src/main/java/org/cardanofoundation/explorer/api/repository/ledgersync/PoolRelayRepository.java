package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRelayProjection;
import org.cardanofoundation.explorer.consumercommon.entity.PoolRelay;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.List;

@Repository
public interface PoolRelayRepository extends JpaRepository<PoolRelay, Long> {

  @Query("SELECT pr.poolUpdate.id as poolUpdateId, pr.dnsName as dnsName, "
      + " pr.dnsSrvName as dnsSrvName, pr.ipv4 as ipv4, pr.ipv6 as ipv6, pr.port as port"
      + " FROM PoolRelay pr WHERE pr.poolUpdate.id IN :poolUpdateIds")
  List<PoolRelayProjection> findByPoolHashIdIn(@Param("poolUpdateIds") Collection<Long> poolUpdateIds);
}
