package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.PoolRelayProjection;
import com.sotatek.cardano.common.entity.PoolRelay;
import java.util.Collection;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolRelayRepository extends JpaRepository<PoolRelay, Long> {

  @Query("SELECT pr.poolUpdate.id as poolUpdateId, pr.dnsName as dnsName, "
      + " pr.dnsSrvName as dnsSrvName, pr.ipv4 as ipv4, pr.ipv6 as ipv6, pr.port as port"
      + " FROM PoolRelay pr WHERE pr.poolUpdate.id IN :poolUpdateIds")
  List<PoolRelayProjection> findByPoolHashIdIn(Collection<Long> poolUpdateIds);
}
