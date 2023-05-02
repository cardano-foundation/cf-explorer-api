package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.AddressToken;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressTokenRepository extends JpaRepository<AddressToken, Long> {

  @Query(value = "SELECT DISTINCT addrToken.tx.id "
      + " FROM AddressToken addrToken "
      + " WHERE addrToken.multiAsset = :multiAsset "
      + " ORDER BY addrToken.tx.id DESC")
  List<Long> findTxsByMultiAsset(MultiAsset multiAsset, Pageable pageable);

}
