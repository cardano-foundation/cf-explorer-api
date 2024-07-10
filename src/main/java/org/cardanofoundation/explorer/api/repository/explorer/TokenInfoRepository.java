package org.cardanofoundation.explorer.api.repository.explorer;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.common.entity.explorer.TokenInfo;

@Repository
public interface TokenInfoRepository extends JpaRepository<TokenInfo, Long> {
  List<TokenInfo> findTokenInfosByMultiAssetIdIn(Collection<Long> multiAssetIds);

  Optional<TokenInfo> findTokenInfoByMultiAssetId(Long multiAssetId);

  @Query(
      value =
          "SELECT ti.id FROM TokenInfo ti ORDER BY ti.txCount DESC LIMIT :pageSize")
  List<Long> findOrderedByTxCount(@Param("pageSize") int pageSize);
}
