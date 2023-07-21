package org.cardanofoundation.explorer.api.repository;

import java.util.Collection;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.consumercommon.entity.TokenInfo;

@Repository
public interface TokenInfoRepository extends JpaRepository<TokenInfo, Long> {
  List<TokenInfo> findTokenInfosByMultiAssetIdIn(Collection<Long> multiAssetIds);
}
