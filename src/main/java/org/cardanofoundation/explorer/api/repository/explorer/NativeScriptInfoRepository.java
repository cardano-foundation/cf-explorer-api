package org.cardanofoundation.explorer.api.repository.explorer;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import org.cardanofoundation.explorer.consumercommon.explorer.entity.NativeScriptInfo;

public interface NativeScriptInfoRepository
    extends JpaRepository<NativeScriptInfo, Long>, JpaSpecificationExecutor<NativeScriptInfo> {

  Optional<NativeScriptInfo> findByScriptHash(String scriptHash);
}
