package org.cardanofoundation.explorer.api.repository.explorer;

import org.cardanofoundation.explorer.consumercommon.explorer.entity.NativeScriptInfo;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.Optional;

public interface NativeScriptInfoRepository extends JpaRepository<NativeScriptInfo, Long>,
    JpaSpecificationExecutor<NativeScriptInfo> {

  Optional<NativeScriptInfo> findByScriptHash(String scriptHash);
}
