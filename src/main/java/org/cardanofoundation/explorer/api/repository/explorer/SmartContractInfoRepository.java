package org.cardanofoundation.explorer.api.repository.explorer;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.SmartContractInfo;

public interface SmartContractInfoRepository extends JpaRepository<SmartContractInfo, Long> {

  @Query("SELECT sci FROM SmartContractInfo sci "
      + "WHERE (:scriptType IS NULL OR sci.type = :scriptType) "
      + "AND (:isScriptAny = TRUE OR"
      + "(:isScriptNone = TRUE AND sci.txCount = 0) OR "
      + "(:isScriptCert = sci.isScriptCert AND sci.isScriptCert = TRUE) OR "
      + "(:isScriptMint = sci.isScriptMint AND sci.isScriptMint = TRUE) OR "
      + "(:isScriptReward = sci.isScriptReward AND sci.isScriptReward = TRUE) OR "
      + "(:isScriptSpend= sci.isScriptSpend AND sci.isScriptSpend = TRUE)) ")
  Page<SmartContractInfo> findAllByFilterRequest(
      @Param("scriptType") ScriptType scriptType,
      @Param("isScriptReward") Boolean isScriptReward,
      @Param("isScriptCert") Boolean isScriptCert,
      @Param("isScriptSpend") Boolean isScriptSpend,
      @Param("isScriptMint") Boolean isScriptMint,
      @Param("isScriptAny") Boolean isScriptAny,
      @Param("isScriptNone") Boolean isScriptNone,
      Pageable pageable);

  @Query("SELECT sci.txCount FROM SmartContractInfo sci WHERE sci.scriptHash = :scriptHash")
  Long getTxCountByScriptHash(@Param("scriptHash") String scriptHash);
}
