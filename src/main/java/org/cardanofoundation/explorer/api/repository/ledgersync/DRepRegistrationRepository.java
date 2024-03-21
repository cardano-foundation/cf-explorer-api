package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepCertificateProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.DRepRegistrationEntity;
import org.cardanofoundation.explorer.common.entity.ledgersync.compositeKey.DRepRegistrationId;

@Repository
public interface DRepRegistrationRepository
    extends JpaRepository<DRepRegistrationEntity, DRepRegistrationId> {

  @Query(
      value =
          "select dr.txHash as txHash, dr.certIndex as txIndex, dr.type as type,"
              + " dr.blockNumber as blockNo, dr.slot as slotNo, dr.epoch as epochNo,"
              + " dr.blockTime as blockTime, b.slotNo as absoluteSlot"
              + " from DRepRegistrationEntity dr"
              + " join Block b on b.blockNo = dr.blockNumber"
              + " where dr.drepId = :drepHashOrDrepId or dr.drepHash = :drepHashOrDrepId"
              + " order by blockTime desc")
  List<DRepCertificateProjection> getDRepCertificateByDRepIdOrHash(
      @Param("drepHashOrDrepId") String drepHashOrDrepId);
}
