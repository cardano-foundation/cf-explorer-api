package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.compositeKey.CommitteeDeRegistrationId;
import org.cardanofoundation.explorer.common.entity.ledgersync.CommitteeDeRegistration;

public interface CommitteeDeRegistrationRepository
    extends JpaRepository<CommitteeDeRegistration, CommitteeDeRegistrationId> {

  @Query(
      value =
          """
      SELECT max(cdr.blockTime) FROM CommitteeDeRegistration cdr
      WHERE cdr.coldKey = :coldKey
      """)
  Long getResignationBlockTimeByColdKey(@Param("coldKey") String coldKey);
}
