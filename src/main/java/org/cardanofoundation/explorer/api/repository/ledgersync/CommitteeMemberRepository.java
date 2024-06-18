package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.ledgersync.CommitteeMember;

public interface CommitteeMemberRepository extends JpaRepository<CommitteeMember, String> {

  @Query(value = "SELECT COUNT(cm) FROM CommitteeMember cm WHERE cm.expiredEpoch > :epochNo")
  Long countActiveMembersByExpiredEpochGreaterThan(@Param("epochNo") Integer epochNo);

  @Query(
      value =
          "SELECT MIN(cm.expiredEpoch) FROM CommitteeMember cm WHERE cm.expiredEpoch > :epochNo")
  Integer getMinExpireEpochOfActiveMembers(@Param("epochNo") Integer epochNo);

  @Query(value = "SELECT MIN(cm.slot) FROM CommitteeMember cm")
  Long getMinSlotOfCommitteeMembers();

  @Query(value = "SELECT MIN(cm.slot) FROM CommitteeMember cm WHERE cm.hash = :coldKey")
  Long getSlotOfCommitteeMemberByColdKey(@Param("coldKey") String coldKey);

  @Query(
      value =
          "SELECT DISTINCT cr.hotKey from CommitteeRegistration cr "
              + "JOIN CommitteeMember cm ON cr.coldKey = cm.hash ")
  List<String> getHotKeyOfCommitteeMember();

  @Query(
      value =
          "SELECT cr.hotKey from CommitteeRegistration cr "
              + "WHERE cr.coldKey = :coldKey ORDER BY cr.slot DESC LIMIT 1")
  String getHotKeyOfCommitteeMemberByColdKey(@Param("coldKey") String coldKey);
}
