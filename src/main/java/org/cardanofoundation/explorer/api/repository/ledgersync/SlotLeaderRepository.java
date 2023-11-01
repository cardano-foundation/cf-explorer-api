package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.consumercommon.entity.SlotLeader;
import java.util.Collection;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

public interface SlotLeaderRepository extends JpaRepository<SlotLeader, Long> {

  List<SlotLeader> findByIdIn(@Param("ids") Collection<Long> ids);

}
