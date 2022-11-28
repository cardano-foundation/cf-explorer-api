package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.SlotLeader;
import java.util.Collection;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface SlotLeaderRepository extends JpaRepository<SlotLeader, Long> {

  List<SlotLeader> findByIdIn(Collection<Long> ids);

}
