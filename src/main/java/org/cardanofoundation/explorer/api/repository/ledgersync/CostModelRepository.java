package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.common.entity.ledgersync.CostModel;

@Repository
public interface CostModelRepository extends JpaRepository<CostModel, Long> {}
