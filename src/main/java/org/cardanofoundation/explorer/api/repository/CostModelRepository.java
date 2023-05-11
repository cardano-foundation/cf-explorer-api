package org.cardanofoundation.explorer.api.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.consumercommon.entity.CostModel;


@Repository
public interface CostModelRepository extends JpaRepository<CostModel, Long> {

}
