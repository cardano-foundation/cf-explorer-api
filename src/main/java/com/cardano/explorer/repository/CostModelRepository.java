package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.CostModel;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CostModelRepository extends JpaRepository<CostModel, Long> {

}
