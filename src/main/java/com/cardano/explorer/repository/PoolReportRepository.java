package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.PoolReport;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PoolReportRepository extends JpaRepository<PoolReport, Long> {
}
