package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.StakeAddress;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface StakeAddressRepository extends JpaRepository<StakeAddress, Long> {

  Optional<StakeAddress> findByView(String aLong);
}
