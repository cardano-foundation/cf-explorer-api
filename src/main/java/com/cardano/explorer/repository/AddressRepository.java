package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.StakeAddress;
import java.math.BigDecimal;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressRepository extends JpaRepository<Address, Long> {

  @Query("SELECT address FROM Address address"
      + " WHERE address.addressHasScript = true"
      + " ORDER BY address.balance DESC")
  Page<Address> findAllByAddressHasScriptIsTrue(Pageable pageable);

  Optional<Address> findFirstByAddress(String address);

  @Query("SELECT sum(addr.balance) "
      + " FROM Address addr "
      + " WHERE addr.stakeAddress = :stakeAddress")
  Optional<BigDecimal> getBalanceByStakeAddress(StakeAddress stakeAddress);
}
