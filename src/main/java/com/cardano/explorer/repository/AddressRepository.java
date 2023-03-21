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

  @Query("SELECT address FROM Address address"
      + " ORDER BY address.balance DESC")
  Page<Address> findAllOrderByBalance(Pageable pageable);

  Optional<Address> findFirstByAddress(String address);

  @Query("SELECT addr FROM Address addr WHERE addr.stakeAddress.view = :stakeAddress")
  Page<Address> findByStakeAddress(String stakeAddress, Pageable pageable);

  @Query("SELECT sum(addr.balance) FROM Address addr WHERE addr.stakeAddress = :stake")
  Optional<BigDecimal> findTotalBalanceByStakeAddress(StakeAddress stake);
}
