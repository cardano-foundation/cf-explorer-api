package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.StakeAddress;
import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressRepository extends JpaRepository<Address, Long> {

  @Query("SELECT address FROM Address address"
      + " WHERE address.addressHasScript = true"
      + " ORDER BY address.balance DESC")
  List<Address> findAllByAddressHasScriptIsTrue(Pageable pageable);

  @Query("SELECT address FROM Address address"
      + " ORDER BY address.balance DESC")
  List<Address> findAllOrderByBalance(Pageable pageable);

  Optional<Address> findFirstByAddress(String address);

  @Query("SELECT addr FROM Address addr WHERE addr.stakeAddress.view = :stakeAddress")
  Page<Address> findByStakeAddress(String stakeAddress, Pageable pageable);

  @Query("SELECT addr FROM Address addr WHERE addr.stakeAddress.view = :stakeAddress")
  List<Address> findByStakeAddress(String stakeAddress);

  @Query("SELECT sum(addr.balance) FROM Address addr WHERE addr.stakeAddress = :stake")
  Optional<BigInteger> findTotalBalanceByStakeAddress(StakeAddress stake);

  List<Address> findAddressByIdIn(Collection<Long> idList);
}
