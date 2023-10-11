package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.Address_;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AddressRepository extends JpaRepository<Address, Long> {

  @Query("SELECT address FROM Address address"
      + " WHERE address.addressHasScript = true")
  Page<Address> findAllByAddressHasScriptIsTrue(Pageable pageable);

  @Query("SELECT address FROM Address address"
      + " ORDER BY address.balance DESC")
  List<Address> findAllOrderByBalance(Pageable pageable);

  @EntityGraph(attributePaths = {Address_.STAKE_ADDRESS})
  Optional<Address> findFirstByAddress(@Param("address") String address);

  @Query("SELECT addr FROM Address addr WHERE addr.stakeAddress.view = :stakeAddress")
  Page<Address> findByStakeAddress(@Param("stakeAddress") String stakeAddress, Pageable pageable);

  List<Address> findAddressByIdIn(@Param("idList") Collection<Long> idList);
}
