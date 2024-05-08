package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.common.entity.ledgersync.Address;
import org.cardanofoundation.explorer.common.entity.ledgersync.AddressTxAmount;
import org.cardanofoundation.explorer.common.entity.ledgersync.AddressTxCount;

public interface AddressRepository extends JpaRepository<Address, Long> {

  //  @Query("SELECT address FROM Address address" + " WHERE address.addressHasScript = true")
  //  Page<Address> findAllByAddressHasScriptIsTrue(Pageable pageable);
  //  @Query("SELECT address FROM Address address" + " ORDER BY address.balance DESC")
  //  List<Address> findAllOrderByBalance(Pageable pageable);

  Optional<Address> findFirstByAddress(@Param("address") String address);

  //  @Query("SELECT addr FROM Address addr WHERE addr.stakeAddress.view = :stakeAddress")
  //  Page<Address> findByStakeAddress(@Param("stakeAddress") String stakeAddress, Pageable
  // pageable);
  //
  List<Address> findAddressByIdIn(@Param("idList") Collection<Long> idList);

  @Query(value = "SELECT addr.address FROM Address addr WHERE addr.paymentCredential = :scriptHash")
  List<String> getAssociatedAddress(@Param("scriptHash") String scriptHash);

  @Query(value =
      """
          SELECT new org.cardanofoundation.explorer.api.model.response.address.AddressResponse
          (addr.address, CAST(COALESCE(addrTxCount.txCount, 0) AS long), CAST(COALESCE(latestAddrBalance.quantity, 0) AS biginteger))
          FROM Address addr
          LEFT JOIN AddressTxCount addrTxCount ON addr.address = addrTxCount.address
          LEFT JOIN LatestAddressBalance latestAddrBalance ON addr.address = latestAddrBalance.address
          WHERE addr.address = :address
          """)
  AddressResponse getAddressDetail(@Param("address") String address);
}
