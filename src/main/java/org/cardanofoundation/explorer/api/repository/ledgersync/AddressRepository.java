package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.projection.AddressResponseProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.Address;

public interface AddressRepository extends JpaRepository<Address, Long> {

  //  @Query("SELECT address FROM Address address" + " WHERE address.addressHasScript = true")
  //  Page<Address> findAllByAddressHasScriptIsTrue(Pageable pageable);
  //  @Query("SELECT address FROM Address address" + " ORDER BY address.balance DESC")
  //  List<Address> findAllOrderByBalance(Pageable pageable);

  Optional<Address> findFirstByAddress(@Param("address") String address);

  @Query(
      value =
          """
          SELECT new org.cardanofoundation.explorer.api.model.response.address.AddressResponse
          (addr.address, CAST(COALESCE(latestAddrBalance.quantity, 0) AS biginteger))
          FROM LatestAddressBalance latestAddrBalance
          RIGHT JOIN Address addr ON addr.address = latestAddrBalance.address
          WHERE addr.stakeAddress = :stakeAddress
          """)
  Page<AddressResponse> findByStakeAddress(
      @Param("stakeAddress") String stakeAddress, Pageable pageable);

  List<Address> findAddressByIdIn(@Param("idList") Collection<Long> idList);

  @Query(value = "SELECT addr.address FROM Address addr WHERE addr.paymentCredential = :scriptHash")
  List<String> getAssociatedAddress(@Param("scriptHash") String scriptHash);

  @Query(
      value =
          """
          SELECT addr.address as address, coalesce(atc.tx_count, 0) as txCount, coalesce(ab.quantity, 0) as balance
          FROM address addr
                   LEFT JOIN address_tx_count atc ON addr.address = atc.address
                   LEFT JOIN (SELECT abv.address,
                                     abv.quantity
                              from address_balance_view abv
                              where abv.address = :address
                                and quantity > 0) ab ON ab.address = addr.address
          WHERE addr.address = :address
          """,
      nativeQuery = true)
  AddressResponseProjection getAddressDetail(@Param("address") String address);
}
