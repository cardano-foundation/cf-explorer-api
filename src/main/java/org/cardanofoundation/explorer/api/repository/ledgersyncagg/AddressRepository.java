package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.AddressResponseProjection;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.Address;

public interface AddressRepository extends JpaRepository<Address, Long> {

  Optional<Address> findFirstByAddress(@Param("address") String address);

  @Query(
      value =
          """
          SELECT ab.address, ab.quantity FROM address addr
          CROSS JOIN LATERAL ( SELECT tmp.address,
                                tmp.quantity
                         FROM address_balance tmp
                         WHERE tmp.address = addr.address
                           AND tmp.unit = 'lovelace'
                         ORDER BY tmp.slot DESC
                         LIMIT 1) ab
          WHERE addr.stake_address = :stakeAddress
          ORDER BY ab.quantity DESC
          """, nativeQuery = true)
  Page<AddressResponseProjection> findByStakeAddress(
      @Param("stakeAddress") String stakeAddress, Pageable pageable);

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
                              FROM address_balance_view abv
                              WHERE abv.address = :address
                                AND abv.unit = 'lovelace') ab ON ab.address = addr.address
          WHERE addr.address = :address
          """,
      nativeQuery = true)
  AddressResponseProjection getAddressDetail(@Param("address") String address);
}
