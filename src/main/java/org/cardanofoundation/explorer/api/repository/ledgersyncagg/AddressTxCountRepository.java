package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import java.util.Collection;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AddressTxCount;

public interface AddressTxCountRepository extends JpaRepository<AddressTxCount, String> {

  List<AddressTxCount> findAllByAddressIn(Collection<String> addressList);
}
