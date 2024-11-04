package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.cfexploreraggregator.AddressTxCountRecord;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;

public interface ExplorerAggregatorService {

    Optional<AddressTxCountRecord> getTxCountForAddress(String address);

    List<AddressTxCountRecord> getAllTxCount(Pageable pageable);
}
