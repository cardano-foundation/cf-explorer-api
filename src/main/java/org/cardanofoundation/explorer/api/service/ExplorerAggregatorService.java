package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.cf_explorer_aggregator.AddressTxCountRecord;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;

public interface ExplorerAggregatorService {

    Optional<AddressTxCountRecord> getTxCountForAddress(String address);

    List<AddressTxCountRecord> getAllTxCount(Pageable pageable);
}
