package org.cardanofoundation.explorer.api.service;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.cf_explorer_aggregator.AddressTxCountRecord;

public interface ExplorerAggregatorService {

  Optional<AddressTxCountRecord> getTxCountForAddress(String address);

}
