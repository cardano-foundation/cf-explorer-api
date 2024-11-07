package org.cardanofoundation.explorer.api.service;

import java.util.Optional;

import org.cardanofoundation.cf_explorer_aggregator.AddressTxCountRecord;
import org.cardanofoundation.cf_explorer_aggregator.PoolAggregationRecord;
import org.cardanofoundation.cf_explorer_aggregator.PoolStatusRecord;
import org.cardanofoundation.cf_explorer_aggregator.UniqueAccountRecord;

public interface ExplorerAggregatorService {

  Optional<AddressTxCountRecord> getTxCountForAddress(String address);

  Optional<UniqueAccountRecord> getUniqueAccountForEpoch(int epoch);

  Optional<PoolStatusRecord> getPoolStatusForPoolId(String poolId);

  PoolAggregationRecord getLatestPoolAggregation();

  Optional<PoolAggregationRecord> getPoolAggregationByEpoch(int epoch);
}
