package com.cardano.explorer.repository.custom;

import java.math.BigDecimal;
import java.sql.Date;
import java.util.List;

public interface CustomPoolHashRepository {

  List<Long> findAllPoolHashId(Integer page, Integer size, String search);

  Long totalPoolHashId(Integer page, Integer size, String search);

  List<Date> getFiveLastDateByPool(Long poolId, Integer page, Integer size);

  Long numberDelegatorsByPoolAndDateTxAndCurrentEpoch(Long poolId, Date time, Integer epochNo);

  BigDecimal amountStakeByPoolAndDateTxAndCurrentEpoch(Long poolId, Date time, Integer epochNo);

  Long getMinOrMaxDelegatorsByPoolAndCurrentEpoch(Long poolId, Integer epochNo, Integer type);
}
