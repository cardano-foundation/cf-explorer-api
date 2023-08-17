package org.cardanofoundation.explorer.api.service.impl;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolTxResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolOwnerProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.TxBlockEpochProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.PoolOwnerRepository;
import org.cardanofoundation.explorer.api.repository.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.PoolRegistrationService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class PoolRegistrationServiceImpl implements PoolRegistrationService {

  private final PoolUpdateRepository poolUpdateRepository;

  private final PoolRetireRepository poolRetireRepository;

  private final PoolOwnerRepository poolOwnerRepository;

  private final TxRepository txRepository;

  @Override
  public BaseFilterResponse<PoolTxResponse> getDataForPoolRegistration(Pageable pageable) {
    BaseFilterResponse<PoolTxResponse> response = new BaseFilterResponse<>();
    Page<TxBlockEpochProjection> trxBlockEpochPage
        = poolUpdateRepository.getDataForPoolRegistration(pageable);
    List<PoolTxResponse> poolTxRes = trxBlockEpochPage.stream().map(PoolTxResponse::new).toList();
    setValueForPoolRegistration(poolTxRes);
    response.setData(poolTxRes);
    response.setTotalItems(trxBlockEpochPage.getTotalElements());
    response.setCurrentPage(pageable.getPageNumber());
    response.setTotalPages(trxBlockEpochPage.getTotalPages());
    return response;
  }

  @Override
  public BaseFilterResponse<PoolTxResponse> getDataForPoolDeRegistration(Pageable pageable) {
    BaseFilterResponse<PoolTxResponse> response = new BaseFilterResponse<>();
    Page<TxBlockEpochProjection> trxBlockEpochPage
        = poolRetireRepository.getDataForPoolDeRegistration(pageable);
    List<PoolTxResponse> poolTxRes = trxBlockEpochPage.stream().map(PoolTxResponse::new).toList();
    setValueForPoolRegistration(poolTxRes);
    response.setData(poolTxRes);
    response.setTotalItems(trxBlockEpochPage.getTotalElements());
    response.setCurrentPage(pageable.getPageNumber());
    response.setTotalPages(trxBlockEpochPage.getTotalPages());
    return response;
  }

  /**
   * set value for pool registration list
   * <p>
   */
  private void setValueForPoolRegistration(List<PoolTxResponse> poolTxRes) {
    Set<Long> poolIds = poolTxRes.stream().map(PoolTxResponse::getPoolId)
        .collect(Collectors.toSet());
    Set<Long> txIds = poolTxRes.stream().map(PoolTxResponse::getTxId)
        .collect(Collectors.toSet());
    List<TxIOProjection> txList = txRepository.findTxIn(txIds);
    Map<Long, TxIOProjection> txMap = txList.stream().collect(Collectors.toMap(TxIOProjection::getId, Function.identity()));
    List<PoolOwnerProjection> blockOwnerProjections = poolOwnerRepository.getStakeKeyList(poolIds);
    Map<Long, List<PoolOwnerProjection>> poolOwnerProjectionMap = blockOwnerProjections.stream()
        .collect(Collectors.groupingBy(PoolOwnerProjection::getPoolId));
    Map<Long, List<String>> stakeKeyStrMap = new HashMap<>();
    poolOwnerProjectionMap.forEach((k, v) -> stakeKeyStrMap.put(k,
        v.stream().map(PoolOwnerProjection::getAddress).collect(Collectors.toList())));
    poolTxRes.forEach(pool -> {
      pool.setTxHash(txMap.get(pool.getTxId()).getHash());
      pool.setBlock(txMap.get(pool.getTxId()).getBlockNo());
      pool.setEpoch(txMap.get(pool.getTxId()).getEpochNo());
      pool.setSlotNo(txMap.get(pool.getTxId()).getEpochSlotNo().longValue());
      pool.setTxTime(Timestamp.valueOf(txMap.get(pool.getTxId()).getTime()));
      List<String> stakeKeys = stakeKeyStrMap.get(pool.getPoolId());
      if (Objects.nonNull(stakeKeys)) {
        Collections.sort(stakeKeys);
        pool.setStakeKey(stakeKeys);
      }
    });
  }
}
