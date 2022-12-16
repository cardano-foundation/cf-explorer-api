package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.model.response.pool.projection.BlockOwnerProjection;
import com.cardano.explorer.model.response.pool.projection.TxBlockEpochProjection;
import com.cardano.explorer.model.response.pool.projection.TxPoolProjection;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolOwnerRepository;
import com.cardano.explorer.repository.StakeDeRegistrationRepository;
import com.cardano.explorer.repository.StakeRegistrationRepository;
import com.cardano.explorer.service.PoolRegistrationService;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class PoolRegistrationServiceImpl implements PoolRegistrationService {

  private final StakeRegistrationRepository stakeRegistrationRepository;

  private final StakeDeRegistrationRepository stakeDeRegistrationRepository;

  private final PoolOwnerRepository poolOwnerRepository;

  private final PoolHashRepository poolHashRepository;

  @Override
  public BaseFilterResponse<PoolTxResponse> getDataForPoolRegistration(Pageable pageable) {
    BaseFilterResponse<PoolTxResponse> response = new BaseFilterResponse<>();
    Page<TxBlockEpochProjection> trxBlockEpochPage = stakeRegistrationRepository.getDataForPoolRegistration(
        pageable);
    List<PoolTxResponse> poolTxRes = trxBlockEpochPage.stream().map(PoolTxResponse::new)
        .collect(Collectors.toList());
    setValueForPoolRegistration(poolTxRes);
    response.setData(poolTxRes);
    response.setTotalItems(trxBlockEpochPage.getTotalElements());
    return response;
  }

  @Override
  public BaseFilterResponse<PoolTxResponse> getDataForPoolDeRegistration(Pageable pageable) {
    BaseFilterResponse<PoolTxResponse> response = new BaseFilterResponse<>();
    Page<TxBlockEpochProjection> trxBlockEpochPage = stakeDeRegistrationRepository.getDataForPoolDeRegistration(
        pageable);
    List<PoolTxResponse> poolTxRes = trxBlockEpochPage.stream().map(PoolTxResponse::new)
        .collect(Collectors.toList());
    setValueForPoolRegistration(poolTxRes);
    response.setData(poolTxRes);
    response.setTotalItems(trxBlockEpochPage.getTotalElements());
    return response;
  }

  /**
   * set value for pool registration list
   * <p>
   */
  private void setValueForPoolRegistration(List<PoolTxResponse> poolTxRes) {
    Set<Long> blockIds = poolTxRes.stream().map(PoolTxResponse::getBlock)
        .collect(Collectors.toSet());
    List<TxPoolProjection> txPoolProjections = poolHashRepository.getDataForPoolTx(blockIds);
    Map<Long, TxPoolProjection> txPoolProjectionMap = txPoolProjections.stream()
        .collect(Collectors.toMap(TxPoolProjection::getBlockId, Function.identity()));
    List<BlockOwnerProjection> blockOwnerProjections = poolOwnerRepository.getStakeKeyList(
        blockIds);
    Map<Long, List<BlockOwnerProjection>> blockOwnerProjectionMap = blockOwnerProjections.stream()
        .collect(Collectors.groupingBy(BlockOwnerProjection::getBlockId));
    Map<Long, Set<String>> stakeKeyStrMap = new HashMap<>();
    blockOwnerProjectionMap.forEach((k, v) -> stakeKeyStrMap.put(k,
        v.stream().map(BlockOwnerProjection::getAddress).collect(Collectors.toSet())));
    poolTxRes.forEach(pool -> {
      TxPoolProjection txPool = txPoolProjectionMap.get(pool.getBlock());
      if (Objects.nonNull(txPool)) {
        pool.setPoolName(
            getNameValueFromJson(txPool.getPoolName()));
        pool.setCost(txPool.getCost());
        pool.setPledge(txPool.getPledge());
        pool.setMargin(txPool.getMargin());
      }
      pool.setStakeKey(stakeKeyStrMap.get(pool.getBlock()));
    });
  }


  /**
   * Get name value from json string pools
   *
   * @return String
   */
  private String getNameValueFromJson(String jsonName) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(jsonName))) {
      return null;
    }
    JsonObject jsonObject = new Gson().fromJson(jsonName, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }
}
