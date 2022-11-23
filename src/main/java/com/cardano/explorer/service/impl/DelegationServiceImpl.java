package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.DelegationHeaderResponse;
import com.cardano.explorer.model.response.PoolDetailResponse;
import com.cardano.explorer.model.response.PoolResponse;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.repository.EpochStakeRepository;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolOfflineDataRepository;
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.service.DelegationService;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardano.common.entity.Epoch;
import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import com.sotatek.cardano.common.entity.PoolUpdate;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.exceptions.enums.CommonErrorCode;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class DelegationServiceImpl implements DelegationService {

  private final DelegationRepository delegationRepository;

  private final BlockRepository blockRepository;

  private final EpochRepository epochRepository;

  private final EpochStakeRepository epochStakeRepository;

  private final PoolHashRepository poolHashRepository;

  private final PoolUpdateRepository poolUpdateRepository;

  private final PoolOfflineDataRepository poolOfflineDataRepository;

  @Override
  public ResponseEntity<DelegationHeaderResponse> getDataForDelegationHeader() {
    Epoch epoch = epochRepository.findByCurrentEpochNo()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    Integer epochNo = epoch.getNo();
    Timestamp endTime = epoch.getEndTime();
    long countDownTime = endTime.getTime() - Timestamp.from(Instant.now()).getTime();
    Integer currentSlot = blockRepository.findCurrentSlotByEpochNo(epochNo)
        .orElseThrow(() -> new BusinessException(
            CommonErrorCode.UNKNOWN_ERROR));
    BigDecimal totalStake = epochStakeRepository.totalValueStakeByEpochNo(epochNo)
        .orElseThrow(() -> new BusinessException(
            CommonErrorCode.UNKNOWN_ERROR));
    Integer delegators = delegationRepository.numberDelegators(Long.valueOf(epochNo));
    return ResponseEntity.ok(
        DelegationHeaderResponse.builder().epochNo(epochNo).epochSlotNo(currentSlot)
            .liveStake(totalStake).delegators(delegators)
            .countDownEndTime(countDownTime > 0 ? countDownTime : 0).build());
  }

  @Override
  public ResponseEntity<BaseFilterResponse<PoolResponse>> getDataForPoolTable(Integer page,
      Integer size, String search) {
    List<Long> poolIds = poolHashRepository.findAllPoolHashId(page, size, search);
    if (poolIds == null) {
      log.warn("Pool list is empty");
      return null;
    }
    BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>();
    List<PoolResponse> pools = poolIds.stream().map(PoolResponse::new)
        .collect(Collectors.toList());
    pools.forEach(pool -> {
      pool.setPledge(poolUpdateRepository.sumPledgeByPool(pool.getPoolId()));
      PoolHash poolHash = poolHashRepository.findById(pool.getPoolId())
          .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
      PoolOfflineData poolOff = poolOfflineDataRepository.findFirstByPool(poolHash)
          .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
      pool.setPoolName(getNameValueFromJson(poolOff.getJson()));
//      pool.setPoolSize(poolStakeRepository.findTotalStakeByPoolId(pool.getPoolId())
//          .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR)));
      //Todo continue processing
    });
    Long totalEle = poolHashRepository.totalPoolHashId(page, size, search);
    response.setTotalItems(totalEle);
    response.setData(pools);
    return ResponseEntity.ok(response);
  }

  @Override
  public ResponseEntity<PoolDetailResponse> getDataForPoolDetail(Long poolId) {
    PoolDetailResponse poolDetailResponse = new PoolDetailResponse();
    PoolHash poolHash = poolHashRepository.findById(poolId)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    poolDetailResponse.setPoolView(poolHash.getView());
    PoolOfflineData poolOff = poolOfflineDataRepository.findFirstByPool(poolHash)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    poolDetailResponse.setPoolName(getNameValueFromJson(poolOff.getJson()));
    poolDetailResponse.setTickerName(poolOff.getTickerName());
//    poolDetailResponse.setPoolSize(poolStakeRepository.findTotalStakeByPoolId(poolId)
//        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR)));
    poolDetailResponse.setRewardAccount(
        poolUpdateRepository.findRewardAccountByPool(poolId));//Todo confirm
    poolDetailResponse.setOwnerAccount(
        poolUpdateRepository.findOwnerAccountByPool(poolId));//Todo confirm
    poolDetailResponse.setDelegators(delegationRepository.numberDelegatorsByPool(poolId));
    poolDetailResponse.setPledge(poolUpdateRepository.sumPledgeByPool(poolId));
    PoolUpdate poolUpdate = poolUpdateRepository.findFirstByPoolHash(poolHash);
    poolDetailResponse.setCost(poolUpdate.getFixedCost());
    poolDetailResponse.setMargin(poolUpdate.getMargin() * 100);
    poolDetailResponse.setEpochBlock(epochStakeRepository.countBlockByCurrentEpoch());
    poolDetailResponse.setLifetimeBlock(epochStakeRepository.countBlockByPoolId(poolId));

    return ResponseEntity.ok(poolDetailResponse);
  }

  private String getNameValueFromJson(String json) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(json))) {
      throw new BusinessException(CommonErrorCode.UNKNOWN_ERROR);
    }
    JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }
}
