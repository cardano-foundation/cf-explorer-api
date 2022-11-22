package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.request.DelegationFilterRequest;
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
import com.sotatek.cardano.common.entity.Epoch;
import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import com.sotatek.cardano.common.entity.PoolUpdate;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.exceptions.enums.CommonErrorCode;
import com.sotatek.cardanocommonapi.utils.DateUtils;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.boot.configurationprocessor.json.JSONException;
import org.springframework.boot.configurationprocessor.json.JSONObject;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
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
    Long countDownTime = Timestamp.from(Instant.now()).getTime() - endTime.getTime();
    String countDownStr = DateUtils.formatTimeToDayHourMinute(countDownTime);
    Integer currentSlot = blockRepository.findCurrentSlotByEpochNo(epochNo)
        .orElseThrow(() -> new BusinessException(
            CommonErrorCode.UNKNOWN_ERROR));
    BigDecimal totalStake = epochStakeRepository.totalValueStakeByEpochNo(epochNo)
        .orElseThrow(() -> new BusinessException(
            CommonErrorCode.UNKNOWN_ERROR));
    Integer delegators = delegationRepository.numberDelegators(Long.valueOf(epochNo))
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    return ResponseEntity.ok(
        DelegationHeaderResponse.builder().epochNo(epochNo).epochSlotNo(currentSlot)
            .liveStake(totalStake).delegators(delegators).countDownEndTime(countDownStr).build());
  }

  @Override
  public ResponseEntity<Page<PoolResponse>> getDataForPoolTable(
      DelegationFilterRequest delegationFilterRequest) {
    List<Long> poolIds = poolHashRepository.findAllPoolHashId(delegationFilterRequest);
    if (poolIds == null) {
      log.warn("Pool list is empty");
      return ResponseEntity.ok(Page.empty());
    }
    List<PoolResponse> pools = poolIds.stream().map(PoolResponse::new)
        .collect(Collectors.toList());
    pools.forEach(pool -> {
      pool.setPledge(poolUpdateRepository.sumPledgeByPool(pool.getPoolId())
          .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR)));
      PoolHash poolHash = poolHashRepository.findById(pool.getPoolId())
          .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
      PoolOfflineData poolOff = poolOfflineDataRepository.findFirstByPool(poolHash)
          .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
      pool.setPoolName(getNameValueFromJson(poolOff.getJson()));
//      pool.setPoolSize(poolStakeRepository.findTotalStakeByPoolId(pool.getPoolId())
//          .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR)));
      //Todo continue processing
    });
    Integer totalEle = poolHashRepository.countPoolHash()
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    return ResponseEntity.ok(new PageImpl<>(pools, Pageable.unpaged(), totalEle));
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
    poolDetailResponse.setRewardAccount(poolUpdateRepository.findRewardAccountByPool(poolId)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR)));
    poolDetailResponse.setOwnerAccount(poolUpdateRepository.findOwnerAccountByPool(poolId)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR)));
    poolDetailResponse.setDelegators(delegationRepository.numberDelegatorsByPool(poolId)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR)));
    poolDetailResponse.setPledge(poolUpdateRepository.sumPledgeByPool(poolId)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR)));
    PoolUpdate poolUpdate = poolUpdateRepository.findFirstByPoolHash(poolHash)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR));
    poolDetailResponse.setCost(poolUpdate.getFixedCost());
    poolDetailResponse.setMargin(poolUpdate.getMargin() * 100);
    poolDetailResponse.setLifetimeBlock(epochStakeRepository.countBlockByPoolId(poolId)
        .orElseThrow(() -> new BusinessException(CommonErrorCode.UNKNOWN_ERROR)));

    return ResponseEntity.ok(poolDetailResponse);
  }

  private String getNameValueFromJson(String json) {
    String name = null;
    try {
      final JSONObject jsonObj = new JSONObject(json);
      name = jsonObj.getString("name");
    } catch (JSONException e) {
      log.error("Error when parse json: " + e.getMessage());
    }
    return name;
  }
}
