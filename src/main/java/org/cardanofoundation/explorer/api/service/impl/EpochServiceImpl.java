package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;

import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import org.cardanofoundation.explorer.api.common.enumeration.EpochStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.mapper.EpochMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.EpochResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.springframework.util.CollectionUtils;

import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class EpochServiceImpl implements EpochService {

  private final EpochRepository epochRepository;
  private final EpochMapper epochMapper;
  private final RedisTemplate<String, Object> redisTemplate;
  private final FetchRewardDataService fetchRewardDataService;
  private static final String UNIQUE_ACCOUNTS_KEY = "UNIQUE_ACCOUNTS";
  private static final String UNDERSCORE = "_";

  @Value("${application.network}")
  private String network;

  @Value("${application.epoch.days}")
  public int EPOCH_DAYS;

  @Override
  @Transactional(readOnly = true)
  public EpochResponse getEpochDetail(String no) {
    try {
      Integer epochNo = Integer.parseInt(no);
      Epoch epoch = epochRepository.findFirstByNo(epochNo).orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND)
      );
      var currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
      if (!fetchRewardDataService.checkEpochRewardDistributed(epoch) && epoch.getNo() < currentEpoch - 1 ) {
        List<Epoch> fetchEpochResponse = fetchRewardDataService.fetchEpochRewardDistributed(List.of(epochNo));
        if (CollectionUtils.isEmpty(fetchEpochResponse)) {
          throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
        }
        epoch.setRewardsDistributed(fetchEpochResponse.get(0).getRewardsDistributed());
      }
      Epoch firstEpoch = epochRepository.findFirstByNo(BigInteger.ZERO.intValue())
          .orElseThrow(() -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
      LocalDateTime firstEpochStartTime = firstEpoch.getStartTime().toLocalDateTime();
      EpochResponse response = epochMapper.epochToEpochResponse(epoch);
      checkEpochStatus(response, currentEpoch);
      response.setStartTime(modifyStartTimeAndEndTimeOfEpoch(firstEpochStartTime, response.getStartTime()));
      response.setEndTime(modifyStartTimeAndEndTimeOfEpoch(firstEpochStartTime, response.getEndTime()));
      String uniqueAccountRedisKey = String.join(
          UNDERSCORE,
          getRedisKey(UNIQUE_ACCOUNTS_KEY),
          epoch.getNo().toString());
      Integer account = redisTemplate.opsForHash().size(uniqueAccountRedisKey).intValue();
      response.setAccount(account);
      return response;
    } catch (NumberFormatException e) {
      throw new BusinessException(BusinessCode.EPOCH_NOT_FOUND);
    }
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<EpochResponse> getAllEpoch(Pageable pageable) {

    Epoch firstEpoch = epochRepository.findFirstByNo(BigInteger.ZERO.intValue())
        .orElseThrow(() -> new NoContentException(BusinessCode.EPOCH_NOT_FOUND));
    LocalDateTime firstEpochStartTime = firstEpoch.getStartTime().toLocalDateTime();

    Page<Epoch> epochs = epochRepository.findAll(pageable);
    var epochNeedFetch =  epochs.getContent().stream().filter(
        epoch -> !fetchRewardDataService.checkEpochRewardDistributed(epoch)
    ).map(Epoch::getNo).toList();
    if (!CollectionUtils.isEmpty(epochNeedFetch)) {
      List<Epoch> fetchEpochList = fetchRewardDataService.fetchEpochRewardDistributed(epochNeedFetch);
      if (fetchEpochList == null) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
      Map<Integer, BigInteger> epochRewardMap
          = StreamUtil.toMap(fetchEpochList, Epoch::getNo, Epoch::getRewardsDistributed);
      epochs.forEach(epoch -> {
        if(epochRewardMap.containsKey(epoch.getNo())) {
          epoch.setRewardsDistributed(epochRewardMap.get(epoch.getNo()));
        }
      });
    }

    Page<EpochResponse> pageResponse = epochs.map(epochMapper::epochToEpochResponse);
    var currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow(
        () -> new NoContentException(BusinessCode.EPOCH_NOT_FOUND));
    pageResponse.getContent().forEach(epoch -> {
      checkEpochStatus(epoch, currentEpoch);
      epoch.setStartTime(modifyStartTimeAndEndTimeOfEpoch(firstEpochStartTime, epoch.getStartTime()));
      epoch.setEndTime(modifyStartTimeAndEndTimeOfEpoch(firstEpochStartTime, epoch.getEndTime()));
      String uniqueAccountRedisKey = String.join(
          UNDERSCORE,
          getRedisKey(UNIQUE_ACCOUNTS_KEY),
          epoch.getNo().toString());
      epoch.setAccount(redisTemplate.opsForHash().size(uniqueAccountRedisKey).intValue());
    });
    return new BaseFilterResponse<>(pageResponse);
  }

  /**
   * Set time of epoch belongs to start time of first epoch
   * Set hour, minute, second of epoch belongs to hour, minute, second of first epoch
   * @param firstEpochStartTime start time of first epoch
   * @param epochTime start time or end time of epoch
   *
   * @return epoch time after modify
   */
  private LocalDateTime modifyStartTimeAndEndTimeOfEpoch(LocalDateTime firstEpochStartTime, LocalDateTime epochTime) {
    return LocalDateTime.of(
        epochTime.getYear(),
        epochTime.getMonth(),
        epochTime.getDayOfMonth(),
        firstEpochStartTime.getHour(),
        firstEpochStartTime.getMinute(),
        firstEpochStartTime.getSecond());
  }

  /**
   * Get epoch status from start time and end time
   *
   * <p>Start time < now < end time : in progress</p>
   * <p>End time > now - 10 day and not in progress: rewarding</p>
   * <p>Others: finished</p>
   *
   * @param epoch epoch response
   */
  private void checkEpochStatus(EpochResponse epoch, Integer currentEpoch) {
    if (epoch.getStartTime().plusDays(EPOCH_DAYS).isAfter(LocalDateTime.now(ZoneOffset.UTC))
        && epoch.getStartTime().isBefore(LocalDateTime.now(ZoneOffset.UTC))) {
      epoch.setStatus(EpochStatus.IN_PROGRESS);
      epoch.setEndTime(epoch.getStartTime().plusDays(EPOCH_DAYS));
    } else {
      epoch.setStatus(EpochStatus.FINISHED);
    }
    if (!EpochStatus.IN_PROGRESS.equals(epoch.getStatus()) && currentEpoch.equals(epoch.getNo())) {
      epoch.setStatus(EpochStatus.SYNCING);
    }
  }

  @Override
  @Transactional(readOnly = true)
  public EpochSummary getCurrentEpochSummary() {

    return epochRepository
        .findCurrentEpochSummary()
        .map(epochSummaryProjection -> {
          var currentLocalDateTime = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);
          var epochStartTime = epochSummaryProjection.getStartTime().toLocalDateTime();
          Epoch firstEpoch = epochRepository.findFirstByNo(BigInteger.ZERO.intValue())
              .orElseThrow(() -> new NoContentException(BusinessCode.EPOCH_NOT_FOUND));
          LocalDateTime firstEpochStartTime = firstEpoch.getStartTime().toLocalDateTime();
          epochStartTime = modifyStartTimeAndEndTimeOfEpoch(firstEpochStartTime, epochStartTime);
          var slot =
              currentLocalDateTime.toEpochSecond(ZoneOffset.UTC) - epochStartTime.toEpochSecond(ZoneOffset.UTC);
          String uniqueAccountRedisKey = String.join(
              UNDERSCORE,
              getRedisKey(UNIQUE_ACCOUNTS_KEY),
              epochSummaryProjection.getNo().toString());
          var account = redisTemplate.opsForHash().size(uniqueAccountRedisKey).intValue();

          return EpochSummary.builder()
              .no(epochSummaryProjection.getNo())
              .slot((int) slot)
              .totalSlot(epochSummaryProjection.getMaxSlot())
              .startTime(epochStartTime)
              .endTime(epochStartTime.plusDays(EPOCH_DAYS))
              .account(account)
              .build();
        })
        .orElse(EpochSummary.builder().
            slot(0).
            no(0).
            totalSlot(0).
            build());
  }

  private String getRedisKey(String key) {
    return String.join(UNDERSCORE, network.toUpperCase(), key);
  }
}
