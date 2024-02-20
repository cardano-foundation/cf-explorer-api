package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.*;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.EpochStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.exception.NoContentException;
import org.cardanofoundation.explorer.api.mapper.EpochMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.EpochResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;
import org.cardanofoundation.explorer.common.entity.ledgersync.Epoch;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
public class EpochServiceImpl implements EpochService {

  private final EpochRepository epochRepository;
  private final BlockRepository blockRepository;
  private final EpochMapper epochMapper;
  private final RedisTemplate<String, Object> redisTemplate;
  private final FetchRewardDataService fetchRewardDataService;
  private final AdaPotsRepository adaPotsRepository;
  private static final String UNIQUE_ACCOUNTS_KEY = "UNIQUE_ACCOUNTS";
  private static final String UNDERSCORE = "_";

  @Value("${application.network}")
  private String network;

  @Value("${application.epoch.days}")
  public int epochDays;

  @Value("${application.healthcheck.block-time-threshold}")
  private Long blockTimeThresholdInSecond;

  @Override
  public EpochResponse getEpochDetail(String no) {
    try {
      Integer epochNo = Integer.parseInt(no);
      Epoch epoch =
          epochRepository
              .findFirstByNo(epochNo)
              .orElseThrow(() -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
      var currentEpoch =
          epochRepository
              .findCurrentEpochNo()
              .orElseThrow(() -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
      if (Boolean.FALSE.equals(fetchRewardDataService.checkEpochRewardDistributed(epoch))
          && epoch.getNo() < currentEpoch - 1) {
        List<Epoch> fetchEpochResponse =
            fetchRewardDataService.fetchEpochRewardDistributed(List.of(epochNo));
        if (!CollectionUtils.isEmpty(fetchEpochResponse)) {
          epoch.setRewardsDistributed(fetchEpochResponse.get(0).getRewardsDistributed());
        }
      }
      Epoch firstEpoch =
          epochRepository
              .findFirstByNo(BigInteger.ZERO.intValue())
              .orElseThrow(() -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));

      Block currentBlock =
          blockRepository
              .findLatestBlock()
              .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));

      LocalDateTime firstEpochStartTime = firstEpoch.getStartTime().toLocalDateTime();
      EpochResponse response = epochMapper.epochToEpochResponse(epoch);
      checkEpochStatus(response, currentBlock, currentEpoch);
      LocalDateTime startTime =
          modifyStartTimeAndEndTimeOfEpoch(firstEpochStartTime, response.getStartTime());
      response.setStartTime(startTime);
      response.setEndTime(startTime.plusDays(epochDays));
      String uniqueAccountRedisKey =
          String.join(UNDERSCORE, getRedisKey(UNIQUE_ACCOUNTS_KEY), epoch.getNo().toString());
      Integer account = redisTemplate.opsForHash().size(uniqueAccountRedisKey).intValue();
      response.setAccount(account);
      return response;
    } catch (NumberFormatException e) {
      throw new BusinessException(BusinessCode.EPOCH_NOT_FOUND);
    }
  }

  @Override
  public BaseFilterResponse<EpochResponse> getAllEpoch(Pageable pageable) {
    Block currentBlock =
        blockRepository
            .findLatestBlock()
            .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));

    Epoch firstEpoch =
        epochRepository
            .findFirstByNo(BigInteger.ZERO.intValue())
            .orElseThrow(() -> new NoContentException(BusinessCode.EPOCH_NOT_FOUND));
    LocalDateTime firstEpochStartTime = firstEpoch.getStartTime().toLocalDateTime();

    var currentEpoch =
        epochRepository
            .findCurrentEpochNo()
            .orElseThrow(() -> new NoContentException(BusinessCode.EPOCH_NOT_FOUND));

    Page<Epoch> epochs = epochRepository.findAll(pageable);
    var epochNeedFetch =
        epochs.getContent().stream()
            .filter(
                epoch ->
                    !fetchRewardDataService.checkEpochRewardDistributed(epoch)
                        && epoch.getNo() < currentEpoch - 1)
            .map(Epoch::getNo)
            .toList();
    if (!CollectionUtils.isEmpty(epochNeedFetch)) {
      List<Epoch> fetchEpochList =
          fetchRewardDataService.fetchEpochRewardDistributed(epochNeedFetch);
      if (!CollectionUtils.isEmpty(fetchEpochList)) {
        Map<Integer, BigInteger> epochRewardMap =
            StreamUtil.toMap(fetchEpochList, Epoch::getNo, Epoch::getRewardsDistributed);
        epochs.forEach(
            epoch -> {
              if (epochRewardMap.containsKey(epoch.getNo())) {
                epoch.setRewardsDistributed(epochRewardMap.get(epoch.getNo()));
              }
            });
      }
    }

    Page<EpochResponse> pageResponse = epochs.map(epochMapper::epochToEpochResponse);
    pageResponse
        .getContent()
        .forEach(
            epoch -> {
              checkEpochStatus(epoch, currentBlock, currentEpoch);
              LocalDateTime startTime =
                  modifyStartTimeAndEndTimeOfEpoch(firstEpochStartTime, epoch.getStartTime());
              epoch.setStartTime(startTime);
              epoch.setEndTime(startTime.plusDays(epochDays));
              String uniqueAccountRedisKey =
                  String.join(
                      UNDERSCORE, getRedisKey(UNIQUE_ACCOUNTS_KEY), epoch.getNo().toString());
              epoch.setAccount(redisTemplate.opsForHash().size(uniqueAccountRedisKey).intValue());
            });
    return new BaseFilterResponse<>(pageResponse);
  }

  /**
   * Set time of epoch belongs to start time of first epoch Set hour, minute, second of epoch
   * belongs to hour, minute, second of first epoch
   *
   * @param firstEpochStartTime start time of first epoch
   * @param epochTime start time or end time of epoch
   * @return epoch time after modify
   */
  private LocalDateTime modifyStartTimeAndEndTimeOfEpoch(
      LocalDateTime firstEpochStartTime, LocalDateTime epochTime) {
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
   * <p>Start time < now < end time : in progress
   *
   * <p>End time > now - 10 day and not in progress: rewarding
   *
   * <p>Others: finished
   *
   * @param epoch epoch response
   */
  private void checkEpochStatus(EpochResponse epoch, Block currentBlock, int currentEpochNo) {
    LocalDateTime currentTime = LocalDateTime.now(ZoneOffset.UTC);
    if (epoch.getStartTime().plusDays(epochDays).isAfter(currentTime)
        && epoch.getStartTime().isBefore(currentTime)) {
      epoch.setStatus(EpochStatus.IN_PROGRESS);
      epoch.setEndTime(epoch.getStartTime().plusDays(epochDays));
    } else {
      epoch.setStatus(EpochStatus.FINISHED);
    }

    if (epoch.getNo().equals(currentEpochNo)
        && blockTimeThresholdInSecond
            <= ChronoUnit.SECONDS.between(currentBlock.getTime().toLocalDateTime(), currentTime)) {
      epoch.setStatus(EpochStatus.SYNCING);
    }
  }

  @Override
  public EpochSummary getCurrentEpochSummary() {
    return epochRepository
        .findCurrentEpochSummary()
        .map(
            epochSummaryProjection -> {
              var currentLocalDateTime = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);
              var epochStartTime = epochSummaryProjection.getStartTime().toLocalDateTime();
              Epoch firstEpoch =
                  epochRepository
                      .findFirstByNo(BigInteger.ZERO.intValue())
                      .orElseThrow(() -> new NoContentException(BusinessCode.EPOCH_NOT_FOUND));
              LocalDateTime firstEpochStartTime = firstEpoch.getStartTime().toLocalDateTime();
              epochStartTime =
                  modifyStartTimeAndEndTimeOfEpoch(firstEpochStartTime, epochStartTime);
              var slot =
                  currentLocalDateTime.toEpochSecond(ZoneOffset.UTC)
                      - epochStartTime.toEpochSecond(ZoneOffset.UTC);
              String uniqueAccountRedisKey =
                  String.join(
                      UNDERSCORE,
                      getRedisKey(UNIQUE_ACCOUNTS_KEY),
                      epochSummaryProjection.getNo().toString());
              var account = redisTemplate.opsForHash().size(uniqueAccountRedisKey).intValue();
              if (Boolean.FALSE.equals(
                  fetchRewardDataService.checkAdaPots(epochSummaryProjection.getNo()))) {
                fetchRewardDataService.fetchAdaPots(List.of(epochSummaryProjection.getNo()));
              }
              var circulatingSupply =
                  adaPotsRepository.getReservesByEpochNo(epochSummaryProjection.getNo());
              if (Objects.isNull(circulatingSupply)) {
                circulatingSupply = BigInteger.ZERO;
              }
              return EpochSummary.builder()
                  .no(epochSummaryProjection.getNo())
                  .slot((int) slot)
                  .totalSlot(epochSummaryProjection.getMaxSlot())
                  .startTime(epochStartTime)
                  .endTime(epochStartTime.plusDays(epochDays))
                  .account(account)
                  .circulatingSupply(
                      CommonConstant.TOTAL_ADA.toBigInteger().subtract(circulatingSupply))
                  .blkCount(epochSummaryProjection.getBlkCount())
                  .build();
            })
        .orElse(EpochSummary.builder().slot(0).no(0).totalSlot(0).build());
  }

  private String getRedisKey(String key) {
    return String.join(UNDERSCORE, network.toUpperCase(), key);
  }
}
