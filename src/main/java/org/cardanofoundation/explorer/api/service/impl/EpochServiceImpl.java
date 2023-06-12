package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;

import jakarta.annotation.PostConstruct;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import org.cardanofoundation.explorer.api.common.enumeration.EpochStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.EpochMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.EpochResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.projection.UniqueAddressProjection;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;

@Service
@RequiredArgsConstructor
public class EpochServiceImpl implements EpochService {

  public static final int EPOCH_DAYS = 5;
  private final EpochRepository epochRepository;
  private final EpochMapper epochMapper;
  private final RedisTemplate<String, Object> redisTemplate;
  private static final String UNIQUE_ACCOUNTS = "UNIQUE_ACCOUNTS";
  private static final String MAX_TRANSACTION_ID = "MAX_TRANSACTION_ID";
  public static final long ONE_EPOCH = 5L;
  public static final int NOT_EXPIRE = -1;

  @Value("${application.network}")
  private String network;

  @Override
  @Transactional(readOnly = true)
  public EpochResponse getEpochDetail(String no) {
    try {
      Integer epochNo = Integer.parseInt(no);
      Epoch epoch = epochRepository.findFirstByNo(epochNo).orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND)
      );
      EpochResponse response = epochMapper.epochToEpochResponse(epoch);
      var currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
      checkEpochStatus(response, currentEpoch);
      return response;
    } catch (NumberFormatException e) {
      throw new BusinessException(BusinessCode.EPOCH_NOT_FOUND);
    }
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<EpochResponse> getAllEpoch(Pageable pageable) {
    Page<Epoch> epochs = epochRepository.findAll(pageable);
    Page<EpochResponse> pageResponse = epochs.map(epochMapper::epochToEpochResponse);
    var currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow(
        () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
    pageResponse.getContent().forEach(epoch -> checkEpochStatus(epoch, currentEpoch));
    return new BaseFilterResponse<>(pageResponse);
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
    var rewardTime = LocalDateTime.now().minusDays(10);
    if (epoch.getStartTime().plusDays(EPOCH_DAYS).isAfter(LocalDateTime.now(ZoneId.of("UTC")))
        && epoch.getStartTime().isBefore(LocalDateTime.now(ZoneId.of("UTC")))) {
      epoch.setStatus(EpochStatus.IN_PROGRESS);
      epoch.setEndTime(epoch.getStartTime().plusDays(EPOCH_DAYS));
    } else if (rewardTime.isBefore(epoch.getEndTime())) {
      epoch.setStatus(EpochStatus.REWARDING);
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
          var epochStartTime = LocalDateTime.ofInstant(
              epochSummaryProjection.getStartTime().toInstant(), ZoneOffset.UTC);
          var slot =
              currentLocalDateTime.toEpochSecond(ZoneOffset.UTC) - epochStartTime.toEpochSecond(
                  ZoneOffset.UTC);

          Long startFromId = BigInteger.ZERO.longValue();
          final String redisKey = getRedisKey(UNIQUE_ACCOUNTS, epochSummaryProjection.getNo());
          final Long cacheSize = redisTemplate.opsForHash().size(redisKey);

          if (cacheSize > BigInteger.ZERO.longValue()) {
            Object maxTransaction = redisTemplate.opsForHash()
                .get(redisKey, MAX_TRANSACTION_ID);

            if (Objects.isNull(maxTransaction)) {
              maxTransaction = redisTemplate.opsForHash().values(redisKey)
                  .stream()
                  .filter(Objects::nonNull)
                  .map(Integer.class::cast)
                  .max(Integer::compareTo)
                  .orElse(BigInteger.ZERO.intValue());
              redisTemplate.opsForHash().put(redisKey, MAX_TRANSACTION_ID, maxTransaction);
            }

            startFromId = Long.parseLong(Objects.requireNonNull(String.valueOf(maxTransaction)));
          }

          // TODO: handle rollback case

          List<UniqueAddressProjection> uniqueAddress = epochRepository.getTotalAccountsAtEpoch(
              epochSummaryProjection.getNo(), startFromId);

          AtomicLong maxTxId = new AtomicLong(startFromId);

          uniqueAddress.forEach(unique -> {
            redisTemplate.opsForHash()
                .put(redisKey, unique.getAddress(), unique.getId());
            maxTxId.set(maxTxId.get() < unique.getId() ? unique.getId() : maxTxId.get());
          });

          redisTemplate.opsForHash().put(redisKey, MAX_TRANSACTION_ID, maxTxId.get());

          var expire = redisTemplate.getExpire(
              getRedisKey(UNIQUE_ACCOUNTS, epochSummaryProjection.getNo()));

          if (expire == NOT_EXPIRE) {
            redisTemplate.expire(redisKey, Duration.ofDays(ONE_EPOCH));
          }

          Integer account = redisTemplate.opsForHash().size(redisKey).intValue();
          account =
              account > BigInteger.ONE.intValue() ? account - BigInteger.ONE.intValue() : account;

          return EpochSummary.builder()
              .no(epochSummaryProjection.getNo())
              .slot((int) slot)
              .totalSlot(epochSummaryProjection.getMaxSlot())
              .startTime(epochSummaryProjection.getStartTime().toLocalDateTime())
              .endTime(epochSummaryProjection.getStartTime().toLocalDateTime().plusDays(EPOCH_DAYS))
              .account(account)
              .build();
        })
        .orElse(EpochSummary.builder().
            slot(0).
            no(0).
            totalSlot(0).
            build());
  }


  /**
   * create redis key
   *
   * @param rawKey
   * @return
   */
  private String getRedisKey(String rawKey, Integer epoch) {
    return String.join("_", this.network, rawKey, String.valueOf(epoch));
  }

  @PostConstruct
  void setUp() {
    getCurrentEpochSummary();
  }
}
