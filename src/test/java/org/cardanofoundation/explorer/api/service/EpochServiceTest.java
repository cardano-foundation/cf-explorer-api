package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;

import org.cardanofoundation.explorer.api.common.enumeration.EpochStatus;
import org.cardanofoundation.explorer.api.mapper.EpochMapper;
import org.cardanofoundation.explorer.api.model.response.EpochResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.service.impl.EpochServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.EpochSummaryProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.UniqueAccountProjectionImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.enumeration.EraType;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
class EpochServiceTest {

  @Mock
  EpochRepository epochRepository;
  @Mock
  EpochMapper epochMapper;
  @InjectMocks
  EpochServiceImpl epochService;
  @Mock
  RedisTemplate<String, Object> redisTemplate;
  @Mock
  HashOperations hashOperations;

  //TODO getAllEpoch

  //TODO getCurrentEpochSummary
  @Test
  void testCurrentEpochSummary() {

    var currentLocalDateTime = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);
    var localDate = LocalDateTime.of(LocalDate.now(), LocalTime.of(0, 0));

    when(epochRepository.findCurrentEpochSummary())
        .thenReturn(Optional.of(EpochSummaryProjectionImpl.builder()
            .no(30)
            .maxSlot(432000)
            .statTime(Timestamp.valueOf(localDate))
            .build()));

    when(epochRepository.getTotalAccountsAtEpoch(
        any(), any()))
        .thenReturn(List.of(UniqueAccountProjectionImpl.builder()
            .id(1L)
            .address("1")
            .build()));

    when(redisTemplate.opsForHash())
        .thenReturn(hashOperations);

    when(hashOperations.size(any()))
        .thenReturn(0l);

    when(hashOperations.size(any()))
        .thenReturn(1L);

    var time = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    EpochSummary epochSummary = epochService.getCurrentEpochSummary();

    EpochSummary expect = EpochSummary.builder()
        .no(30)
        .slot(0)
        .totalSlot(432000)
        .account(1)
        .build();
    Assertions.assertEquals(expect.getNo(), epochSummary.getNo());
    Assertions.assertEquals(expect.getAccount(), epochSummary.getAccount());
    Assertions.assertEquals(expect.getTotalSlot(), epochSummary.getTotalSlot());
  }

  // function getEpochDetail test
  @Test
  void testGetEpochDetailInProgress() {
    Epoch epoch = Epoch.builder()
        .era(EraType.ALONZO)
        .id(1L)
        .no(1)
        .blkCount(1)
        .txCount(2)
        .fees(BigInteger.ONE)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        .rewardsDistributed(BigInteger.ONE)
        .startTime(Timestamp.valueOf(LocalDateTime.now()))
        .endTime(Timestamp.valueOf(LocalDateTime.now()))
        .build();

    EpochResponse expect = EpochResponse
        .builder()
        .no(1)
        .blkCount(1)
        .txCount(2)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        .startTime(epoch.getStartTime().toLocalDateTime())
        .endTime(epoch.getEndTime().toLocalDateTime())
        //.rewardsDistributed(BigInteger.ONE)
        .build();

    when(epochRepository.findFirstByNo(any(Integer.class)))
        .thenReturn(Optional.of(epoch));

    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.ofNullable(epoch.getNo()));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(expect);

    EpochResponse actual = epochService.getEpochDetail("1");
    expect.setStatus(EpochStatus.IN_PROGRESS);
    Assertions.assertEquals(expect, actual);
  }

  @Test
  void testGetEpochDetailSync() {
    Epoch epoch = Epoch.builder()
        .era(EraType.ALONZO)
        .id(1L)
        .no(1)
        .blkCount(1)
        .txCount(2)
        .fees(BigInteger.ONE)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        .rewardsDistributed(BigInteger.ONE)
        .startTime(Timestamp.valueOf(LocalDateTime.now()))
        .endTime(Timestamp.valueOf(LocalDateTime.now().minusDays(5)))
        .build();

    EpochResponse expect = EpochResponse
        .builder()
        .no(1)
        .blkCount(1)
        .txCount(2)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        .startTime(epoch.getStartTime().toLocalDateTime())
        .endTime(epoch.getEndTime().toLocalDateTime())
        //.rewardsDistributed(BigInteger.ONE)
        .build();

    when(epochRepository.findFirstByNo(any(Integer.class)))
        .thenReturn(Optional.of(epoch));

    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.ofNullable(epoch.getNo()));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(expect);

    EpochResponse actual = epochService.getEpochDetail("1");
    expect.setStatus(EpochStatus.SYNCING);
    Assertions.assertEquals(expect, actual);
  }

  @Test
  void testGetEpochDetailDone() {
    Epoch epoch = Epoch.builder()
        .era(EraType.ALONZO)
        .id(1L)
        .no(1)
        .blkCount(1)
        .txCount(2)
        .fees(BigInteger.ONE)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        //.rewardsDistributed(BigInteger.ONE)
        .startTime(Timestamp.valueOf(LocalDateTime.now()))
        .endTime(Timestamp.valueOf(LocalDateTime.now()))
        .build();

    EpochResponse expect = EpochResponse
        .builder()
        .no(1)
        .blkCount(1)
        .txCount(2)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        .startTime(epoch.getStartTime().toLocalDateTime())
        .endTime(epoch.getEndTime().toLocalDateTime())
        //.rewardsDistributed(BigInteger.ONE)
        .build();

    when(epochRepository.findFirstByNo(any(Integer.class)))
        .thenReturn(Optional.of(epoch));

    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(epoch.getNo() + 1));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(expect);

    EpochResponse actual = epochService.getEpochDetail("1");
    expect.setStatus(EpochStatus.FINISHED);
    Assertions.assertEquals(expect, actual);
  }

  @Test
  void testGetEpochDetailReward() {
    Epoch epoch = Epoch.builder()
        .era(EraType.ALONZO)
        .id(1L)
        .no(1)
        .blkCount(1)
        .txCount(2)
        .fees(BigInteger.ONE)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        //.rewardsDistributed(BigInteger.ONE)
        .startTime(Timestamp.valueOf(LocalDateTime.now()))
        .endTime(Timestamp.valueOf(LocalDateTime.now().minusDays(10)))
        .build();

    EpochResponse expect = EpochResponse
        .builder()
        .no(1)
        .blkCount(1)
        .txCount(2)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        .startTime(epoch.getStartTime().toLocalDateTime())
        .endTime(epoch.getEndTime().toLocalDateTime())
        //.rewardsDistributed(BigInteger.ONE)
        .build();

    when(epochRepository.findFirstByNo(any(Integer.class)))
        .thenReturn(Optional.of(epoch));

    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(epoch.getNo() + 1));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(expect);

    EpochResponse actual = epochService.getEpochDetail("1");
    expect.setStatus(EpochStatus.REWARDING);
    Assertions.assertEquals(expect, actual);
  }

  @Test
  void testGetEpochDetailThrowCauseNumberFormatException() {
    Epoch epoch = Epoch.builder()
        .era(EraType.ALONZO)
        .id(1L)
        .no(1)
        .blkCount(1)
        .txCount(2)
        .fees(BigInteger.ONE)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        //.rewardsDistributed(BigInteger.ONE)
        .startTime(Timestamp.valueOf(LocalDateTime.now()))
        .endTime(Timestamp.valueOf(LocalDateTime.now()))
        .build();

    EpochResponse expect = EpochResponse
        .builder()
        .no(1)
        .blkCount(1)
        .txCount(2)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        //.rewardsDistributed(BigInteger.ONE)
        .status(EpochStatus.IN_PROGRESS)
        .build();

    Assertions.assertThrows(BusinessException.class, () ->
        epochService.getEpochDetail(UUID.randomUUID().toString()));
  }

  @Test
  void testGetEpochDetailThrowCauseNotFound() {
    Epoch epoch = Epoch.builder()
        .era(EraType.ALONZO)
        .id(1L)
        .no(1)
        .blkCount(1)
        .txCount(2)
        .fees(BigInteger.ONE)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        //.rewardsDistributed(BigInteger.ONE)
        .startTime(Timestamp.valueOf(LocalDateTime.now()))
        .endTime(Timestamp.valueOf(LocalDateTime.now()))
        .build();

    EpochResponse expect = EpochResponse
        .builder()
        .no(1)
        .blkCount(1)
        .txCount(2)
        .outSum(BigInteger.valueOf(12L))
        .maxSlot(12)
        //.rewardsDistributed(BigInteger.ONE)
        .status(EpochStatus.IN_PROGRESS)
        .build();

    Assertions.assertThrows(BusinessException.class, () -> epochService.getEpochDetail("0"));
  }

}
