package com.cardano.explorer.service;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

import com.cardano.explorer.common.enumeration.EpochStatus;
import com.cardano.explorer.mapper.EpochMapper;
import com.cardano.explorer.model.response.EpochResponse;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.service.impl.EpochServiceImpl;
import com.sotatek.cardano.common.entity.Epoch;
import com.sotatek.cardano.common.enumeration.EraType;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
 class EpochServiceTest {

  @Mock
  EpochRepository epochRepository;
  @Mock
  EpochMapper epochMapper;
  @InjectMocks
  EpochServiceImpl epochService;

  //TODO getAllEpoch

  //TODO getCurrentEpochSummary

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
        .rewardsDistributed(BigInteger.ONE)
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
        .rewardsDistributed(BigInteger.ONE)
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
        .rewardsDistributed(BigInteger.ONE)
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
        .rewardsDistributed(BigInteger.ONE)
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
        .rewardsDistributed(BigInteger.ONE)
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
        .rewardsDistributed(BigInteger.ONE)
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
        .rewardsDistributed(BigInteger.ONE)
        .status(EpochStatus.IN_PROGRESS)
        .build();

    Assertions.assertThrows(BusinessException.class, () ->epochService.getEpochDetail("0"));
  }

}
