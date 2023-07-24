package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.projection.EpochSummaryProjection;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
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

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.util.ReflectionTestUtils;

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

  @Mock
  FetchRewardDataService fetchRewardDataService;

  @Test
  void testCurrentEpochSummary() {

    var localDate = LocalDateTime.of(LocalDate.now(), LocalTime.of(0, 0));
    ReflectionTestUtils.setField(epochService, "network", "mainnet");
    ReflectionTestUtils.setField(epochService, "EPOCH_DAYS", 5);
    when(epochRepository.findCurrentEpochSummary())
            .thenReturn(Optional.of(EpochSummaryProjectionImpl.builder()
                    .no(30)
                    .maxSlot(432000)
                    .statTime(Timestamp.valueOf(localDate))
                    .endTime(Timestamp.valueOf(localDate.plusDays(5)))
                    .build()));

    when(redisTemplate.opsForHash())
            .thenReturn(hashOperations);

    when(hashOperations.size(any()))
            .thenReturn(0l);

    when(hashOperations.size(any()))
            .thenReturn(1L);

    when(epochRepository.findFirstByNo(BigInteger.ZERO.intValue()))
            .thenReturn(Optional.of(
                    Epoch.builder()
                            .no(0)
                            .startTime(Timestamp.valueOf(localDate))
                            .endTime(Timestamp.valueOf(localDate.plusDays(5)))
                            .build()));

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

    ReflectionTestUtils.setField(epochService, "network", "mainnet");
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
    when(redisTemplate.opsForHash())
            .thenReturn(hashOperations);

    when(hashOperations.size(any()))
            .thenReturn(0l);

    when(hashOperations.size(any()))
            .thenReturn(1L);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.ofNullable(epoch.getNo()));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(expect);
    when(fetchRewardDataService.checkEpochRewardDistributed(any())).thenReturn(true);
    EpochResponse actual = epochService.getEpochDetail("1");
    expect.setStatus(EpochStatus.IN_PROGRESS);
    Assertions.assertEquals(expect, actual);
  }

  @Test
  void testGetEpochDetailSync() {

    ReflectionTestUtils.setField(epochService, "network", "mainnet");
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
    when(fetchRewardDataService.checkEpochRewardDistributed(any())).thenReturn(true);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.ofNullable(epoch.getNo()));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(expect);
    when(redisTemplate.opsForHash())
            .thenReturn(hashOperations);

    when(hashOperations.size(any()))
            .thenReturn(0l);

    when(hashOperations.size(any()))
            .thenReturn(1L);

    EpochResponse actual = epochService.getEpochDetail("1");
    expect.setStatus(EpochStatus.SYNCING);
    Assertions.assertEquals(expect, actual);
  }

  @Test
  void testGetEpochDetailDone() {
    ReflectionTestUtils.setField(epochService, "network", "mainnet");
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
    when(fetchRewardDataService.checkEpochRewardDistributed(any())).thenReturn(true);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(epoch.getNo() + 1));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(expect);
    when(redisTemplate.opsForHash())
            .thenReturn(hashOperations);

    when(hashOperations.size(any()))
            .thenReturn(0l);

    when(hashOperations.size(any()))
            .thenReturn(1L);

    EpochResponse actual = epochService.getEpochDetail("1");
    expect.setStatus(EpochStatus.FINISHED);
    Assertions.assertEquals(expect, actual);
  }

  @Test
  void testGetEpochDetailReward() {
    ReflectionTestUtils.setField(epochService, "network", "mainnet");
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
    when(fetchRewardDataService.checkEpochRewardDistributed(any())).thenReturn(true);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(epoch.getNo() + 1));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(expect);
    when(redisTemplate.opsForHash())
            .thenReturn(hashOperations);

    when(hashOperations.size(any()))
            .thenReturn(0l);

    when(hashOperations.size(any()))
            .thenReturn(1L);

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

  @Test
  void testGetEpochDetail_throwCurrentEpoch() {
    Integer no = 0;

    when(epochRepository.findFirstByNo(no)).thenReturn(Optional.of(new Epoch()));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.empty());

    Assertions.assertThrows(BusinessException.class, () -> epochService.getEpochDetail(no.toString()));
  }

  @Test
  void testGetEpochDetail_thenReturnNotRewardDistributed() {
    Integer no = 1;
    Epoch epoch = Epoch.builder().no(no).build();
    LocalDateTime dateTime = LocalDateTime.now().minusDays(1);
    DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd hh:mm:ss");

    when(epochRepository.findFirstByNo(no)).thenReturn(Optional.of(epoch));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(3));
    when(fetchRewardDataService.checkEpochRewardDistributed(epoch)).thenReturn(false);
    when(fetchRewardDataService.fetchEpochRewardDistributed(List.of(no))).thenReturn(List.of(Epoch.builder().rewardsDistributed(BigInteger.ONE).build()));
    when(epochRepository.findFirstByNo(BigInteger.ZERO.intValue())).thenReturn(Optional.of(Epoch.builder().startTime(Timestamp.valueOf(dateTime)).build()));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(EpochResponse.builder().startTime(dateTime).endTime(dateTime).build());
    when(redisTemplate.opsForHash()).thenReturn(hashOperations);
    when(hashOperations.size(anyString())).thenReturn(1L);
    ReflectionTestUtils.setField(epochService, "network", "mainnet");
    ReflectionTestUtils.setField(epochService, "EPOCH_DAYS", 2);

    var response = epochService.getEpochDetail(no.toString());
    Assertions.assertEquals(response.getStatus() , EpochStatus.IN_PROGRESS);
    Assertions.assertEquals(response.getStartTime().format(dtf), dateTime.format(dtf));
    Assertions.assertEquals(response.getEndTime().format(dtf), dateTime.plusDays(2).format(dtf));
    Assertions.assertEquals(response.getAccount(), 1);
  }

  @Test
  void testGetEpochDetail_throwNotRewardDistributed() {
    Integer no = 1;
    Epoch epoch = Epoch.builder().no(no).build();
    LocalDateTime now = LocalDateTime.now();
    String pattern = "yyyy/MM/dd hh:mm:ss";
    DateTimeFormatter dtf = DateTimeFormatter.ofPattern(pattern);

    when(epochRepository.findFirstByNo(no)).thenReturn(Optional.of(epoch));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(3));
    when(fetchRewardDataService.checkEpochRewardDistributed(epoch)).thenReturn(false);
    when(fetchRewardDataService.fetchEpochRewardDistributed(List.of(no))).thenReturn(null);

    Assertions.assertThrows(FetchRewardException.class, () -> epochService.getEpochDetail(no.toString()));
  }

  @Test
  void testGetCurrentEpochSummary_thenReturn() {
    LocalDateTime now = LocalDateTime.now();
    DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd hh:mm:ss");

    EpochSummaryProjection esp = Mockito.mock(EpochSummaryProjection.class);
    when(esp.getStartTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(esp.getNo()).thenReturn(1);
    when(esp.getMaxSlot()).thenReturn(26000);

    when(epochRepository.findCurrentEpochSummary()).thenReturn(Optional.of(esp));
    when(epochRepository.findFirstByNo(BigInteger.ZERO.intValue())).thenReturn(Optional.of(Epoch.builder().startTime(Timestamp.valueOf(LocalDateTime.now())).build()));
    when(redisTemplate.opsForHash()).thenReturn(hashOperations);
    when(hashOperations.size(anyString())).thenReturn(1L);
    ReflectionTestUtils.setField(epochService, "network", "mainnet");

    var response = epochService.getCurrentEpochSummary();
    Assertions.assertEquals(response.getNo() , 1);
    Assertions.assertEquals(response.getTotalSlot(), 26000);
    Assertions.assertEquals(response.getAccount(), 1);
    Assertions.assertEquals(response.getStartTime().format(dtf), now.format(dtf));
    Assertions.assertEquals(response.getEndTime().format(dtf), now.format(dtf));
  }

  @Test
  void testGetAllEpoch_thenReturn() {
    Pageable pageable = PageRequest.of(0, 10);
    LocalDateTime now = LocalDateTime.now();
    Epoch epoch = Epoch.builder().no(1).build();
    DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd hh:mm:ss");

    when(epochRepository.findFirstByNo(BigInteger.ZERO.intValue())).thenReturn(Optional.of(Epoch.builder().startTime(Timestamp.valueOf(now)).build()));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(3));
    when(epochRepository.findAll(pageable)).thenReturn(new PageImpl<>(List.of(epoch)));
    when(fetchRewardDataService.checkEpochRewardDistributed(any())).thenReturn(false);
    when(fetchRewardDataService.fetchEpochRewardDistributed(List.of(1))).thenReturn(List.of(Epoch.builder().no(1).rewardsDistributed(BigInteger.ONE).build()));
    when(epochMapper.epochToEpochResponse(epoch)).thenReturn(EpochResponse.builder().no(1).startTime(now).endTime(now).build());
    when(redisTemplate.opsForHash()).thenReturn(hashOperations);
    when(hashOperations.size(anyString())).thenReturn(1L);
    ReflectionTestUtils.setField(epochService, "network", "mainnet");

    var response = epochService.getAllEpoch(pageable);
    Assertions.assertEquals(response.getTotalItems() , 1);
    Assertions.assertEquals(response.getTotalPages() , 1);
    Assertions.assertEquals(response.getCurrentPage() , 0);
    Assertions.assertEquals(response.getData().get(0).getNo() , 1);
    Assertions.assertEquals(response.getData().get(0).getStatus() , EpochStatus.FINISHED);
    Assertions.assertEquals(response.getData().get(0).getStartTime().format(dtf) , now.format(dtf));
    Assertions.assertEquals(response.getData().get(0).getEndTime().format(dtf) , now.format(dtf));
  }

  @Test
  void testGetAllEpoch_throwCurrentEpochNo() {
    Pageable pageable = PageRequest.of(0, 10);
    LocalDateTime now = LocalDateTime.now();

    when(epochRepository.findFirstByNo(BigInteger.ZERO.intValue())).thenReturn(Optional.of(Epoch.builder().startTime(Timestamp.valueOf(now)).build()));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.empty());

    Assertions.assertThrows(NoContentException.class, () -> epochService.getAllEpoch(pageable));
  }

  @Test
  void testGetAllEpoch_throwFetchEpochList() {
    Pageable pageable = PageRequest.of(0, 10);
    LocalDateTime now = LocalDateTime.now();
    Epoch epoch = Epoch.builder().no(1).build();

    when(epochRepository.findFirstByNo(BigInteger.ZERO.intValue())).thenReturn(Optional.of(Epoch.builder().startTime(Timestamp.valueOf(now)).build()));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(3));
    when(epochRepository.findAll(pageable)).thenReturn(new PageImpl<>(List.of(epoch)));
    when(fetchRewardDataService.checkEpochRewardDistributed(any())).thenReturn(false);
    when(fetchRewardDataService.fetchEpochRewardDistributed(List.of(1))).thenReturn(null);

    Assertions.assertThrows(FetchRewardException.class, () -> epochService.getAllEpoch(pageable));
  }

}