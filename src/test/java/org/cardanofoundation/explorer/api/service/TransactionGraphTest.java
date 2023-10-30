package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.IntStream;

import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.util.ObjectUtils;

import org.cardanofoundation.explorer.api.common.enumeration.TxChartRange;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.projection.TxGraphProjection;
import org.cardanofoundation.explorer.api.projection.TxGraphProjectionImp;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxChartRepository;
import org.cardanofoundation.explorer.api.service.impl.TxServiceImpl;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
class TransactionGraphTest {

  @Mock
  private TxChartRepository txChartRepository;

  @InjectMocks
  private TxServiceImpl txService;

  @Mock
  private RedisTemplate<String, TxGraph> redisTemplate;

  @Mock
  private ListOperations listOperations;

  @Test
  void testEmptyTransactionsInOneDay() {
    when(txChartRepository.getTransactionGraphByHour(anyList()))
        .thenReturn(Collections.emptyList());

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_DAY);

    Assertions.assertEquals(Collections.emptyList(), actual);
  }

  @Test
  void testEmptyTransactionsByRangeRedisSizeZero() {

    when(redisTemplate.opsForList()).thenReturn(listOperations);
    when(listOperations.size(any(String.class)))
        .thenReturn(0L);
    when(txChartRepository
        .getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(Collections.emptyList());

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_MONTH);

    Assertions.assertTrue(ObjectUtils.isEmpty(actual));
  }

  @Test
  void testEmptyTransactionsByRangeRedisSizeNull() {

    when(redisTemplate.opsForList()).thenReturn(listOperations);
    when(listOperations.size(any(String.class)))
        .thenReturn(null);
    when(txChartRepository
        .getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(Collections.emptyList());

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_MONTH);

    Assertions.assertTrue(ObjectUtils.isEmpty(actual));
  }

  // transaction graph in day
  @Test
  void testTransactionInOneDate() {
    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(localDate.getHour(), BigInteger.ZERO.intValue()));

    final var timeDayOne = currentLoaLocalDateTime.toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayOne = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(200))
        .metadata(BigInteger.valueOf(200))
        .smartContract(BigInteger.valueOf(200))
        .time(BigInteger.valueOf(timeDayOne))
        .build();

    final var timeDayTwo = currentLoaLocalDateTime.minusHours(1L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayTwo = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(100))
        .metadata(BigInteger.valueOf(100))
        .smartContract(BigInteger.valueOf(100))
        .time(BigInteger.valueOf(timeDayTwo))
        .build();

    final var timeDayThree = currentLoaLocalDateTime.minusHours(2L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayThree = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(50))
        .metadata(BigInteger.valueOf(50))
        .smartContract(BigInteger.valueOf(50))
        .time(BigInteger.valueOf(timeDayThree))
        .build();

    List<TxGraphProjection> txGraphs =
        List.of(dayThree, dayTwo, dayOne);

    when(txChartRepository.getTransactionGraphByHour(anyList()))
        .thenReturn(txGraphs);

    TxGraph txGraphDayOne = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayOne.getSimpleTransactions())
        .metadata(dayOne.getMetadata())
        .smartContract(dayOne.getSmartContract())
        .build();

    TxGraph txGraphDayTwo = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayTwo.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayTwo.getSimpleTransactions())
        .metadata(dayTwo.getMetadata())
        .smartContract(dayTwo.getSmartContract())
        .build();

    TxGraph txGraphDayThree = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayThree.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayThree.getSimpleTransactions())
        .metadata(dayThree.getMetadata())
        .smartContract(dayThree.getSmartContract())
        .build();

    List<TxGraph> expect = List.of(txGraphDayThree, txGraphDayTwo, txGraphDayOne);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_DAY);

    for (int i = 0; i < 3; i++) {
      Assertions.assertEquals(expect.get(i), actual.get(i));
    }

  }

  // transaction graph in range
  @Test
  void getTransactionChartInRangeWithoutCache() {
    when(redisTemplate.opsForList()).thenReturn(listOperations);
    when(listOperations.size(any(String.class)))
        .thenReturn(0L);

    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final var timeDayOne = currentLoaLocalDateTime.toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayOne = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(200))
        .metadata(BigInteger.valueOf(200))
        .smartContract(BigInteger.valueOf(200))
        .time(BigInteger.valueOf(timeDayOne))
        .build();

    final var timeDayTwo = currentLoaLocalDateTime.minusDays(1L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayTwo = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(100))
        .metadata(BigInteger.valueOf(100))
        .smartContract(BigInteger.valueOf(100))
        .time(BigInteger.valueOf(timeDayTwo))
        .build();

    final var timeDayThree = currentLoaLocalDateTime.minusDays(2L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayThree = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(50))
        .metadata(BigInteger.valueOf(50))
        .smartContract(BigInteger.valueOf(50))
        .time(BigInteger.valueOf(timeDayThree))
        .build();

    when(txChartRepository
        .getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(List.of(dayThree, dayTwo, dayOne));

    TxGraph txGraphDayOne = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayOne.getSimpleTransactions())
        .metadata(dayOne.getMetadata())
        .smartContract(dayOne.getSmartContract())
        .build();

    TxGraph txGraphDayTwo = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayTwo.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayTwo.getSimpleTransactions())
        .metadata(dayTwo.getMetadata())
        .smartContract(dayTwo.getSmartContract())
        .build();

    TxGraph txGraphDayThree = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayThree.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayThree.getSimpleTransactions())
        .metadata(dayThree.getMetadata())
        .smartContract(dayThree.getSmartContract())
        .build();

    List<TxGraph> expect = List.of(txGraphDayThree, txGraphDayTwo, txGraphDayOne);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_WEEK);

    for (int i = 0; i < 3; i++) {
      Assertions.assertEquals(expect.get(i), actual.get(i));
    }

  }

  @Test
  void testTransactionChartInRangeWithCacheNotCallRepository() {
    when(redisTemplate.opsForList()).thenReturn(listOperations);
    when(listOperations.size(any(String.class)))
        .thenReturn(3L);

    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final var timeDayOne = currentLoaLocalDateTime.toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayOne = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(200))
        .metadata(BigInteger.valueOf(200))
        .smartContract(BigInteger.valueOf(200))
        .time(BigInteger.valueOf(timeDayOne))
        .build();

    final var timeDayTwo = currentLoaLocalDateTime.minusDays(1L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayTwo = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(100))
        .metadata(BigInteger.valueOf(100))
        .smartContract(BigInteger.valueOf(100))
        .time(BigInteger.valueOf(timeDayTwo))
        .build();

    final var timeDayThree = currentLoaLocalDateTime.minusDays(2L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayThree = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(50))
        .metadata(BigInteger.valueOf(50))
        .smartContract(BigInteger.valueOf(50))
        .time(BigInteger.valueOf(timeDayThree))
        .build();

    TxGraph txGraphDayOne = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayOne.getSimpleTransactions())
        .metadata(dayOne.getMetadata())
        .smartContract(dayOne.getSmartContract())
        .build();

    TxGraph txGraphDayTwo = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayTwo.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayTwo.getSimpleTransactions())
        .metadata(dayTwo.getMetadata())
        .smartContract(dayTwo.getSmartContract())
        .build();

    TxGraph txGraphDayThree = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayThree.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayThree.getSimpleTransactions())
        .metadata(dayThree.getMetadata())
        .smartContract(dayThree.getSmartContract())
        .build();

    when(listOperations.range(any(String.class), any(Long.class), any(Long.class)))
        .thenReturn(List.of(txGraphDayOne, txGraphDayTwo, txGraphDayThree));

    List<TxGraph> expect = List.of(txGraphDayThree, txGraphDayTwo, txGraphDayOne);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_WEEK);

    for (int i = 0; i < 3; i++) {
      Assertions.assertEquals(expect.get(i), actual.get(i));
    }

  }

  @Test
  void testTransactionChartInRangeWithCacheCallRepository() {
    when(redisTemplate.opsForList()).thenReturn(listOperations);
    when(listOperations.size(any(String.class)))
        .thenReturn(3L);

    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final var timeDayOne = currentLoaLocalDateTime.toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayOne = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(200))
        .metadata(BigInteger.valueOf(200))
        .smartContract(BigInteger.valueOf(200))
        .time(BigInteger.valueOf(timeDayOne))
        .build();

    final var timeDayTwo = currentLoaLocalDateTime.minusDays(1L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayTwo = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(100))
        .metadata(BigInteger.valueOf(100))
        .smartContract(BigInteger.valueOf(100))
        .time(BigInteger.valueOf(timeDayTwo))
        .build();

    final var timeDayThree = currentLoaLocalDateTime.minusDays(2L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayThree = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(50))
        .metadata(BigInteger.valueOf(50))
        .smartContract(BigInteger.valueOf(50))
        .time(BigInteger.valueOf(timeDayThree))
        .build();

    TxGraph txGraphDayOne = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayOne.getSimpleTransactions())
        .metadata(dayOne.getMetadata())
        .smartContract(dayOne.getSmartContract())
        .build();

    TxGraph txGraphDayTwo = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayTwo.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayTwo.getSimpleTransactions())
        .metadata(dayTwo.getMetadata())
        .smartContract(dayTwo.getSmartContract())
        .build();

    TxGraph txGraphDayThree = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayThree.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayThree.getSimpleTransactions())
        .metadata(dayThree.getMetadata())
        .smartContract(dayThree.getSmartContract())
        .build();

    when(listOperations.range(any(String.class), any(Long.class), any(Long.class)))
        .thenReturn(List.of(txGraphDayOne, txGraphDayTwo, txGraphDayThree));

    when(txChartRepository.getTransactionGraphByDay(anyList()))
        .thenReturn(List.of(dayOne, dayTwo));

    List<TxGraph> expect = List.of(txGraphDayThree, txGraphDayTwo, txGraphDayOne);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_WEEK);

    for (int i = 0; i < 3; i++) {
      Assertions.assertEquals(expect.get(i), actual.get(i));
    }

  }

  @Test
  void testWithCacheCallRepositoryWithOneDayDistance() {
    when(redisTemplate.opsForList()).thenReturn(listOperations);
    when(listOperations.size(any(String.class)))
        .thenReturn(3L);

    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final var timeCurrent = currentLoaLocalDateTime.toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayCurrent = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(200))
        .metadata(BigInteger.valueOf(200))
        .smartContract(BigInteger.valueOf(200))
        .time(BigInteger.valueOf(timeCurrent))
        .build();

    // data
    final LocalDateTime pastTwoDayLoaLocalDateTime =
        LocalDateTime.of(
                LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
                LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()))
            .minusDays(1L);

    final var timeDayOne = pastTwoDayLoaLocalDateTime.toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayOne = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(200))
        .metadata(BigInteger.valueOf(200))
        .smartContract(BigInteger.valueOf(200))
        .time(BigInteger.valueOf(timeDayOne))
        .build();

    final var timeDayTwo = pastTwoDayLoaLocalDateTime.minusDays(1L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayTwo = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(100))
        .metadata(BigInteger.valueOf(100))
        .smartContract(BigInteger.valueOf(100))
        .time(BigInteger.valueOf(timeDayTwo))
        .build();

    final var timeDayThree = pastTwoDayLoaLocalDateTime.minusDays(2L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayThree = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(50))
        .metadata(BigInteger.valueOf(50))
        .smartContract(BigInteger.valueOf(50))
        .time(BigInteger.valueOf(timeDayThree))
        .build();

    TxGraph txGraphDayOne = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayOne.getSimpleTransactions())
        .metadata(dayOne.getMetadata())
        .smartContract(dayOne.getSmartContract())
        .build();

    TxGraph txGraphDayTwo = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayTwo.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayTwo.getSimpleTransactions())
        .metadata(dayTwo.getMetadata())
        .smartContract(dayTwo.getSmartContract())
        .build();

    TxGraph txGraphDayThree = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayThree.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayThree.getSimpleTransactions())
        .metadata(dayThree.getMetadata())
        .smartContract(dayThree.getSmartContract())
        .build();

    TxGraph txGraphCurrent = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                    Instant.ofEpochSecond(dayCurrent.getTime().longValue()), ZoneOffset.UTC)
                .toInstant()))
        .simpleTransactions(dayCurrent.getSimpleTransactions())
        .metadata(dayCurrent.getMetadata())
        .smartContract(dayCurrent.getSmartContract())
        .build();

    when(listOperations.range(any(String.class), any(Long.class), any(Long.class)))
        .thenReturn(List.of(txGraphDayOne, txGraphDayTwo, txGraphDayThree));

    when(txChartRepository.getTransactionGraphByDay(anyList()))
        .thenReturn(List.of(dayCurrent, dayOne));

    List<TxGraph> expect = List.of(txGraphDayThree, txGraphDayTwo, txGraphDayOne, txGraphCurrent);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_WEEK);

    for (int i = 0; i < 4; i++) {
      Assertions.assertEquals(expect.get(i), actual.get(i));
    }
  }

  @Test
  void testWithCacheCallRepositoryWithOneDayDistanceInCache() {
    when(redisTemplate.opsForList()).thenReturn(listOperations);
    when(listOperations.size(any(String.class)))
        .thenReturn(3L);

    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final var timeCurrent = currentLoaLocalDateTime.toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayCurrent = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(200))
        .metadata(BigInteger.valueOf(200))
        .smartContract(BigInteger.valueOf(200))
        .time(BigInteger.valueOf(timeCurrent))
        .build();

    // data
    final LocalDateTime pastTwoDayLoaLocalDateTime =
        LocalDateTime.of(
                LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
                LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()))
            .minusDays(1L);

    final var timeDayOne = pastTwoDayLoaLocalDateTime.toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayOne = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(200))
        .metadata(BigInteger.valueOf(200))
        .smartContract(BigInteger.valueOf(200))
        .time(BigInteger.valueOf(timeDayOne))
        .build();

    final var timeDayTwo = pastTwoDayLoaLocalDateTime.minusDays(1L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayTwo = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(100))
        .metadata(BigInteger.valueOf(100))
        .smartContract(BigInteger.valueOf(100))
        .time(BigInteger.valueOf(timeDayTwo))
        .build();

    final var timeDayThree = pastTwoDayLoaLocalDateTime.minusDays(2L).toInstant(ZoneOffset.UTC)
        .getEpochSecond();
    final var dayThree = TxGraphProjectionImp.builder()
        .simpleTransactions(BigInteger.valueOf(50))
        .metadata(BigInteger.valueOf(50))
        .smartContract(BigInteger.valueOf(50))
        .time(BigInteger.valueOf(timeDayThree))
        .build();

    TxGraph txGraphDayOne = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayOne.getSimpleTransactions())
        .metadata(dayOne.getMetadata())
        .smartContract(dayOne.getSmartContract())
        .build();

    TxGraph txGraphDayTwo = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayTwo.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayTwo.getSimpleTransactions())
        .metadata(dayTwo.getMetadata())
        .smartContract(dayTwo.getSmartContract())
        .build();

    TxGraph txGraphDayThree = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                Instant.ofEpochSecond(dayThree.getTime().longValue()), ZoneOffset.UTC).toInstant()))
        .simpleTransactions(dayThree.getSimpleTransactions())
        .metadata(dayThree.getMetadata())
        .smartContract(dayThree.getSmartContract())
        .build();

    TxGraph txGraphCurrent = TxGraph.builder()
        .date(Date.from(
            OffsetDateTime.ofInstant(
                    Instant.ofEpochSecond(dayCurrent.getTime().longValue()), ZoneOffset.UTC)
                .toInstant()))
        .simpleTransactions(dayCurrent.getSimpleTransactions())
        .metadata(dayCurrent.getMetadata())
        .smartContract(dayCurrent.getSmartContract())
        .build();

    when(listOperations.range(any(String.class), any(Long.class), any(Long.class)))
        .thenReturn(List.of(txGraphDayOne, txGraphDayTwo, txGraphDayThree));

    when(txChartRepository.getTransactionGraphByDay(anyList()))
        .thenReturn(List.of(dayCurrent, dayOne));

    List<TxGraph> expect = List.of(txGraphDayThree, txGraphDayTwo, txGraphDayOne, txGraphCurrent);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_WEEK);

    for (int i = 0; i < 4; i++) {
      Assertions.assertEquals(expect.get(i), actual.get(i));
    }
  }

  @Test
  void testChartRange() {
    when(redisTemplate.opsForList()).thenReturn(listOperations);
    when(listOperations.size(any(String.class)))
        .thenReturn(7L);

    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    List<TxGraphProjection> projections = new ArrayList<>();
    List<TxGraph> expect = new ArrayList<>();

    IntStream.range(0, 7)
        .forEach(index -> {

          final var timeDayOne = currentLoaLocalDateTime.minusDays(index).toInstant(ZoneOffset.UTC)
              .getEpochSecond();
          final var dayOne = TxGraphProjectionImp.builder()
              .simpleTransactions(BigInteger.valueOf(index))
              .metadata(BigInteger.valueOf(index))
              .smartContract(BigInteger.valueOf(index))
              .time(BigInteger.valueOf(timeDayOne))
              .build();

          projections.add(dayOne);

          TxGraph txGraphDayOne = TxGraph.builder()
              .date(Date.from(
                  OffsetDateTime.ofInstant(
                          Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC)
                      .toInstant()))
              .simpleTransactions(dayOne.getSimpleTransactions())
              .metadata(dayOne.getMetadata())
              .smartContract(dayOne.getSmartContract())
              .build();

          expect.add(txGraphDayOne);
        });

    when(listOperations.range(any(String.class), any(Long.class), any(Long.class)))
        .thenReturn(List.of(expect.get(6)));

    when(txChartRepository
        .getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(projections);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_WEEK);

    Assertions.assertEquals(List.of(expect.get(0)), actual);
  }

  @Test
  void testChartOutOfRange() {
    when(redisTemplate.opsForList()).thenReturn(listOperations);
    final long range = 34L;
    when(listOperations.size(any(String.class)))
        .thenReturn(range);

    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    List<TxGraphProjection> projections = new ArrayList<>();
    List<TxGraph> expectPrepare = new ArrayList<>();

    IntStream.range(0, (int) range)
        .forEach(index -> {

          final var timeDayOne = currentLoaLocalDateTime.minusDays(index).toInstant(ZoneOffset.UTC)
              .getEpochSecond();
          final var dayOne = TxGraphProjectionImp.builder()
              .simpleTransactions(BigInteger.valueOf(index))
              .metadata(BigInteger.valueOf(index))
              .smartContract(BigInteger.valueOf(index))
              .time(BigInteger.valueOf(timeDayOne))
              .build();

          projections.add(dayOne);

          TxGraph txGraphDayOne = TxGraph.builder()
              .date(Date.from(
                  OffsetDateTime.ofInstant(
                          Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC)
                      .toInstant()))
              .simpleTransactions(dayOne.getSimpleTransactions())
              .metadata(dayOne.getMetadata())
              .smartContract(dayOne.getSmartContract())
              .build();

          expectPrepare.add(txGraphDayOne);
        });

    when(listOperations.range(any(String.class), any(Long.class), any(Long.class)))
        .thenReturn(expectPrepare);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_WEEK);

    final var expect = expectPrepare
        .subList(0, 7)
        .stream()
        .sorted(Comparator.comparing(TxGraph::getDate))
        .toList();

    Assertions.assertEquals(expect, actual);
  }

  @Test
  void testMissingDataFewDay() {

    final long range = 11L;
    when(redisTemplate.opsForList()).thenReturn(listOperations);
    when(listOperations.size(any(String.class)))
        .thenReturn(range);

    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    List<TxGraphProjection> projections = new ArrayList<>();
    List<TxGraph> expectPrepare = new ArrayList<>();

    IntStream.range(3, (int) range)
        .forEach(index -> {

          final var timeDayOne = currentLoaLocalDateTime.minusDays(index).toInstant(ZoneOffset.UTC)
              .getEpochSecond();
          final var dayOne = TxGraphProjectionImp.builder()
              .simpleTransactions(BigInteger.valueOf(index))
              .metadata(BigInteger.valueOf(index))
              .smartContract(BigInteger.valueOf(index))
              .time(BigInteger.valueOf(timeDayOne))
              .build();

          projections.add(dayOne);

          TxGraph txGraphDayOne = TxGraph.builder()
              .date(Date.from(
                  OffsetDateTime.ofInstant(
                          Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC)
                      .toInstant()))
              .simpleTransactions(dayOne.getSimpleTransactions())
              .metadata(dayOne.getMetadata())
              .smartContract(dayOne.getSmartContract())
              .build();

          expectPrepare.add(txGraphDayOne);
        });

    when(listOperations.range(any(String.class), any(Long.class), any(Long.class)))
        .thenReturn(expectPrepare);

    List<TxGraphProjection> dataMissingProjections = new ArrayList<>();
    List<TxGraph> dataMissingTxGraph = new ArrayList<>();

    IntStream.range(3, 34)
        .forEach(index -> {

          final var timeDayOne = currentLoaLocalDateTime.minusDays(index).toInstant(ZoneOffset.UTC)
              .getEpochSecond();
          final var dayOne = TxGraphProjectionImp.builder()
              .simpleTransactions(BigInteger.valueOf(index))
              .metadata(BigInteger.valueOf(index))
              .smartContract(BigInteger.valueOf(index))
              .time(BigInteger.valueOf(timeDayOne))
              .build();

          dataMissingProjections.add(dayOne);

          TxGraph txGraphDayOne = TxGraph.builder()
              .date(Date.from(
                  OffsetDateTime.ofInstant(
                          Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC)
                      .toInstant()))
              .simpleTransactions(dayOne.getSimpleTransactions())
              .metadata(dayOne.getMetadata())
              .smartContract(dayOne.getSmartContract())
              .build();

          dataMissingTxGraph.add(txGraphDayOne);
        });

    when(txChartRepository
        .getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(dataMissingProjections);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_WEEK);

    List<TxGraph> expected = List.of(
        dataMissingTxGraph.get(3),
        dataMissingTxGraph.get(2),
        dataMissingTxGraph.get(1),
        dataMissingTxGraph.get(0));

    Assertions.assertEquals(expected, actual);
  }

}
