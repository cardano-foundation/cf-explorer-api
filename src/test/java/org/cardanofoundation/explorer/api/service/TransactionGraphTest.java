package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.springframework.util.ObjectUtils;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.common.enumeration.TxChartRange;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.projection.TxGraphProjectionImp;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxChartRepository;
import org.cardanofoundation.explorer.api.service.impl.TxServiceImpl;

@ExtendWith(MockitoExtension.class)
class TransactionGraphTest {

  @Mock private TxChartRepository txChartRepository;

  @InjectMocks private TxServiceImpl txService;

  @Test
  void testEmptyTransactionsByRangeRedisSizeZero() {

    when(txChartRepository.getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(Collections.emptyList());

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_MONTH);
    Assertions.assertTrue(ObjectUtils.isEmpty(actual));
  }

  @Test
  void testEmptyTransactionsByRangeRedisSizeNull() {

    when(txChartRepository.getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(Collections.emptyList());

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_MONTH);

    Assertions.assertTrue(ObjectUtils.isEmpty(actual));
  }

  // transaction graph in range
  @Test
  void getTransactionChartInRangeWithoutCache() {
    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLoaLocalDateTime =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final var timeDayOne = currentLoaLocalDateTime.toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayOne =
        TxGraphProjectionImp.builder()
            .simpleTransactions(BigInteger.valueOf(200))
            .metadata(BigInteger.valueOf(200))
            .smartContract(BigInteger.valueOf(200))
            .time(BigInteger.valueOf(timeDayOne))
            .build();

    final var timeDayTwo =
        currentLoaLocalDateTime.minusDays(1L).toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayTwo =
        TxGraphProjectionImp.builder()
            .simpleTransactions(BigInteger.valueOf(100))
            .metadata(BigInteger.valueOf(100))
            .smartContract(BigInteger.valueOf(100))
            .time(BigInteger.valueOf(timeDayTwo))
            .build();

    final var timeDayThree =
        currentLoaLocalDateTime.minusDays(2L).toInstant(ZoneOffset.UTC).getEpochSecond();
    final var dayThree =
        TxGraphProjectionImp.builder()
            .simpleTransactions(BigInteger.valueOf(50))
            .metadata(BigInteger.valueOf(50))
            .smartContract(BigInteger.valueOf(50))
            .time(BigInteger.valueOf(timeDayThree))
            .build();

    when(txChartRepository.getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(List.of(dayThree, dayTwo, dayOne));

    TxGraph txGraphDayOne =
        TxGraph.builder()
            .date(
                Date.from(
                    OffsetDateTime.ofInstant(
                            Instant.ofEpochSecond(dayOne.getTime().longValue()), ZoneOffset.UTC)
                        .toInstant()))
            .simpleTransactions(dayOne.getSimpleTransactions())
            .metadata(dayOne.getMetadata())
            .smartContract(dayOne.getSmartContract())
            .build();

    TxGraph txGraphDayTwo =
        TxGraph.builder()
            .date(
                Date.from(
                    OffsetDateTime.ofInstant(
                            Instant.ofEpochSecond(dayTwo.getTime().longValue()), ZoneOffset.UTC)
                        .toInstant()))
            .simpleTransactions(dayTwo.getSimpleTransactions())
            .metadata(dayTwo.getMetadata())
            .smartContract(dayTwo.getSmartContract())
            .build();

    TxGraph txGraphDayThree =
        TxGraph.builder()
            .date(
                Date.from(
                    OffsetDateTime.ofInstant(
                            Instant.ofEpochSecond(dayThree.getTime().longValue()), ZoneOffset.UTC)
                        .toInstant()))
            .simpleTransactions(dayThree.getSimpleTransactions())
            .metadata(dayThree.getMetadata())
            .smartContract(dayThree.getSmartContract())
            .build();

    var timeOneMonthAgo =
        currentLoaLocalDateTime.minusMonths(1L).toInstant(ZoneOffset.UTC).getEpochSecond();
    TxGraph txZeroData =
        TxGraph.builder()
            .date(
                Date.from(
                    OffsetDateTime.ofInstant(Instant.ofEpochSecond(timeOneMonthAgo), ZoneOffset.UTC)
                        .toInstant()))
            .simpleTransactions(BigInteger.ZERO)
            .metadata(BigInteger.ZERO)
            .smartContract(BigInteger.ZERO)
            .build();

    List<TxGraph> expect = List.of(txZeroData, txGraphDayThree, txGraphDayTwo, txGraphDayOne);

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_MONTH);

    for (int i = 0; i < 3; i++) {
      Assertions.assertEquals(expect.get(i), actual.get(i));
    }
  }
}
