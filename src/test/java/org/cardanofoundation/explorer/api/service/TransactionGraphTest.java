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
import java.util.List;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.conversions.CardanoConverters;
import org.cardanofoundation.conversions.ClasspathConversionsFactory;
import org.cardanofoundation.conversions.domain.NetworkType;
import org.cardanofoundation.explorer.api.common.enumeration.TxChartRange;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.projection.TxGraphProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxChartRepository;
import org.cardanofoundation.explorer.api.service.impl.TxServiceImpl;

@ExtendWith(MockitoExtension.class)
class TransactionGraphTest {

  @Mock private TxChartRepository txChartRepository;
  @Mock private BlockRepository blockRepository;

  @InjectMocks private TxServiceImpl txService;

  @Test
  void getTxChartOneMonth() {
    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);
    final LocalDateTime currentLocalDate =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final LocalDateTime previousDays = currentLocalDate.minusMonths(1);
    final BigInteger previousDaysInSeconds =
        BigInteger.valueOf(previousDays.toInstant(ZoneOffset.UTC).getEpochSecond());

    TxGraphProjection txGraphProjection = Mockito.mock(TxGraphProjection.class);
    when(txGraphProjection.getTime()).thenReturn(previousDaysInSeconds);
    when(txGraphProjection.getSimpleTransactions()).thenReturn(BigInteger.valueOf(200));
    when(txGraphProjection.getMetadata()).thenReturn(BigInteger.valueOf(300));
    when(txGraphProjection.getSmartContract()).thenReturn(BigInteger.valueOf(400));
    when(txChartRepository.getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(Collections.singletonList(txGraphProjection));

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_MONTH);

    Assertions.assertEquals(31, actual.size());

    Assertions.assertEquals(200, actual.get(0).getSimpleTransactions().intValue());
    Assertions.assertEquals(300, actual.get(0).getMetadata().intValue());
    Assertions.assertEquals(400, actual.get(0).getSmartContract().intValue());

    Assertions.assertEquals(0, actual.get(1).getSimpleTransactions().intValue());
    Assertions.assertEquals(0, actual.get(1).getMetadata().intValue());
    Assertions.assertEquals(0, actual.get(1).getSmartContract().intValue());
  }

  @Test
  void getTxChartThreeMonth() {
    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);
    final LocalDateTime currentLocalDate =
        LocalDateTime.of(
            LocalDate.of(localDate.getYear(), localDate.getMonth(), localDate.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final LocalDateTime previousDays = currentLocalDate.minusMonths(3);
    final BigInteger previousDaysInSeconds =
        BigInteger.valueOf(previousDays.toInstant(ZoneOffset.UTC).getEpochSecond());

    TxGraphProjection txGraphProjection = Mockito.mock(TxGraphProjection.class);
    when(txGraphProjection.getTime()).thenReturn(previousDaysInSeconds);
    when(txGraphProjection.getSimpleTransactions()).thenReturn(BigInteger.valueOf(200));
    when(txGraphProjection.getMetadata()).thenReturn(BigInteger.valueOf(300));
    when(txGraphProjection.getSmartContract()).thenReturn(BigInteger.valueOf(400));
    when(txChartRepository.getTransactionGraphDayGreaterThan(any(BigInteger.class)))
        .thenReturn(Collections.singletonList(txGraphProjection));

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.THREE_MONTH);

    Assertions.assertEquals(92, actual.size());

    Assertions.assertEquals(200, actual.get(0).getSimpleTransactions().intValue());
    Assertions.assertEquals(300, actual.get(0).getMetadata().intValue());
    Assertions.assertEquals(400, actual.get(0).getSmartContract().intValue());

    Assertions.assertEquals(0, actual.get(1).getSimpleTransactions().intValue());
    Assertions.assertEquals(0, actual.get(1).getMetadata().intValue());
    Assertions.assertEquals(0, actual.get(1).getSmartContract().intValue());
  }

  @Test
  void getTxChartOneYear() {
    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLocalDate =
        LocalDateTime.of(
            LocalDate.of(
                localDate.getYear(), localDate.getMonth(), LocalDateTime.MIN.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final LocalDateTime previousMonths = currentLocalDate.minusMonths(12);
    final BigInteger previousMonthsInSeconds =
        BigInteger.valueOf(previousMonths.toInstant(ZoneOffset.UTC).getEpochSecond());

    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    LocalDateTime startNetworkTime = cardanoConverters.genesisConfig().getStartTime();
    when(blockRepository.getMinBlockTime())
        .thenReturn(startNetworkTime.toEpochSecond(ZoneOffset.UTC));

    TxGraphProjection txGraphProjection = Mockito.mock(TxGraphProjection.class);
    when(txGraphProjection.getTime()).thenReturn(previousMonthsInSeconds);
    when(txGraphProjection.getSimpleTransactions()).thenReturn(BigInteger.valueOf(200));
    when(txGraphProjection.getMetadata()).thenReturn(BigInteger.valueOf(300));
    when(txGraphProjection.getSmartContract()).thenReturn(BigInteger.valueOf(400));
    when(txChartRepository.getTransactionGraphMonthGreaterThan(any(BigInteger.class)))
        .thenReturn(Collections.singletonList(txGraphProjection));

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ONE_YEAR);

    Assertions.assertEquals(13, actual.size());

    Assertions.assertEquals(200, actual.get(0).getSimpleTransactions().intValue());
    Assertions.assertEquals(300, actual.get(0).getMetadata().intValue());
    Assertions.assertEquals(400, actual.get(0).getSmartContract().intValue());

    Assertions.assertEquals(0, actual.get(1).getSimpleTransactions().intValue());
    Assertions.assertEquals(0, actual.get(1).getMetadata().intValue());
    Assertions.assertEquals(0, actual.get(1).getSmartContract().intValue());
  }

  @Test
  void getTxChartThreeYear() {
    LocalDateTime localDate = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    final LocalDateTime currentLocalDate =
        LocalDateTime.of(
            LocalDate.of(
                localDate.getYear(), localDate.getMonth(), LocalDateTime.MIN.getDayOfMonth()),
            LocalTime.of(BigInteger.ZERO.intValue(), BigInteger.ZERO.intValue()));

    final LocalDateTime previousMonths = currentLocalDate.minusMonths(36);
    final BigInteger previousMonthsInSeconds =
        BigInteger.valueOf(previousMonths.toInstant(ZoneOffset.UTC).getEpochSecond());

    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    LocalDateTime startNetworkTime = cardanoConverters.genesisConfig().getStartTime();
    when(blockRepository.getMinBlockTime())
        .thenReturn(startNetworkTime.toEpochSecond(ZoneOffset.UTC));

    TxGraphProjection txGraphProjection = Mockito.mock(TxGraphProjection.class);
    when(txGraphProjection.getTime()).thenReturn(previousMonthsInSeconds);
    when(txGraphProjection.getSimpleTransactions()).thenReturn(BigInteger.valueOf(200));
    when(txGraphProjection.getMetadata()).thenReturn(BigInteger.valueOf(300));
    when(txGraphProjection.getSmartContract()).thenReturn(BigInteger.valueOf(400));
    when(txChartRepository.getTransactionGraphMonthGreaterThan(any(BigInteger.class)))
        .thenReturn(Collections.singletonList(txGraphProjection));

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.THREE_YEAR);

    Assertions.assertEquals(37, actual.size());

    Assertions.assertEquals(200, actual.get(0).getSimpleTransactions().intValue());
    Assertions.assertEquals(300, actual.get(0).getMetadata().intValue());
    Assertions.assertEquals(400, actual.get(0).getSmartContract().intValue());

    Assertions.assertEquals(0, actual.get(1).getSimpleTransactions().intValue());
    Assertions.assertEquals(0, actual.get(1).getMetadata().intValue());
    Assertions.assertEquals(0, actual.get(1).getSmartContract().intValue());
  }

  @Test
  void getTxChartAllTime() {
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    LocalDateTime startNetworkTime = cardanoConverters.genesisConfig().getStartTime();

    final long startTime =
        OffsetDateTime.of(startNetworkTime, ZoneOffset.UTC)
            .withDayOfMonth(1)
            .withHour(0)
            .withMinute(0)
            .withSecond(0)
            .toEpochSecond();

    when(blockRepository.getMinBlockTime()).thenReturn(startTime);

    TxGraphProjection txGraphProjection = Mockito.mock(TxGraphProjection.class);
    when(txGraphProjection.getTime()).thenReturn(BigInteger.valueOf(startTime));
    when(txGraphProjection.getSimpleTransactions()).thenReturn(BigInteger.valueOf(200));
    when(txGraphProjection.getMetadata()).thenReturn(BigInteger.valueOf(300));
    when(txGraphProjection.getSmartContract()).thenReturn(BigInteger.valueOf(400));
    when(txChartRepository.getTransactionGraphMonthGreaterThan(any()))
        .thenReturn(Collections.singletonList(txGraphProjection));

    List<TxGraph> actual = txService.getTransactionChartByRange(TxChartRange.ALL_TIME);

    Assertions.assertEquals(83, actual.size());

    Assertions.assertEquals(200, actual.get(0).getSimpleTransactions().intValue());
    Assertions.assertEquals(300, actual.get(0).getMetadata().intValue());
    Assertions.assertEquals(400, actual.get(0).getSmartContract().intValue());

    Assertions.assertEquals(0, actual.get(1).getSimpleTransactions().intValue());
    Assertions.assertEquals(0, actual.get(1).getMetadata().intValue());
    Assertions.assertEquals(0, actual.get(1).getSmartContract().intValue());
  }
}
