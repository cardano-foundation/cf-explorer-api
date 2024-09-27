package org.cardanofoundation.explorer.api.service;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.Mockito.*;

import java.math.BigInteger;
import java.util.Optional;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.service.impl.SupplyServiceImpl;
import org.cardanofoundation.explorer.common.entity.ledgersync.AdaPots;

@ExtendWith(MockitoExtension.class)
public class SupplyServiceTest {
  @Mock private AdaPotsRepository adaPotsRepository;

  @Mock private EpochRepository epochRepository;

  @InjectMocks private SupplyServiceImpl supplyService;

  @Test
  void testGetSupplyCirculating_CurrentEpochIsEmpty() {
    // Arrange
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.empty());

    // Act
    Long circulatingSupply = supplyService.getSupplyCirculating();

    // Assert
    assertThat(circulatingSupply).isNull();
    verify(epochRepository).findCurrentEpochNo();
    verifyNoInteractions(adaPotsRepository);
  }

  @Test
  void testGetSupplyCirculating_ReservesOrTreasuryIsNull() {
    // Arrange
    int currentEpoch = 200;
    AdaPots adaPots = AdaPots.builder().epochNo(currentEpoch).reserves(null).treasury(null).build();
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
    when(adaPotsRepository.findByEpochNo(currentEpoch)).thenReturn(adaPots);

    // Act
    Long circulatingSupply = supplyService.getSupplyCirculating();

    // Assert
    assertThat(circulatingSupply).isNull();
    verify(epochRepository).findCurrentEpochNo();
    verify(adaPotsRepository).findByEpochNo(currentEpoch);
  }

  @Test
  void testGetSupplyCirculating_SuccessfulResponse() {
    // Arrange
    int currentEpoch = 200;
    BigInteger reserves = BigInteger.valueOf(2000000000L);
    BigInteger treasury = BigInteger.valueOf(1000000000L);
    AdaPots adaPots =
        AdaPots.builder().epochNo(currentEpoch).reserves(reserves).treasury(treasury).build();

    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
    when(adaPotsRepository.findByEpochNo(currentEpoch)).thenReturn(adaPots);

    BigInteger expectedCirculatingSupply =
        CommonConstant.TOTAL_ADA
            .toBigInteger()
            .subtract(reserves)
            .subtract(treasury)
            .divide(CommonConstant.ONE_ADA_IN_LOVELACES);

    // Act
    Long circulatingSupply = supplyService.getSupplyCirculating();

    // Assert
    assertThat(circulatingSupply).isEqualTo(expectedCirculatingSupply.longValue());
    verify(epochRepository).findCurrentEpochNo();
    verify(adaPotsRepository).findByEpochNo(currentEpoch);
  }

  @Test
  void testGetSupplyTotal_CurrentEpochIsEmpty() {
    // Arrange
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.empty());

    // Act
    Long supplyTotal = supplyService.getSupplyTotal();

    // Assert
    assertThat(supplyTotal).isNull();
    verify(epochRepository).findCurrentEpochNo();
  }

  @Test
  void testGetSupplyTotal_ReservesIsNull() {
    // Arrange
    int currentEpoch = 200;
    AdaPots adaPots = AdaPots.builder().epochNo(currentEpoch).reserves(null).build();
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
    when(adaPotsRepository.findByEpochNo(currentEpoch)).thenReturn(adaPots);

    // Act
    Long supplyTotal = supplyService.getSupplyTotal();

    // Assert
    assertThat(supplyTotal).isNull();
    verify(epochRepository).findCurrentEpochNo();
    verify(adaPotsRepository).findByEpochNo(currentEpoch);
  }

  @Test
  void testGetSupplyTotal_SuccessfulResponse() {
    // Arrange
    int currentEpoch = 200;
    BigInteger reserves = BigInteger.valueOf(2000000000L);
    AdaPots adaPots = AdaPots.builder().epochNo(currentEpoch).reserves(reserves).build();

    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
    when(adaPotsRepository.findByEpochNo(currentEpoch)).thenReturn(adaPots);

    BigInteger expectedSupplyTotal =
        CommonConstant.TOTAL_ADA
            .toBigInteger()
            .subtract(reserves)
            .divide(CommonConstant.ONE_ADA_IN_LOVELACES);

    // Act
    Long supplyTotal = supplyService.getSupplyTotal();

    // Assert
    assertThat(supplyTotal).isEqualTo(expectedSupplyTotal.longValue());
    verify(epochRepository).findCurrentEpochNo();
    verify(adaPotsRepository).findByEpochNo(currentEpoch);
  }
}
