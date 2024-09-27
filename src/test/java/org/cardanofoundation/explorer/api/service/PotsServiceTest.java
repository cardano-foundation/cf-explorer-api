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
import org.cardanofoundation.explorer.api.model.response.PotsOverviewResponse;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.service.impl.PotsServiceImpl;
import org.cardanofoundation.explorer.common.entity.ledgersync.AdaPots;

@ExtendWith(MockitoExtension.class)
public class PotsServiceTest {
  @Mock private EpochRepository epochRepository;

  @Mock private AdaPotsRepository adaPotsRepository;

  @InjectMocks private PotsServiceImpl potsService;

  @Test
  void testGetPotsOverview_CurrentEpochIsEmpty() {
    // Arrange
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.empty());

    // Act
    PotsOverviewResponse response = potsService.getPotsOverview();

    // Assert
    assertThat(response.getEpoch()).isNull();
    verify(epochRepository).findCurrentEpochNo();
    verifyNoInteractions(adaPotsRepository);
  }

  @Test
  void testGetPotsOverview_AdaPotsIsNull() {
    // Arrange
    int currentEpoch = 200;
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
    when(adaPotsRepository.findByEpochNo(currentEpoch)).thenReturn(null);

    // Act
    PotsOverviewResponse response = potsService.getPotsOverview();

    // Assert
    assertThat(response.getEpoch()).isNull();
    verify(epochRepository).findCurrentEpochNo();
    verify(adaPotsRepository).findByEpochNo(currentEpoch);
  }

  @Test
  void testGetPotsOverview_SuccessfulResponse() {
    // Arrange
    int currentEpoch = 200;
    AdaPots adaPots =
        AdaPots.builder()
            .deposits(BigInteger.ONE)
            .rewards(BigInteger.TEN)
            .treasury(BigInteger.ZERO)
            .reserves(BigInteger.ZERO)
            .build();

    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
    when(adaPotsRepository.findByEpochNo(currentEpoch)).thenReturn(adaPots);

    // Act
    PotsOverviewResponse response = potsService.getPotsOverview();

    // Assert
    assertThat(response.getEpoch()).isEqualTo(currentEpoch);
    assertThat(response.getDepositsAndFees())
        .isEqualTo(CommonConstant.TOTAL_ADA.toBigInteger().subtract(BigInteger.TEN));
    assertThat(response.getRewards()).isEqualTo(BigInteger.TEN);
    assertThat(response.getTreasury()).isEqualTo(BigInteger.ZERO);
    assertThat(response.getReserves()).isEqualTo(BigInteger.ZERO);
    verify(epochRepository).findCurrentEpochNo();
    verify(adaPotsRepository).findByEpochNo(currentEpoch);
  }
}
