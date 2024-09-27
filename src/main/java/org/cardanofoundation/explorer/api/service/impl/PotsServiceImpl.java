package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.util.Objects;
import java.util.Optional;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.model.response.PotsOverviewResponse;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.service.PotsService;
import org.cardanofoundation.explorer.common.entity.ledgersync.AdaPots;

@Service
@RequiredArgsConstructor
@Log4j2
public class PotsServiceImpl implements PotsService {

  private final EpochRepository epochRepository;

  private final AdaPotsRepository adaPotsRepository;

  @Override
  public PotsOverviewResponse getPotsOverview() {
    Optional<Integer> currentEpoch = epochRepository.findCurrentEpochNo();

    if (currentEpoch.isEmpty()) {
      log.info("Current epoch is empty");
      return PotsOverviewResponse.builder().epoch(null).build();
    }
    AdaPots adaPots = adaPotsRepository.findByEpochNo(currentEpoch.get());
    if (Objects.isNull(adaPots)) {
      log.info("Ada pots of current epoch is null");
      return PotsOverviewResponse.builder().epoch(null).build();
    }

    BigInteger depositsAndFees =
        CommonConstant.TOTAL_ADA
            .toBigInteger()
            .subtract(
                Objects.isNull(adaPots.getTreasury()) ? BigInteger.ZERO : adaPots.getTreasury())
            .subtract(Objects.isNull(adaPots.getRewards()) ? BigInteger.ZERO : adaPots.getRewards())
            .subtract(
                Objects.isNull(adaPots.getReserves()) ? BigInteger.ZERO : adaPots.getReserves());

    return PotsOverviewResponse.builder()
        .epoch(currentEpoch.get())
        .depositsAndFees(depositsAndFees)
        .rewards(adaPots.getRewards())
        .reserves(adaPots.getReserves())
        .treasury(adaPots.getTreasury())
        .build();
  }
}
