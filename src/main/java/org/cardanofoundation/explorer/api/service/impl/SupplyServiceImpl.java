package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.util.Objects;
import java.util.Optional;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.service.SupplyService;
import org.cardanofoundation.explorer.common.entity.ledgersync.AdaPots;

@Service
@RequiredArgsConstructor
@Log4j2
public class SupplyServiceImpl implements SupplyService {

  private final AdaPotsRepository adaPotsRepository;

  private final EpochRepository epochRepository;

  public Long getSupplyCirculating() {

    AdaPots latestAdaPots = getLatestAdaPots();

    if (Objects.isNull(latestAdaPots.getReserves())
        || Objects.isNull(latestAdaPots.getTreasury())) {
      log.info("The reserves or treasury of the epoch {} is null", latestAdaPots.getEpochNo());
      return null;
    }
    BigInteger circulatingSupply =
        CommonConstant.TOTAL_ADA
            .toBigInteger()
            .subtract(latestAdaPots.getReserves())
            .subtract(latestAdaPots.getTreasury());
    return circulatingSupply.divide(CommonConstant.ONE_ADA_IN_LOVELACES).longValue();
  }

  public Long getSupplyTotal() {
    AdaPots latestAdaPots = getLatestAdaPots();
    if (Objects.isNull(latestAdaPots.getReserves())) {
      log.info("The reserves of the epoch {} is null", latestAdaPots.getEpochNo());
      return null;
    }
    BigInteger supplyTotal =
        CommonConstant.TOTAL_ADA.toBigInteger().subtract(latestAdaPots.getReserves());
    return supplyTotal.divide(CommonConstant.ONE_ADA_IN_LOVELACES).longValue();
  }

  private AdaPots getLatestAdaPots() {
    Optional<Integer> currentEpoch = epochRepository.findCurrentEpochNo();

    if (currentEpoch.isEmpty()) {
      log.info("Current epoch is empty");
      return AdaPots.builder().build();
    }
    return adaPotsRepository.findByEpochNo(currentEpoch.get());
  }
}
