package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.InstantaneousRewardsResponse;
import org.cardanofoundation.explorer.api.projection.InstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.ReserveRepository;
import org.cardanofoundation.explorer.api.repository.TreasuryRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.InstantaneousRewardsService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class InstantaneousRewardsServiceImpl implements InstantaneousRewardsService {

  private final ReserveRepository reserveRepository;
  private final TreasuryRepository treasuryRepository;
  private final TxRepository txRepository;
  private static final String TX_ID = "txId";
  private static final String NUMBER_OF_STAKES = "numberOfStakes";
  private static final String REWARDS = "rewards";

  @Override
  public BaseFilterResponse<InstantaneousRewardsResponse> getAll(Pageable pageable) {
    var instantaneousRewards = reserveRepository.findAllTx();
    instantaneousRewards.addAll(treasuryRepository.findAllTx());
    sortInstantaneousRewards(pageable.getSort(), instantaneousRewards);
    int totalElements = instantaneousRewards.size();
    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), totalElements);
    instantaneousRewards = instantaneousRewards.subList(start, end);
    Set<Long> txIds = instantaneousRewards.stream().map(InstantaneousRewardsProjection::getTxId).collect(
        Collectors.toSet());
    List<TxIOProjection> txs = txRepository.findTxIn(txIds);
    Map<Long, TxIOProjection> txMap
        = txs.stream().collect(Collectors.toMap(TxIOProjection::getId, Function.identity()));
    List<InstantaneousRewardsResponse> response = instantaneousRewards.stream().map(
        item -> InstantaneousRewardsResponse.builder()
            .txHash(txMap.get(item.getTxId()).getHash())
            .blockNo(txMap.get(item.getTxId()).getBlockNo())
            .epochNo(txMap.get(item.getTxId()).getEpochNo())
            .epochSlotNo(txMap.get(item.getTxId()).getEpochSlotNo())
            .time(txMap.get(item.getTxId()).getTime())
            .epochSlotNo(txMap.get(item.getTxId()).getEpochSlotNo())
            .slotNo(txMap.get(item.getTxId()).getSlot())
            .numberOfStakes(item.getNumberOfStakes())
            .rewards(item.getRewards())
            .build()).collect(Collectors.toList());
    Page<InstantaneousRewardsResponse> page = new PageImpl<>(response,
        pageable, totalElements);
    return new BaseFilterResponse<>(page);
  }

  /**
   * Sorts the list of instantaneous rewards based on the sortable parameter
   * @param sortable the sortable parameter
   * @param instantaneousRewards the list of instantaneous rewards
   */
  private void sortInstantaneousRewards(Sort sortable, List<InstantaneousRewardsProjection> instantaneousRewards) {
    String sortField = sortable.stream().findFirst().map(Sort.Order::getProperty).orElse(TX_ID);
    String sortOrder = sortable.stream().findFirst().map(Sort.Order::getDirection).map(Enum::name).orElse(Sort.Direction.DESC.name());
    switch (sortField) {
      case NUMBER_OF_STAKES -> {
        if (sortOrder.equals(Sort.Direction.ASC.name()))
          instantaneousRewards.sort(Comparator.comparing(InstantaneousRewardsProjection::getNumberOfStakes));
        else
          instantaneousRewards.sort(Comparator.comparing(InstantaneousRewardsProjection::getNumberOfStakes).reversed());
      }
      case REWARDS -> {
        if (sortOrder.equals(Sort.Direction.ASC.name()))
          instantaneousRewards.sort(Comparator.comparing(InstantaneousRewardsProjection::getRewards));
        else
          instantaneousRewards.sort(Comparator.comparing(InstantaneousRewardsProjection::getRewards).reversed());
      }
      default -> {
        if (sortOrder.equals(Sort.Direction.ASC.name()))
          instantaneousRewards.sort(Comparator.comparing(InstantaneousRewardsProjection::getTxId));
        else
          instantaneousRewards.sort(Comparator.comparing(InstantaneousRewardsProjection::getTxId).reversed());
      }
    }
  }
}
