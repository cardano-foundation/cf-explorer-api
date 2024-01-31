package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.InstantaneousRewardsResponse;
import org.cardanofoundation.explorer.api.projection.InstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.ReserveRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TreasuryRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.service.impl.InstantaneousRewardsServiceImpl;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
public class InstantaneousRewardsServiceTest {
  @InjectMocks
  private InstantaneousRewardsServiceImpl instantaneousRewardsService;
  @Mock
  private TreasuryRepository treasuryRepository;
  @Mock
  private ReserveRepository reserveRepository;
  @Mock
  private TxRepository txRepository;

//  @Test
  void testGetAll_thenReturn(){
    Pageable pageable = PageRequest.of(0,1);
    InstantaneousRewardsProjection projection = Mockito.mock(InstantaneousRewardsProjection.class);
    when(projection.getRewards()).thenReturn(BigInteger.ONE);
    when(projection.getTxId()).thenReturn(1L);

    InstantaneousRewardsProjection projection1 = Mockito.mock(InstantaneousRewardsProjection.class);
    when(projection.getRewards()).thenReturn(BigInteger.TWO);
    when(projection.getTxId()).thenReturn(1L);

    List<InstantaneousRewardsProjection> instantaneousRewards = List.of(projection);
    List<InstantaneousRewardsProjection> instantaneousRewards1 = List.of(projection1);


    when(reserveRepository.findAllTx()).thenReturn(instantaneousRewards);
    when(treasuryRepository.findAllTx()).thenReturn(instantaneousRewards1);

    List<InstantaneousRewardsProjection> list = List.of(projection,projection1);

    Set<Long> txIds = Set.of(1L);

    TxIOProjection txIOProjection = Mockito.mock(TxIOProjection.class);
    when(txIOProjection.getId()).thenReturn(1L);
    when(txIOProjection.getHash()).thenReturn("hash");
    when(txRepository.findTxIn(txIds)).thenReturn(List.of(txIOProjection));

    InstantaneousRewardsResponse response = InstantaneousRewardsResponse.builder()
        .txHash("hash")
        .build();
    Page<InstantaneousRewardsResponse> page =  new PageImpl<>(List.of(response),pageable,pageable.getPageSize());

    BaseFilterResponse<InstantaneousRewardsResponse> expect = new BaseFilterResponse<>(page);
   when(instantaneousRewardsService.getAll(pageable)).thenReturn(expect);

    BaseFilterResponse<InstantaneousRewardsResponse> actual = instantaneousRewardsService.getAll(pageable);
  }
}
