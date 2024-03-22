package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.service.impl.SearchServiceImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;
import org.cardanofoundation.explorer.common.entity.ledgersync.Epoch;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
import org.cardanofoundation.explorer.common.entity.ledgersync.Script;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;
import org.cardanofoundation.explorer.common.entity.ledgersync.Tx;

@ExtendWith(MockitoExtension.class)
public class SearchServiceTest {
  @InjectMocks private SearchServiceImpl searchService;
  @Mock private EpochRepository epochRepository;
  @Mock private BlockRepository blockRepository;
  @Mock private ScriptRepository scriptRepository;
  @Mock private TxRepository txRepository;
  @Mock private MultiAssetRepository multiAssetRepository;
  @Mock private PoolHashRepository poolHashRepository;
  @Mock private StakeAddressRepository stakeAddressRepository;

  @BeforeEach
  void setUp() {
    ReflectionTestUtils.setField(searchService, "network", "mainnet");
  }

  @Test
  void testSearch_thenReturnMatchingEpoch() {
    String query = "1";
    Integer epochNo = 1;
    Epoch epoch = Epoch.builder().id(1L).no(epochNo).build();

    when(epochRepository.findFirstByNo(epochNo)).thenReturn(Optional.of(epoch));
    var actual = searchService.search(query);

    Assertions.assertEquals(actual.getEpoch(), epochNo);
  }

  @Test
  void testSearch_thenReturnMatchBlock() {
    String query = "1";
    Long blockNo = 1L;
    Block block = Block.builder().blockNo(blockNo).build();

    when(blockRepository.findFirstByBlockNo(blockNo)).thenReturn(Optional.of(block));
    var actual = searchService.search(query);

    Assertions.assertEquals(Integer.parseInt(actual.getBlock()), blockNo);
  }

  @Test
  void testSearch_thenReturnMatchTx9() {
    String query = "hash";
    Tx tx = Tx.builder().id(1L).hash(query).build();

    when(txRepository.findByHash(query)).thenReturn(Optional.of(tx));
    var actual = searchService.search(query);

    Assertions.assertEquals(actual.getTx(), query);
  }

  @Test
  void testSearch_thenReturnMatchMultiAssets_MatchToken() {
    String query = "token";
    MultiAsset multiAsset = MultiAsset.builder().fingerprint(query).build();

    when(multiAssetRepository.findByFingerprint(query)).thenReturn(Optional.of(multiAsset));
    var actual = searchService.search(query);

    Assertions.assertEquals(actual.getToken().getFingerprint(), query);
  }

  @Test
  void testSearch_thenReturnMatchMultiAssets_NoMatchToken() {
    String query = "token";
    MultiAsset multiAsset = MultiAsset.builder().nameView(query).fingerprint("fingerprint").build();
    List<MultiAsset> list = List.of(multiAsset);

    when(multiAssetRepository.findByNameViewLike(query, PageRequest.of(0, 2))).thenReturn(list);
    var actual = searchService.search(query);

    Assertions.assertEquals(actual.isValidTokenName(), true);
    Assertions.assertEquals(actual.getToken().getFingerprint(), "fingerprint");
    Assertions.assertEquals(actual.getToken().getName(), query);
  }

  @Test
  void testSearch_thenReturnMatchAddress_isStakeAddress() {
    String query = "stakeAddress";

    StakeAddress stakeAddress = StakeAddress.builder().view(query).build();

    when(stakeAddressRepository.findByView(query)).thenReturn(Optional.of(stakeAddress));
    var actual = searchService.search(query);

    Assertions.assertEquals(actual.getAddress().getAddress(), query);
    Assertions.assertEquals(actual.getAddress().isStakeAddress(), true);
  }

  @Test
  void testSearch_thenReturnMatchPool() {
    String query = "query";
    PoolInfoProjection poolInfoProjection = mock(PoolInfoProjection.class);
    when(poolInfoProjection.getPoolView()).thenReturn(query);

    when(poolHashRepository.findByPoolNameLike(query, PageRequest.of(0, 2)))
        .thenReturn(List.of(poolInfoProjection));

    var actual = searchService.search(query);

    Assertions.assertEquals(actual.getPool().getPoolId(), query);
    Assertions.assertEquals(actual.isValidPoolName(), true);
  }

  @Test
  void testSearch_thenReturnMatchScript() {
    String query = "query";
    Script script = Script.builder().hash(query).type(ScriptType.PLUTUSV3).build();

    when(scriptRepository.findByHash(query)).thenReturn(Optional.of(script));
    var actual = searchService.search(query);

    Assertions.assertEquals(actual.getScript().getScriptHash(), query);
    Assertions.assertEquals(actual.getScript().isSmartContract(), true);
    Assertions.assertEquals(actual.getScript().isNativeScript(), false);
  }
}
