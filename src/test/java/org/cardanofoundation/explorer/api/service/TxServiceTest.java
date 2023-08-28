package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;

import org.cardanofoundation.explorer.api.common.enumeration.CertificateType;
import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.DelegationMapper;
import org.cardanofoundation.explorer.api.mapper.MaTxMintMapper;
import org.cardanofoundation.explorer.api.mapper.ProtocolMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.mapper.TxContractMapper;
import org.cardanofoundation.explorer.api.mapper.TxMapper;
import org.cardanofoundation.explorer.api.mapper.TxOutMapper;
import org.cardanofoundation.explorer.api.mapper.WithdrawalMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxSummary;
import org.cardanofoundation.explorer.api.model.response.pool.PoolRelayResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.model.response.tx.CollateralResponse;
import org.cardanofoundation.explorer.api.model.response.tx.ContractResponse;
import org.cardanofoundation.explorer.api.model.response.tx.ProtocolParamResponse;
import org.cardanofoundation.explorer.api.model.response.tx.SummaryResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxDelegationResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxInfoResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxMetadataResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxMintingResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxOutResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxPoolCertificate;
import org.cardanofoundation.explorer.api.model.response.tx.TxResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxStakeCertificate;
import org.cardanofoundation.explorer.api.model.response.tx.UTxOResponse;
import org.cardanofoundation.explorer.api.model.response.tx.WithdrawalResponse;
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.AddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.repository.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.FailedTxOutRepository;
import org.cardanofoundation.explorer.api.repository.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ParamProposalRepository;
import org.cardanofoundation.explorer.api.repository.PoolRelayRepository;
import org.cardanofoundation.explorer.api.repository.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.RedeemerRepository;
import org.cardanofoundation.explorer.api.repository.ReserveRepository;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.StakeDeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.StakeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.TreasuryRepository;
import org.cardanofoundation.explorer.api.repository.TxChartRepository;
import org.cardanofoundation.explorer.api.repository.TxMetadataRepository;
import org.cardanofoundation.explorer.api.repository.TxOutRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.repository.UnconsumeTxInRepository;
import org.cardanofoundation.explorer.api.repository.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.impl.TxServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.AddressInputOutputProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.PoolDeRegistrationProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.PoolRelayProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.PoolUpdateDetailProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.StakeKeyProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.TxContractProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.TxIOProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.TxInstantaneousRewardsProjectionImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AddressToken;
import org.cardanofoundation.explorer.consumercommon.entity.AddressTxBalance;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.StakeDeregistration;
import org.cardanofoundation.explorer.consumercommon.entity.StakeRegistration;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.TxMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.TxMetadataHash;
import org.cardanofoundation.explorer.consumercommon.entity.Withdrawal;
import org.mockito.Answers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(MockitoExtension.class)
class TxServiceTest {

  @Mock
  private TxRepository txRepository;
  @Mock
  private TxOutRepository txOutRepository;
  @Mock
  private BlockRepository blockRepository;
  @Mock
  private TxMapper txMapper;
  @Mock
  private TxOutMapper txOutMapper;
  @Mock
  private TokenMapper tokenMapper;
  @Mock
  private RedeemerRepository redeemerRepository;
  @Mock
  private EpochRepository epochRepository;
  @Mock
  private UnconsumeTxInRepository unconsumeTxInRepository;
  @Mock
  private FailedTxOutRepository failedTxOutRepository;
  @Mock
  private WithdrawalRepository withdrawalRepository;
  @Mock
  private AddressRepository addressRepository;
  @Mock
  private WithdrawalMapper withdrawalMapper;
  @Mock
  private DelegationRepository delegationRepository;
  @Mock
  private DelegationMapper delegationMapper;
  @Mock
  private MaTxMintRepository maTxMintRepository;
  @Mock
  private MaTxMintMapper maTxMintMapper;
  @Mock
  private AddressTxBalanceRepository addressTxBalanceRepository;
  @Mock
  private MultiAssetRepository multiAssetRepository;
  @Mock
  private AddressTokenRepository addressTokenRepository;
  @Mock
  private AssetMetadataRepository assetMetadataRepository;
  @Mock
  private AssetMetadataMapper assetMetadataMapper;
  @Mock
  private StakeRegistrationRepository stakeRegistrationRepository;
  @Mock
  private StakeDeRegistrationRepository stakeDeRegistrationRepository;
  @Mock
  private PoolUpdateRepository poolUpdateRepository;
  @Mock
  private PoolRelayRepository poolRelayRepository;
  @Mock
  private PoolRetireRepository poolRetireRepository;
  @Mock
  private ParamProposalRepository paramProposalRepository;
  @Mock
  private EpochParamRepository epochParamRepository;
  @Mock
  private ProtocolMapper protocolMapper;
  @Mock
  private TxChartRepository txChartRepository;
  @Mock
  private TreasuryRepository treasuryRepository;
  @Mock
  private ReserveRepository reserveRepository;
  @Mock
  private StakeAddressRepository stakeAddressRepository;
  @Mock
  private TxContractMapper txContractMapper;
  @Mock
  private TxMetadataRepository txMetadataRepository;

  @Mock(answer = Answers.RETURNS_DEEP_STUBS)
  private RedisTemplate<String, TxGraph> redisTemplate;

  @InjectMocks
  private TxServiceImpl txService;

  @Test
  void testFindLatestTxSummary() {
    final List<TxSummary> expect = List.of(TxSummary.builder()
        .blockNo(1000000L)
        .fromAddress(List.of(
            "addr1qyx9ezl77cf8n3qs4hw2gw0ewn4l8c095k6qfjy3un9qnw9ywmzs2czds2nthq8uqenjc06p9l40xgyh5q4xmx7d2cgs3xwhx4"))
        .toAddress(List.of(
            "addr1zyq0kyrml023kwjk8zr86d5gaxrt5w8lxnah8r6m6s4jp4g3r6dxnzml343sx8jweqn4vn3fz2kj8kgu9czghx0jrsyqqktyhv"))
        .amount(456350000000.0)
        .hash("29c8a63ac4ff2bdb630656e9e568c3e526ef316b280b7123b9a9a9719f9ce8d7")
        .epochNo(410)
        .epochSlotNo(102927)
        .slot(99203727)
        .time(LocalDateTime.of(2020, 1, 1, 0, 0, 0))
        .status(TxStatus.SUCCESS)
        .build());
    final TxIOProjection txIOProjection = TxIOProjectionImpl.builder()
        .id(0L)
        .blockNo(1000000L)
        .fromAddress(
            "addr1qyx9ezl77cf8n3qs4hw2gw0ewn4l8c095k6qfjy3un9qnw9ywmzs2czds2nthq8uqenjc06p9l40xgyh5q4xmx7d2cgs3xwhx4")
        .toAddress(
            "addr1zyq0kyrml023kwjk8zr86d5gaxrt5w8lxnah8r6m6s4jp4g3r6dxnzml343sx8jweqn4vn3fz2kj8kgu9czghx0jrsyqqktyhv")
        .amount(BigInteger.valueOf(456350000000L))
        .hash("29c8a63ac4ff2bdb630656e9e568c3e526ef316b280b7123b9a9a9719f9ce8d7")
        .epochNo(410)
        .epochSlotNo(102927)
        .slot(99203727)
        .time(LocalDateTime.of(2020, 1, 1, 0, 0, 0))
        .validContract(Boolean.TRUE)
        .build();

    when(txRepository.findLatestTxId(any())).thenReturn(List.of(0L));
    when(txRepository.findLatestTxIO(List.of(0L))).thenReturn(List.of(txIOProjection));

    final List<TxSummary> result = txService.findLatestTxSummary();

    assertEquals(expect, result);
  }

  @Test
  void testFindLatestTxSummary_WhenTxRepositoryFindLatestTxIdReturnsNoItems() {
    when(txRepository.findLatestTxId(any())).thenReturn(Collections.emptyList());

    final List<TxSummary> result = txService.findLatestTxSummary();

    assertEquals(List.of(), result);
  }

  @Test
  void testFindLatestTxSummary_WhenTxRepositoryFindLatestTxIOReturnsNoItems() {
    when(txRepository.findLatestTxId(any())).thenReturn(List.of(0L));
    when(txRepository.findLatestTxIO(List.of(0L))).thenReturn(Collections.emptyList());

    final List<TxSummary> result = txService.findLatestTxSummary();

    assertEquals(List.of(), result);
  }

  @Test
  void testGetAll() {
    // Setup
    final Tx tx = new Tx();
    tx.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(400);
    tx.setBlock(block);
    tx.setBlockId(0L);
    tx.setValidContract(true);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("e2ebea596eea2b218b2ce40b8f21179fb57edf069e4f693741aadca1a101c0ba");
    tx.setTxMetadataHash(txMetadataHash);

    final Page<Tx> txs = new PageImpl<>(List.of(tx));
    when(txRepository.findAllTx(any(Pageable.class))).thenReturn(txs);
    when(blockRepository.findAllByIdIn(any())).thenReturn(
        List.of(Block.builder().id(0L).build()));
    when(txOutRepository.findAddressInputListByTxId(any())).thenReturn(List.of(
        AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZLC5VMKspod9dcf5PtUtvobxd3THtoxfR9uuzcqDghef9oiTc")
            .build()
    ));
    when(txOutRepository.findAddressOutputListByTxId(any())).thenReturn(List.of(
        AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq")
            .build()
    ));

    when(txMapper.txToTxFilterResponse(any())).thenReturn(new TxFilterResponse());

    final TxFilterResponse expect = new TxFilterResponse();
    expect.setAddressesInput(
        List.of("Ae2tdPwUPEZLC5VMKspod9dcf5PtUtvobxd3THtoxfR9uuzcqDghef9oiTc"));
    expect.setAddressesOutput(
        List.of("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq"));

    // Run the test
    final BaseFilterResponse<TxFilterResponse> result = txService.getAll(
        PageRequest.of(0, 1));

    // Verify the results
    verify(txMapper, times(1)).txToTxFilterResponse(tx);
    assertEquals(1, result.getData().size());
    assertEquals(expect.getAddressesInput(), result.getData().get(0).getAddressesInput());
    assertEquals(expect.getAddressesOutput(), result.getData().get(0).getAddressesOutput());
  }

  @Test
  void testGetAll_WhenTxRepositoryReturnsNoItems() {
    when(txRepository.findAllTx(any(Pageable.class)))
        .thenReturn(new PageImpl<>(Collections.emptyList()));

    final BaseFilterResponse<TxFilterResponse> result = txService.getAll(
        PageRequest.of(0, 1));

    assertEquals(List.of(), result.getData());
  }

  @Test
  void testGetAll_WhenTxOutRepositoryFindAddressInputListByTxIdReturnsNoItems() {
    // Setup
    final Tx tx = new Tx();
    tx.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx.setBlock(block);
    tx.setBlockId(0L);
    tx.setValidContract(false);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("e2ebea596eea2b218b2ce40b8f21179fb57edf069e4f693741aadca1a101c0ba");
    tx.setTxMetadataHash(txMetadataHash);
    final Page<Tx> txs = new PageImpl<>(List.of(tx));
    when(txRepository.findAllTx(any(Pageable.class))).thenReturn(txs);

    when(blockRepository.findAllByIdIn(any())).thenReturn(List.of(new Block()));
    when(txOutRepository.findAddressInputListByTxId(any()))
        .thenReturn(Collections.emptyList());
    when(txOutRepository.findAddressOutputListByTxId(any())).thenReturn(
        List.of(AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq")
            .build()));

    when(txMapper.txToTxFilterResponse(any())).thenReturn(new TxFilterResponse());

    final TxFilterResponse expect = new TxFilterResponse();
    expect.setAddressesInput(List.of());
    expect.setAddressesOutput(
        List.of("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq"));

    // Run the test
    final BaseFilterResponse<TxFilterResponse> result = txService.getAll(
        PageRequest.of(0, 1));

    // Verify the results
    verify(txMapper, times(1)).txToTxFilterResponse(tx);
    assertEquals(1, result.getData().size());
    assertEquals(expect.getAddressesInput(), result.getData().get(0).getAddressesInput());
    assertEquals(expect.getAddressesOutput(), result.getData().get(0).getAddressesOutput());
  }

  @Test
  void testGetAll_WhenTxOutRepositoryFindAddressOutputListByTxIdReturnsNoItems() {
    // Setup
    final Tx tx = new Tx();
    tx.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx.setBlock(block);
    tx.setBlockId(0L);
    tx.setValidContract(false);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("e2ebea596eea2b218b2ce40b8f21179fb57edf069e4f693741aadca1a101c0ba");
    tx.setTxMetadataHash(txMetadataHash);
    final Page<Tx> txs = new PageImpl<>(List.of(tx));
    when(txRepository.findAllTx(any(Pageable.class))).thenReturn(txs);

    when(blockRepository.findAllByIdIn(any())).thenReturn(List.of(new Block()));
    when(txOutRepository.findAddressInputListByTxId(any())).thenReturn(
        List.of(AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq")
            .build()));
    when(txOutRepository.findAddressOutputListByTxId(any()))
        .thenReturn(Collections.emptyList());
    when(txMapper.txToTxFilterResponse(any())).thenReturn(new TxFilterResponse());

    final TxFilterResponse expect = new TxFilterResponse();
    expect.setAddressesInput(
        List.of("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq"));
    expect.setAddressesOutput(List.of());

    // Run the test
    final BaseFilterResponse<TxFilterResponse> result = txService.getAll(
        PageRequest.of(0, 1));

    // Verify the results
    verify(txMapper, times(1)).txToTxFilterResponse(tx);
    assertEquals(1, result.getData().size());
    assertEquals(expect.getAddressesInput(), result.getData().get(0).getAddressesInput());
    assertEquals(expect.getAddressesOutput(), result.getData().get(0).getAddressesOutput());
  }

  @Test
  void testGetTransactionsByBlock() {
    // Setup
    final Tx tx = new Tx();
    tx.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx.setBlock(block);
    tx.setBlockId(0L);
    tx.setValidContract(false);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("e2ebea596eea2b218b2ce40b8f21179fb57edf069e4f693741aadca1a101c0ba");
    tx.setTxMetadataHash(txMetadataHash);
    final Page<Tx> txs = new PageImpl<>(List.of(tx));
    when(txRepository.findByBlockNo(eq(0L), any(Pageable.class))).thenReturn(txs);

    when(blockRepository.findAllByIdIn(any())).thenReturn(List.of(block));
    when(txOutRepository.findAddressInputListByTxId(any())).thenReturn(List.of(
        AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZLC5VMKspod9dcf5PtUtvobxd3THtoxfR9uuzcqDghef9oiTc")
            .build()
    ));
    when(txOutRepository.findAddressOutputListByTxId(any())).thenReturn(List.of(
        AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq")
            .build()
    ));

    final TxFilterResponse expect = new TxFilterResponse();
    expect.setAddressesInput(
        List.of("Ae2tdPwUPEZLC5VMKspod9dcf5PtUtvobxd3THtoxfR9uuzcqDghef9oiTc"));
    expect.setAddressesOutput(
        List.of("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq"));

    when(txMapper.txToTxFilterResponse(any())).thenReturn(new TxFilterResponse());

    // Run the test
    final BaseFilterResponse<TxFilterResponse> result = txService.getTransactionsByBlock(
        "0", PageRequest.of(0, 1));

    // Verify the results
    verify(txMapper, times(1)).txToTxFilterResponse(tx);
    assertEquals(1, result.getData().size());
    assertEquals(expect.getAddressesInput(), result.getData().get(0).getAddressesInput());
    assertEquals(expect.getAddressesOutput(), result.getData().get(0).getAddressesOutput());
  }

  @Test
  void testGetTransactionsByBlock_WhenTxRepositoryFindByBlockNoReturnsNoItems() {
    // Setup
    when(txRepository.findByBlockNo(eq(0L), any(Pageable.class)))
        .thenReturn(new PageImpl<>(Collections.emptyList()));

    // Run the test
    final BaseFilterResponse<TxFilterResponse> result = txService.getTransactionsByBlock(
        "0", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(List.of(), result.getData());
  }

  @Test
  void testGetTransactionsByBlock_WhenBlockIdIsHash() {
    final String blockId = "67f852c0d4214d8291e3c887c180b105c6cf859d79284de6aea4cdcfb186d4e5";

    final Tx tx = new Tx();
    tx.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(400);
    block.setHash(blockId);
    tx.setBlock(block);
    tx.setBlockId(0L);
    tx.setValidContract(false);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("e2ebea596eea2b218b2ce40b8f21179fb57edf069e4f693741aadca1a101c0ba");
    tx.setTxMetadataHash(txMetadataHash);
    final Page<Tx> txs = new PageImpl<>(List.of(tx));
    when(txRepository.findByBlockHash(blockId, PageRequest.of(0, 1))).thenReturn(txs);

    when(blockRepository.findAllByIdIn(Set.of(0L))).thenReturn(List.of(new Block()));
    when(txOutRepository.findAddressInputListByTxId(Set.of(0L))).thenReturn(List.of(
        AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZLC5VMKspod9dcf5PtUtvobxd3THtoxfR9uuzcqDghef9oiTc")
            .build()
    ));
    when(txOutRepository.findAddressOutputListByTxId(any())).thenReturn(List.of(
        AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq")
            .build()
    ));

    when(txMapper.txToTxFilterResponse(any())).thenReturn(new TxFilterResponse());
    final BaseFilterResponse<TxFilterResponse> result = txService.getTransactionsByBlock(
        blockId, PageRequest.of(0, 1));

    verify(txRepository).findByBlockHash(blockId, PageRequest.of(0, 1));
  }

  @Test
  void testGetTransactionsByAddress() {
    // Setup
    final Address address = Address.builder()
        .id(0L)
        .address("Ae2tdPwUPEZ1pRs1gSidtoRGMpJR54UyNrdVDMFxXu2pBkhxitAWhqrGqd9")
        .txCount(1L)
        .balance(new BigInteger("100"))
        .addressHasScript(false)
        .build();
    final Optional<Address> addressOpt = Optional.of(address);
    when(addressRepository.findFirstByAddress(anyString())).thenReturn(addressOpt);

    final Tx tx = new Tx();
    tx.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx.setBlock(block);
    tx.setBlockId(0L);
    tx.setValidContract(false);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("f684a50874a52d429e142cbd45f12363d9c61419cd03668319ecf57be35d5933");
    tx.setTxMetadataHash(txMetadataHash);
    final List<Tx> txs = List.of(tx);

    when(addressTxBalanceRepository.findAllByAddress(eq(address),
        any(Pageable.class))).thenReturn(txs);

    when(blockRepository.findAllByIdIn(any())).thenReturn(List.of(block));
    when(txOutRepository.findAddressInputListByTxId(any())).thenReturn(List.of());
    when(txOutRepository.findAddressOutputListByTxId(any())).thenReturn(List.of());
    TxFilterResponse txFilterResponse = new TxFilterResponse();
    txFilterResponse.setId(0L);
    when(txMapper.txToTxFilterResponse(any())).thenReturn(
        txFilterResponse);

    final AddressTxBalance addressTxBalance = AddressTxBalance.builder()
        .id(0L)
        .address(address)
        .txId(0L)
        .balance(new BigInteger("100"))
        .build();

    final List<AddressTxBalance> addressTxBalances = List.of(addressTxBalance);
    when(
        addressTxBalanceRepository.findByTxIdInAndByAddress(any(), eq(address.getAddress())))
        .thenReturn(addressTxBalances);
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .txCount(10L)
        .build();
    final AddressToken addressToken = AddressToken.builder()
        .address(address)
        .multiAssetId(0L)
        .multiAsset(multiAsset)
        .tx(tx)
        .balance(new BigInteger("100"))
        .build();
    final List<AddressToken> addressTokens = List.of(addressToken);
    when(addressTokenRepository.findByTxIdInAndByAddress(any(), eq(address.getAddress())))
        .thenReturn(addressTokens);

    final List<MultiAsset> multiAssets = List.of(multiAsset);
    when(multiAssetRepository.findAllByIdIn(any())).thenReturn(multiAssets);

    final AssetMetadata assetMetadata1 = AssetMetadata.builder()
        .id(0L)
        .subject(multiAsset.getPolicy() + multiAsset.getName())
        .build();
    final List<AssetMetadata> assetMetadata = List.of(assetMetadata1);
    when(assetMetadataRepository.findBySubjectIn(any())).thenReturn(assetMetadata);

    final TokenAddressResponse tokenAddressResponse = TokenAddressResponse.builder()
        .address("address")
        .metadata(TokenMetadataResponse.builder().build())
        .build();

    when(tokenMapper.fromMultiAssetAndAddressToken(any(), any(AddressToken.class)))
        .thenReturn(tokenAddressResponse);

    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");
    when(assetMetadataMapper.fromAssetMetadata(any())).thenReturn(tokenMetadataResponse);

    // Run the test
    final BaseFilterResponse<TxFilterResponse> result = txService.getTransactionsByAddress(
        address.getAddress(), PageRequest.of(0, 1));

    final TxFilterResponse expect = new TxFilterResponse();
    expect.setBalance(addressTxBalance.getBalance());
    expect.setTokens(List.of(tokenAddressResponse));
    // Verify the results
    verify(tokenMapper, times(1)).fromMultiAssetAndAddressToken(multiAsset, addressToken);
    verify(assetMetadataMapper, times(1)).fromAssetMetadata(assetMetadata1);
  }

  @Test
  void testGetTransactionsByAddress_WhenAddressRepositoryReturnsAbsent() {
    // Setup
    when(addressRepository.findFirstByAddress("address")).thenReturn(Optional.empty());

    // Run the test
    assertThrows(NoContentException.class, () -> txService.getTransactionsByAddress("address",
        PageRequest.of(0, 1)));
  }

  @Test
  void testGetTransactionsByToken() {
    // Setup
    final MultiAsset multiAsset1 = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .txCount(1L)
        .build();

    final Optional<MultiAsset> multiAsset = Optional.of(multiAsset1);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAsset);

    when(addressTokenRepository.findTxsByMultiAsset(eq(multiAsset1),
        any(Pageable.class))).thenReturn(List.of(0L));

    final Tx tx = new Tx();
    tx.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx.setBlock(block);
    tx.setBlockId(0L);
    tx.setValidContract(false);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("hash");
    tx.setTxMetadataHash(txMetadataHash);
    final List<Tx> txs = List.of(tx);
    when(txRepository.findByIdIn(any())).thenReturn(txs);

    when(blockRepository.findAllByIdIn(any())).thenReturn(
        List.of(block));
    when(txOutRepository.findAddressInputListByTxId(any())).thenReturn(List.of(
        AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZLC5VMKspod9dcf5PtUtvobxd3THtoxfR9uuzcqDghef9oiTc")
            .build()
    ));
    when(txOutRepository.findAddressOutputListByTxId(any())).thenReturn(List.of(
        AddressInputOutputProjectionImpl.builder()
            .txId(0L)
            .address("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq")
            .build()
    ));

    when(txMapper.txToTxFilterResponse(any())).thenReturn(new TxFilterResponse());

    final TxFilterResponse expect = new TxFilterResponse();
    expect.setAddressesInput(
        List.of("Ae2tdPwUPEZLC5VMKspod9dcf5PtUtvobxd3THtoxfR9uuzcqDghef9oiTc"));
    expect.setAddressesOutput(
        List.of("Ae2tdPwUPEZJAUCigmqNN4Lh5sZ3E2FZn75EjVbdEKVxAirKzh4zHkdYpBq"));

    // Run the test
    final BaseFilterResponse<TxFilterResponse> result = txService.getTransactionsByToken(
        "tokenId", PageRequest.of(0, 1));

    // Verify the results
    verify(txMapper, times(1)).txToTxFilterResponse(tx);
    assertEquals(1, result.getData().size());
    assertEquals(expect.getAddressesInput(), result.getData().get(0).getAddressesInput());
    assertEquals(expect.getAddressesOutput(), result.getData().get(0).getAddressesOutput());
  }

  @Test
  void testGetTransactionsByToken_MultiAssetRepositoryReturnsAbsent() {
    // Setup
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(Optional.empty());

    // Run the test
    final BaseFilterResponse<TxFilterResponse> result = txService.getTransactionsByToken(
        "tokenId", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(0, result.getTotalItems());
  }

  @Test
  void testGetTransactionsByStake() {
    // Setup
    final StakeAddress stakeAddress1 = StakeAddress.builder()
        .id(0L)
        .hashRaw("hashRaw")
        .view("stakeAddress")
        .scriptHash("scriptHash")
        .balance(new BigInteger("100"))
        .build();

    final Optional<StakeAddress> stakeAddress = Optional.of(stakeAddress1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(stakeAddress);

    final Tx tx = new Tx();
    tx.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx.setBlock(block);
    tx.setBlockId(0L);
    tx.setValidContract(false);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("hash");
    tx.setTxMetadataHash(txMetadataHash);
    final Page<Tx> txs = new PageImpl<>(List.of(tx));
    when(addressTxBalanceRepository.findAllByStake(any(), any(Pageable.class)))
        .thenReturn(txs);

    when(blockRepository.findAllByIdIn(any())).thenReturn(List.of(block));
    when(txOutRepository.findAddressInputListByTxId(any())).thenReturn(List.of());
    when(txOutRepository.findAddressOutputListByTxId(any())).thenReturn(List.of());

    TxFilterResponse txFilterResponse = new TxFilterResponse();
    txFilterResponse.setId(0L);
    when(txMapper.txToTxFilterResponse(any())).thenReturn(
        txFilterResponse);
    when(txMapper.txToTxFilterResponse(tx)).thenReturn(txFilterResponse);

    final AddressTxBalance addressTxBalance = AddressTxBalance.builder()
        .id(0L)
        .stakeAddress(stakeAddress1)
        .txId(0L)
        .balance(new BigInteger("100"))
        .build();

    final List<AddressTxBalance> addressTxBalances = List.of(addressTxBalance);
    when(addressTxBalanceRepository.findByTxIdInAndStakeId(any(), eq(stakeAddress1.getId())))
        .thenReturn(addressTxBalances);

    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .txCount(10L)
        .build();
    final AddressToken addressToken = AddressToken.builder()
        .address(Address.builder().id(0L).stakeAddress(stakeAddress1).build())
        .multiAssetId(0L)
        .multiAsset(multiAsset)
        .tx(tx)
        .balance(new BigInteger("100"))
        .build();
    final List<AddressToken> addressTokens = List.of(addressToken);
    when(addressTokenRepository.findByTxIdInAndStakeId(any(), eq(stakeAddress1.getId())))
        .thenReturn(addressTokens);
    final List<MultiAsset> multiAssets = List.of(multiAsset);
    when(multiAssetRepository.findAllByIdIn(List.of(0L))).thenReturn(multiAssets);
    //

    final AssetMetadata assetMetadata1 = AssetMetadata.builder()
        .id(0L)
        .subject(multiAsset.getPolicy() + multiAsset.getName())
        .build();
    final List<AssetMetadata> assetMetadata = List.of(assetMetadata1);

    when(assetMetadataRepository.findBySubjectIn(any())).thenReturn(assetMetadata);

    final TokenAddressResponse tokenAddressResponse = TokenAddressResponse.builder()
        .address("address")
        .metadata(TokenMetadataResponse.builder().build())
        .build();

    when(tokenMapper.fromMultiAssetAndAddressToken(any(), any(AddressToken.class)))
        .thenReturn(tokenAddressResponse);

    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");
    when(assetMetadataMapper.fromAssetMetadata(any())).thenReturn(tokenMetadataResponse);
    // Run the test
    final BaseFilterResponse<TxFilterResponse> result = txService.getTransactionsByStake(
        "stakeKey", PageRequest.of(0, 1));

    final TxFilterResponse expect = new TxFilterResponse();
    expect.setBalance(addressTxBalance.getBalance());
    expect.setTokens(List.of(tokenAddressResponse));
    // Verify the results
    verify(tokenMapper, times(1)).fromMultiAssetAndAddressToken(multiAsset, addressToken);
    verify(assetMetadataMapper, times(1)).fromAssetMetadata(assetMetadata1);
  }

  @Test
  void testGetTransactionsByStake_StakeAddressRepositoryReturnsAbsent() {
    // Setup
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.empty());

    // Run the test
    assertThrows(NoContentException.class, () -> txService.getTransactionsByStake("stakeKey",
        PageRequest.of(0, 1)));
  }

  @Test
  void testGetTxContractDetailForTxSuccess() {
    // Setup
    // Configure TxRepository.findByHash(...).
    final Tx tx1 = new Tx();
    tx1.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx1.setBlock(block);
    tx1.setBlockId(0L);
    tx1.setValidContract(true);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("hash");
    tx1.setTxMetadataHash(txMetadataHash);
    final Optional<Tx> tx = Optional.of(tx1);
    when(txRepository.findByHash(anyString())).thenReturn(tx);

    // Configure RedeemerRepository.findContractByTx(...).
    when(redeemerRepository.findContractByTx(tx1)).thenReturn(List.of(
        TxContractProjectionImpl.builder().build()
    ));

    // Configure TxContractMapper.fromTxContractProjectionToContractResponse(...).
    final ContractResponse contractResponse = ContractResponse.builder()
        .address("address")
        .scriptHash("scriptHash")
        .build();
    when(txContractMapper.fromTxContractProjectionToContractResponse(
        any(TxContractProjection.class))).thenReturn(contractResponse);

    // Configure TxOutRepository.getContractDatumOutByTx(...).
    when(txOutRepository.getContractDatumOutByTx(tx1)).thenReturn(List.of(
        TxContractProjectionImpl.builder()
            .address("address")
            .datumBytesOut("datumBytesOut".getBytes())
            .datumHashOut("datumHashOut")
            .build()
    ));

    when(txContractMapper.bytesToString(any(byte[].class))).thenReturn("datumBytesOut");

    // Run the test
    final List<ContractResponse> result = txService.getTxContractDetail("txHash",
        "address");

    // Verify the results
    assertEquals(contractResponse.getDatumBytesOut(), result.get(0).getDatumBytesOut());
    assertEquals(contractResponse.getDatumHashOut(), result.get(0).getDatumHashOut());
  }

  @Test
  void testGetTxContractDetailForTxFail() {
    // Setup
    // Configure TxRepository.findByHash(...).
    final Tx tx1 = new Tx();
    tx1.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx1.setBlock(block);
    tx1.setBlockId(0L);
    tx1.setValidContract(false);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("hash");
    tx1.setTxMetadataHash(txMetadataHash);
    final Optional<Tx> tx = Optional.of(tx1);
    when(txRepository.findByHash(anyString())).thenReturn(tx);

    // Configure RedeemerRepository.findContractByTx(...).
    when(redeemerRepository.findContractByTxFail(tx1)).thenReturn(List.of(
        TxContractProjectionImpl.builder().build()
    ));

    // Configure TxContractMapper.fromTxContractProjectionToContractResponse(...).
    final ContractResponse contractResponse = ContractResponse.builder()
        .address("address")
        .scriptHash("scriptHash")
        .build();
    when(txContractMapper.fromTxContractProjectionToContractResponse(
        any(TxContractProjection.class))).thenReturn(contractResponse);

    // Configure TxOutRepository.getContractDatumOutByTx(...).
    when(txOutRepository.getContractDatumOutByTxFail(tx1)).thenReturn(List.of(
        TxContractProjectionImpl.builder()
            .address("address")
            .datumBytesOut("datumBytesOut".getBytes())
            .datumHashOut("datumHashOut")
            .build()
    ));

    when(txContractMapper.bytesToString(any(byte[].class))).thenReturn("datumBytesOut");

    // Run the test
    final List<ContractResponse> result = txService.getTxContractDetail("txHash",
                                                                        "address");

    // Verify the results
    assertEquals(contractResponse.getDatumBytesOut(), result.get(0).getDatumBytesOut());
    assertEquals(contractResponse.getDatumHashOut(), result.get(0).getDatumHashOut());
  }


  @Test
  void testGetTxContractDetail_TxRepositoryReturnsAbsent() {
    // Setup
    when(txRepository.findByHash("txHash")).thenReturn(Optional.empty());

    // Run the test
    assertThrows(BusinessException.class,
        () -> txService.getTxContractDetail("txHash", "address"));
  }

  @Test
  void testGetTxDetailByHash_TxRepositoryReturnsAbsent() {
    // Setup
    when(txRepository.findByHash("hash")).thenReturn(Optional.empty());

    // Run the test
    assertThrows(BusinessException.class, () -> txService.getTxDetailByHash("hash"));
  }

  @Test
  void testGetTxDetailByHash_BlockRepositoryReturnsAbsent() {
    // Setup
    // Configure TxRepository.findByHash(...).
    final Tx tx1 = new Tx();
    tx1.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx1.setBlock(block);
    tx1.setBlockId(0L);
    tx1.setValidContract(false);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("hash");
    tx1.setTxMetadataHash(txMetadataHash);
    final Optional<Tx> tx = Optional.of(tx1);
    when(txRepository.findByHash("hash")).thenReturn(tx);

    when(blockRepository.findCurrentBlock()).thenReturn(Optional.empty());

    // Run the test
    assertThrows(BusinessException.class, () -> txService.getTxDetailByHash("hash"));
  }

  @Test
  void testGetTxDetailByHash_EpochRepositoryReturnsAbsent() {
    // Setup
    // Configure TxRepository.findByHash(...).
    final Tx tx1 = new Tx();
    tx1.setId(0L);
    final Block block = new Block();
    block.setId(0L);
    block.setEpochNo(0);
    tx1.setBlock(block);
    tx1.setBlockId(0L);
    tx1.setValidContract(true);
    final TxMetadataHash txMetadataHash = new TxMetadataHash();
    txMetadataHash.setId(0L);
    txMetadataHash.setHash("hash");
    tx1.setTxMetadataHash(txMetadataHash);
    final Optional<Tx> tx = Optional.of(tx1);
    when(txRepository.findByHash(anyString())).thenReturn(tx);

    when(blockRepository.findCurrentBlock()).thenReturn(Optional.of(1));

    final TxResponse txResponse = new TxResponse();
    txResponse.setTx(TxInfoResponse.builder().epochNo(1).build());
    when(txMapper.txToTxResponse(tx1)).thenReturn(txResponse);

    when(epochRepository.findFirstByNo(any())).thenReturn(Optional.empty());

    // Run the test
    assertThrows(BusinessException.class, () -> txService.getTxDetailByHash("hash"));
  }

}
