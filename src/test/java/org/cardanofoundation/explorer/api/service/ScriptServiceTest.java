package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.SliceImpl;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.util.ReflectionTestUtils;

import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.Address;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.common.enumeration.TxPurposeType;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapperImpl;
import org.cardanofoundation.explorer.api.mapper.ScriptMapper;
import org.cardanofoundation.explorer.api.mapper.ScriptMapperImpl;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapperImpl;
import org.cardanofoundation.explorer.api.model.request.script.nativescript.NativeScriptFilterRequest;
import org.cardanofoundation.explorer.api.model.request.script.smartcontract.SmartContractFilterRequest;
import org.cardanofoundation.explorer.api.model.response.tx.ContractResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.projection.TokenProjection;
import org.cardanofoundation.explorer.api.repository.explorer.NativeScriptInfoRepository;
import org.cardanofoundation.explorer.api.repository.explorer.SmartContractInfoRepository;
import org.cardanofoundation.explorer.api.repository.explorer.VerifiedScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.LatestTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RedeemerRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.service.impl.ScriptServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.SmartContractTxProjectionImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptPurposeType;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.explorer.NativeScriptInfo;
import org.cardanofoundation.explorer.common.entity.explorer.SmartContractInfo;
import org.cardanofoundation.explorer.common.entity.explorer.VerifiedScript;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
import org.cardanofoundation.explorer.common.entity.ledgersync.Script;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@ExtendWith(MockitoExtension.class)
class ScriptServiceTest {

  @Mock ScriptRepository scriptRepository;
  @Mock StakeAddressRepository stakeAddressRepository;
  @Mock TxRepository txRepository;
  @Mock RedeemerRepository redeemerRepository;
  @Mock AddressRepository addressRepository;
  @Mock MultiAssetRepository multiAssetRepository;
  @Mock NativeScriptInfoRepository nativeScriptInfoRepository;
  @Mock BlockRepository blockRepository;
  @Mock SmartContractInfoRepository smartContractInfoRepository;
  @Mock LatestTokenBalanceRepository latestTokenBalanceRepository;
  @Mock VerifiedScriptRepository verifiedScriptRepository;
  @Mock TxService txService;
  @Mock MaTxMintRepository maTxMintRepository;
  @InjectMocks ScriptServiceImpl scriptService;

  @BeforeEach
  void setup() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    ScriptMapper scriptMapper = new ScriptMapperImpl();
    AssetMetadataMapper assetMetadataMapper = new AssetMetadataMapperImpl();
    TokenMapper tokenMapper = new TokenMapperImpl();
    ReflectionTestUtils.setField(scriptService, "scriptMapper", scriptMapper);
    ReflectionTestUtils.setField(scriptService, "assetMetadataMapper", assetMetadataMapper);
    ReflectionTestUtils.setField(scriptService, "tokenMapper", tokenMapper);
    when(blockRepository.findFirstShellyBlock())
        .thenReturn(
            Optional.of(
                Block.builder()
                    .blockNo(1L)
                    .slotNo(1L)
                    .time(Timestamp.valueOf("2020-07-29 21:44:51"))
                    .build()));
    when(blockRepository.findFirstBlock())
        .thenReturn(
            Optional.of(
                Block.builder()
                    .blockNo(2L)
                    .slotNo(2L)
                    .time(Timestamp.valueOf("2017-09-23 21:44:51"))
                    .build()));
    Method postConstruct =
        ScriptServiceImpl.class.getDeclaredMethod("init"); // methodName,parameters
    postConstruct.setAccessible(true);
    postConstruct.invoke(scriptService);
  }

  @Test
  void testGetNativeScripts_thenReturn() {
    // Given
    Pageable pageable = PageRequest.of(0, 1);
    List<NativeScriptInfo> scriptList =
        List.of(
            NativeScriptInfo.builder()
                .scriptHash("hash")
                .type(ScriptType.TIMELOCK)
                .id(1L)
                .beforeSlot(101L)
                .afterSlot(99L)
                .numberOfTokens(2L)
                .numberOfAssetHolders(3L)
                .build());
    NativeScriptFilterRequest request = new NativeScriptFilterRequest();
    // When and The
    when(blockRepository.findLatestBlock())
        .thenReturn(Optional.of(Block.builder().slotNo(100L).build()));
    when(nativeScriptInfoRepository.findAll(any(Specification.class), any(Pageable.class)))
        .thenReturn(new PageImpl<>(scriptList));
    when(multiAssetRepository.findTopMultiAssetByScriptHashIn(any())).thenReturn(List.of());
    var response = scriptService.getNativeScripts(request, pageable);
    // Assert
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("hash", response.getData().get(0).getScriptHash());
    Assertions.assertEquals(2, response.getData().get(0).getNumberOfTokens());
    Assertions.assertEquals(3, response.getData().get(0).getNumberOfAssetHolders());
    Assertions.assertTrue(response.getData().get(0).getIsOpen());
  }

  @Test
  void testGetNativeScripts_shouldThrowExceptionNotFoundLastestBlock() {
    Pageable pageable = PageRequest.of(0, 1);
    NativeScriptFilterRequest request = new NativeScriptFilterRequest();

    when(blockRepository.findLatestBlock()).thenReturn(Optional.empty());

    Assertions.assertThrows(
        BusinessException.class, () -> scriptService.getNativeScripts(request, pageable));
  }

  @Test
  void testGetSmartContracts() {
    // Given
    Pageable pageable = PageRequest.of(0, 1);
    SmartContractFilterRequest filterRequest =
        SmartContractFilterRequest.builder()
            .txPurpose(Set.of(TxPurposeType.SPEND, TxPurposeType.MINT))
            .scriptVersion(ScriptType.PLUTUSV1)
            .isScriptMint(true)
            .isScriptSpend(true)
            .isScriptNone(false)
            .isScriptAny(false)
            .isScriptReward(false)
            .isScriptCert(false)
            .isScriptVote(false)
            .isScriptPropose(false)
            .build();

    SmartContractInfo smartContractInfo =
        SmartContractInfo.builder()
            .scriptHash("e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6")
            .type(ScriptType.PLUTUSV1)
            .txCount(10L)
            .isScriptSpend(true)
            .isScriptMint(true)
            .build();

    when(smartContractInfoRepository.findAllByFilterRequest(
            filterRequest.getScriptVersion(),
            filterRequest.getIsScriptReward(),
            filterRequest.getIsScriptCert(),
            filterRequest.getIsScriptSpend(),
            filterRequest.getIsScriptMint(),
            filterRequest.getIsScriptVote(),
            filterRequest.getIsScriptPropose(),
            filterRequest.getIsScriptAny(),
            filterRequest.getIsScriptNone(),
            pageable))
        .thenReturn(new PageImpl<>(List.of(smartContractInfo)));

    var response = scriptService.getSmartContracts(filterRequest, pageable);
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6",
        response.getData().get(0).getScriptHash());
    Assertions.assertEquals(10L, response.getData().get(0).getTxCount());
    Assertions.assertTrue(response.getData().get(0).getTxPurposes().contains(TxPurposeType.SPEND));
    Assertions.assertTrue(response.getData().get(0).getTxPurposes().contains(TxPurposeType.MINT));
    Assertions.assertEquals(ScriptType.PLUTUSV1, response.getData().get(0).getScriptVersion());
  }

  @Test
  void testGetSmartContracts_withScriptVersionIsPlutusV3() {
    // Given
    Pageable pageable = PageRequest.of(0, 1);
    SmartContractFilterRequest filterRequest =
        SmartContractFilterRequest.builder()
            .txPurpose(Set.of(TxPurposeType.VOTE))
            .scriptVersion(ScriptType.PLUTUSV3)
            .isScriptMint(false)
            .isScriptSpend(false)
            .isScriptNone(false)
            .isScriptAny(false)
            .isScriptReward(false)
            .isScriptCert(false)
            .isScriptVote(true)
            .isScriptPropose(false)
            .build();

    SmartContractInfo smartContractInfo =
        SmartContractInfo.builder()
            .scriptHash("e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6")
            .type(ScriptType.PLUTUSV3)
            .txCount(10L)
            .isScriptVote(true)
            .build();

    when(smartContractInfoRepository.findAllByFilterRequest(
            filterRequest.getScriptVersion(),
            filterRequest.getIsScriptReward(),
            filterRequest.getIsScriptCert(),
            filterRequest.getIsScriptSpend(),
            filterRequest.getIsScriptMint(),
            filterRequest.getIsScriptVote(),
            filterRequest.getIsScriptPropose(),
            filterRequest.getIsScriptAny(),
            filterRequest.getIsScriptNone(),
            pageable))
        .thenReturn(new PageImpl<>(List.of(smartContractInfo)));

    var response = scriptService.getSmartContracts(filterRequest, pageable);
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6",
        response.getData().get(0).getScriptHash());
    Assertions.assertEquals(10L, response.getData().get(0).getTxCount());
    Assertions.assertFalse(response.getData().get(0).getTxPurposes().contains(TxPurposeType.SPEND));
    Assertions.assertFalse(response.getData().get(0).getTxPurposes().contains(TxPurposeType.MINT));
    Assertions.assertTrue(response.getData().get(0).getTxPurposes().contains(TxPurposeType.VOTE));
    Assertions.assertEquals(ScriptType.PLUTUSV3, response.getData().get(0).getScriptVersion());
  }

  @Test
  void getSmartContractDetail_shouldReturnSmartContractDetailResponse() {
    String scriptHash = "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6";
    when(scriptRepository.findByHash(scriptHash))
        .thenReturn(
            Optional.of(Script.builder().hash(scriptHash).type(ScriptType.PLUTUSV1).build()));
    when(addressRepository.getAssociatedAddress(scriptHash))
        .thenReturn(
            List.of(
                "addr1z9fvxytwm8dv0aht3x8cxetm3tu4f47kaqdgxney8mu6hjxy0sm24zwzyknar6v855calh3yvxzj8lu6q0ke9a3vejrsjhed7q"));
    when(stakeAddressRepository.getAssociatedAddress(scriptHash))
        .thenReturn(List.of("stake1u8sadwjcje35pu2kmfsytxttnhjyzr948nqd449q5geh83ggjxnfd"));

    var response = scriptService.getSmartContractDetail(scriptHash);
    Assertions.assertEquals(scriptHash, response.getScriptHash());
    Assertions.assertEquals(ScriptType.PLUTUSV1, response.getScriptType());
    Assertions.assertEquals(2, response.getAssociatedAddresses().size());
    Assertions.assertEquals(
        "stake1u8sadwjcje35pu2kmfsytxttnhjyzr948nqd449q5geh83ggjxnfd",
        response.getAssociatedAddresses().get(0));
    Assertions.assertEquals(
        "addr1z9fvxytwm8dv0aht3x8cxetm3tu4f47kaqdgxney8mu6hjxy0sm24zwzyknar6v855calh3yvxzj8lu6q0ke9a3vejrsjhed7q",
        response.getAssociatedAddresses().get(1));
  }

  @Test
  void getSmartContractDetail_shouldThrowExceptionWhenScriptHashNotFound() {
    String scriptHash = "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6";
    when(scriptRepository.findByHash(scriptHash)).thenReturn(Optional.empty());
    Assertions.assertThrows(
        BusinessException.class, () -> scriptService.getSmartContractDetail(scriptHash));
  }

  @Test
  void getSmartContractDetail_shouldThrowExceptionWhenScriptHashNotBelongToSC() {
    String scriptHash = "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6";
    when(scriptRepository.findByHash(scriptHash))
        .thenReturn(
            Optional.of(
                Script.builder()
                    .hash("e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6")
                    .type(ScriptType.TIMELOCK)
                    .build()));
    Assertions.assertThrows(
        BusinessException.class, () -> scriptService.getSmartContractDetail(scriptHash));
  }

  @Test
  void getSmartContractTxs_shouldReturnSmartContractTxResponse() {
    String scriptHash = "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6";
    Pageable pageable = PageRequest.of(0, 10);
    Page<Long> txIds = new PageImpl<>(List.of(1L), pageable, 1);
    SmartContractTxProjectionImpl smartContractTxProjection =
        SmartContractTxProjectionImpl.builder()
            .hash("hash1")
            .blockNo(1L)
            .epochNo(1)
            .epochSlotNo(1)
            .absoluteSlot(1)
            .txId(1L)
            .build();

    when(txRepository.getSmartContractTxsByTxIds(txIds.getContent()))
        .thenReturn(List.of(smartContractTxProjection));
    when(smartContractInfoRepository.getTxCountByScriptHash(scriptHash)).thenReturn(1L);
    when(redeemerRepository.findTxIdsInteractWithContract(scriptHash, pageable))
        .thenReturn(txIds.getContent());

    when(txRepository.getSmartContractTxsPurpose(txIds.getContent(), scriptHash))
        .thenReturn(
            List.of(
                SmartContractTxProjectionImpl.builder()
                    .txId(1L)
                    .scriptPurposeType(ScriptPurposeType.SPEND)
                    .build()));

    var response = scriptService.getSmartContractTxs(scriptHash, pageable);
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(response.getData().get(0).getTxId(), 1L);
    Assertions.assertEquals(response.getData().get(0).getHash(), "hash1");
    Assertions.assertEquals(
        response.getData().get(0).getScriptPurposeTypes(), List.of(ScriptPurposeType.SPEND));
  }

  @Test
  void searchScript_shouldThrowExceptionWhenScriptNotFound() {
    when(scriptRepository.findByHash("76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed"))
        .thenReturn(Optional.empty());
    Assertions.assertThrows(
        BusinessException.class,
        () ->
            scriptService.searchScript("76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed"));
  }

  @Test
  void searchScript_shouldReturnNativeScriptResponse() {
    when(scriptRepository.findByHash("76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed"))
        .thenReturn(
            Optional.of(
                Script.builder()
                    .hash("76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed")
                    .type(ScriptType.TIMELOCK)
                    .build()));

    var response =
        scriptService.searchScript("76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed");
    Assertions.assertEquals(
        "76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed", response.getScriptHash());
    Assertions.assertFalse(response.isSmartContract());
    Assertions.assertTrue(response.isNativeScript());
  }

  @Test
  void searchScript_shouldReturnSmartContractResponse() {
    when(scriptRepository.findByHash("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79"))
        .thenReturn(
            Optional.of(
                Script.builder()
                    .hash("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79")
                    .type(ScriptType.PLUTUSV1)
                    .build()));

    var response =
        scriptService.searchScript("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79");
    Assertions.assertEquals(
        "9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79", response.getScriptHash());
    Assertions.assertTrue(response.isSmartContract());
    Assertions.assertFalse(response.isNativeScript());
  }

  @Test
  void searchScript_shouldReturnSmartContractResponseWithTypePlutus3() {
    when(scriptRepository.findByHash("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79"))
        .thenReturn(
            Optional.of(
                Script.builder()
                    .hash("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79")
                    .type(ScriptType.PLUTUSV3)
                    .build()));

    var response =
        scriptService.searchScript("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79");
    Assertions.assertEquals(
        "9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79", response.getScriptHash());
    Assertions.assertTrue(response.isSmartContract());
    Assertions.assertFalse(response.isNativeScript());
  }

  @Test
  void getContractExecutions_shouldReturnContractExecutions() {
    String scriptHash = "9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79";
    String txHash = "3ea3cbbdb2db54df20031587896fbed1ae96d215060944b71b9508de9f3feb44";
    ContractResponse contractResponse1 =
        ContractResponse.builder()
            .scriptHash("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79")
            .executionInputs(List.of("input1", "input2"))
            .executionOutputs(List.of("output1", "output2"))
            .build();

    ContractResponse contractResponse2 =
        ContractResponse.builder()
            .scriptHash("scriptHash")
            .executionInputs(List.of("input3", "input4"))
            .executionOutputs(List.of("output3", "output4"))
            .build();

    TxResponse txResponse = new TxResponse();
    txResponse.setContracts(List.of(contractResponse1, contractResponse2));
    when(txService.getTxDetailByHash(txHash)).thenReturn(txResponse);
    var response = scriptService.getContractExecutions(txHash, scriptHash);
    Assertions.assertEquals(4, response.size());
    Assertions.assertTrue(response.contains("input1"));
    Assertions.assertTrue(response.contains("input2"));
    Assertions.assertTrue(response.contains("output1"));
    Assertions.assertTrue(response.contains("output2"));
  }

  @Test
  void testGetNativeScriptTokens_thenReturn() {
    String scriptHash = "9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79";
    NativeScriptInfo nativeScriptInfo =
        NativeScriptInfo.builder().scriptHash(scriptHash).numberOfTokens(2L).build();
    Pageable pageable = PageRequest.of(0, 2);
    TokenProjection projection1 = mock(TokenProjection.class);
    when(projection1.getPolicy()).thenReturn(scriptHash);
    TokenProjection projection2 = mock(TokenProjection.class);
    when(projection2.getPolicy()).thenReturn("scriptHash");

    when(nativeScriptInfoRepository.findByScriptHash(scriptHash))
        .thenReturn(Optional.of(nativeScriptInfo));
    when(multiAssetRepository.findTokenInfoByScriptHash(scriptHash, pageable))
        .thenReturn(List.of(projection1, projection2));

    var actual = scriptService.getNativeScriptTokens(scriptHash, pageable);

    Assertions.assertEquals(2, actual.getTotalItems());
    Assertions.assertTrue(actual.getData().get(0).getPolicy().equals(scriptHash));
    Assertions.assertTrue(actual.getData().get(1).getPolicy().equals("scriptHash"));
  }

  @Test
  void testGetNativeScriptTokens_whenNotFoundNativeScriptByScriptHash_thenReturn() {
    String scriptHash = "9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79";
    Pageable pageable = PageRequest.of(0, 2);
    TokenProjection projection1 = mock(TokenProjection.class);
    when(projection1.getPolicy()).thenReturn(scriptHash);

    when(nativeScriptInfoRepository.findByScriptHash(scriptHash)).thenReturn(Optional.empty());
    when(multiAssetRepository.countMultiAssetByPolicy(scriptHash)).thenReturn(Long.valueOf(1));
    when(multiAssetRepository.findTokenInfoByScriptHash(scriptHash, pageable))
        .thenReturn(List.of(projection1));

    var actual = scriptService.getNativeScriptTokens(scriptHash, pageable);

    Assertions.assertEquals(1, actual.getTotalItems());
    Assertions.assertTrue(actual.getData().get(0).getPolicy().equals(scriptHash));
  }

  @Test
  void testGetNativeScriptHolders_thenReturn() {
    String scriptHash = "9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79";
    Pageable pageable = PageRequest.of(0, 2);
    Long addressId = 1L;
    Address address = Address.builder().id(addressId).address("address").build();
    AddressTokenProjection projection = mock(AddressTokenProjection.class);
    when(projection.getPolicy()).thenReturn(scriptHash);
    when(projection.getAddressId()).thenReturn(addressId);
    when(projection.getAddress()).thenReturn("address");
    when(latestTokenBalanceRepository.findAddressAndBalanceByPolicy(scriptHash, pageable))
        .thenReturn(List.of(projection));
    when(latestTokenBalanceRepository.findAddressAndBalanceByPolicy(scriptHash, pageable))
        .thenReturn(List.of(projection));

    var actual = scriptService.getNativeScriptHolders(scriptHash, pageable);

    Assertions.assertTrue(actual.getData().get(0).getPolicy().equals(scriptHash));
    Assertions.assertEquals(1, actual.getTotalItems());
    Assertions.assertNotNull(actual.getData().get(0).getAddressId());
  }

  @Test
  void testVerifyNativeScript_thenReturn() throws BusinessException {
    String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
    String scriptJson =
        "{\n"
            + "    \"type\": \"all\",\n"
            + "    \"scripts\": [\n"
            + "        {\n"
            + "            \"type\": \"sig\",\n"
            + "            \"keyHash\": \"26bacc7b88e2b40701387c521cd0c50d5c0cfa4c6c6d7f0901395757\"\n"
            + "        },\n"
            + "        {\n"
            + "            \"type\": \"before\",\n"
            + "            \"slot\": 23069343\n"
            + "        }\n"
            + "    ]\n"
            + "}";
    Script script = Script.builder().hash(scriptHash).type(ScriptType.TIMELOCK).json(null).build();
    when(scriptRepository.findByHash(scriptHash)).thenReturn(Optional.of(script));
    when(verifiedScriptRepository.existsVerifiedScriptByHash(scriptHash)).thenReturn(false);
    when(verifiedScriptRepository.save(any(VerifiedScript.class)))
        .thenAnswer(i -> i.getArguments()[0]);
    String actual = scriptService.verifyNativeScript(scriptHash, scriptJson);

    Assertions.assertEquals(scriptJson, actual);
  }

  @Test
  void testVerifyNativeScript_shouldThrowExceptionWhenScriptHashNotFound() {
    String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
    String scriptJson = "json";
    when(scriptRepository.findByHash(scriptHash)).thenReturn(Optional.empty());
    Assertions.assertThrows(
        BusinessException.class, () -> scriptService.verifyNativeScript(scriptHash, scriptJson));
  }

  @Test
  void testVerifyNativeScript_shouldThrowExceptionWhenFoundScriptNotMatchType() {
    String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
    String scriptJson = "json";
    Script script = Script.builder().hash(scriptHash).type(ScriptType.PLUTUSV1).json(null).build();

    when(scriptRepository.findByHash(scriptHash)).thenReturn(Optional.of(script));

    Assertions.assertThrows(
        BusinessException.class, () -> scriptService.verifyNativeScript(scriptHash, scriptJson));
  }

  @Test
  void testVerifyNativeScript_shouldThrowExceptionWhenExistVerifiedScript() {
    String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
    String scriptJson = "json";
    Script script = Script.builder().hash(scriptHash).type(ScriptType.TIMELOCK).json(null).build();

    when(scriptRepository.findByHash(scriptHash)).thenReturn(Optional.of(script));
    when(verifiedScriptRepository.existsVerifiedScriptByHash(scriptHash)).thenReturn(true);

    Assertions.assertThrows(
        BusinessException.class, () -> scriptService.verifyNativeScript(scriptHash, scriptJson));
  }

  @Test
  void testGetNativeScriptDetail_shouldReturnNativeScriptResponse() {
    String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
    String scriptJson = getScripJson();
    Script script = Script.builder().hash(scriptHash).type(ScriptType.TIMELOCK).json(null).build();

    VerifiedScript verifiedScript =
        VerifiedScript.builder().hash(scriptHash).json(scriptJson).build();

    NativeScriptInfo nativeScriptInfo =
        NativeScriptInfo.builder()
            .scriptHash(scriptHash)
            .id(1L)
            .afterSlot(99L)
            .beforeSlot(101L)
            .numberOfTokens(2L)
            .numberOfAssetHolders(3L)
            .build();
    Block currentBlock = Block.builder().slotNo(100L).build();
    when(blockRepository.findLatestBlock()).thenReturn(Optional.of(currentBlock));
    when(scriptRepository.findByHash(scriptHash)).thenReturn(Optional.of(script));
    when(nativeScriptInfoRepository.findByScriptHash(scriptHash))
        .thenReturn(Optional.of(nativeScriptInfo));

    when(verifiedScriptRepository.findByHash(scriptHash)).thenReturn(Optional.of(verifiedScript));
    var actual = scriptService.getNativeScriptDetail(scriptHash);

    Assertions.assertEquals(nativeScriptInfo.getNumberOfTokens(), actual.getNumberOfTokens());
    Assertions.assertEquals(
        nativeScriptInfo.getNumberOfAssetHolders(), actual.getNumberOfAssetHolders());
    Assertions.assertTrue(actual.getVerifiedContract());
    Assertions.assertFalse(actual.getIsOneTimeMint());
    Assertions.assertEquals(
        actual.getConditionType(),
        com.bloxbean.cardano.client.transaction.spec.script.ScriptType.atLeast);
    Assertions.assertEquals(actual.getScriptHash(), scriptHash);
    Assertions.assertEquals(2, actual.getRequired());
    Assertions.assertTrue(actual.getIsOpen());
  }

  @Test
  void testGetNativeScriptDetail_shouldReturnNativeScriptResponse_withOneTimeMint() {
    String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
    String scriptJson =
        "{\"type\":\"all\",\"scripts\":[{\"type\":\"before\",\"slot\":48037363},{\"type\":\"sig\",\"keyHash\":\"a316ed93fca3970fb603a5d103a25780fc1a0d3c33878073226ca586\"}]}";
    Script script = Script.builder().hash(scriptHash).type(ScriptType.TIMELOCK).json(null).build();

    VerifiedScript verifiedScript =
        VerifiedScript.builder().hash(scriptHash).json(scriptJson).build();

    NativeScriptInfo nativeScriptInfo =
        NativeScriptInfo.builder()
            .scriptHash(scriptHash)
            .id(3L)
            .beforeSlot(23069343L)
            .numberOfTokens(1L)
            .numberOfAssetHolders(1L)
            .build();
    Block currentBlock = Block.builder().slotNo(100L).build();
    when(blockRepository.findLatestBlock()).thenReturn(Optional.of(currentBlock));
    when(scriptRepository.findByHash(scriptHash)).thenReturn(Optional.of(script));
    when(nativeScriptInfoRepository.findByScriptHash(scriptHash))
        .thenReturn(Optional.of(nativeScriptInfo));

    when(verifiedScriptRepository.findByHash(scriptHash)).thenReturn(Optional.of(verifiedScript));

    Slice<MultiAsset> multiAssetSlice =
        new SliceImpl<>(List.of(MultiAsset.builder().id(1L).build()));

    Pageable pageable = PageRequest.of(0, 100, Sort.by("id").ascending());
    when(multiAssetRepository.getSliceByPolicy(scriptHash, pageable)).thenReturn(multiAssetSlice);
    when(maTxMintRepository.findFirstTxMintByMultiAssetId(anyLong())).thenReturn(1L);
    when(maTxMintRepository.existsMoreOneMintTx(anyList(), anyLong())).thenReturn(Boolean.FALSE);

    var actual = scriptService.getNativeScriptDetail(scriptHash);

    Assertions.assertEquals(nativeScriptInfo.getNumberOfTokens(), actual.getNumberOfTokens());
    Assertions.assertEquals(
        nativeScriptInfo.getNumberOfAssetHolders(), actual.getNumberOfAssetHolders());
    Assertions.assertTrue(actual.getVerifiedContract());
    Assertions.assertEquals(
        actual.getConditionType(),
        com.bloxbean.cardano.client.transaction.spec.script.ScriptType.all);
    Assertions.assertEquals(actual.getScriptHash(), scriptHash);
    Assertions.assertTrue(actual.getIsOpen());
    Assertions.assertTrue(actual.getIsOneTimeMint());
  }

  @Test
  void testGetNativeScriptDetail_shouldThrowExceptionScriptHashNotFound() {
    String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
    String scriptJson = "json";
    when(scriptRepository.findByHash(scriptHash)).thenReturn(Optional.empty());
    Assertions.assertThrows(
        BusinessException.class, () -> scriptService.verifyNativeScript(scriptHash, scriptJson));
  }

  String getScripJson() {
    return "{\n"
        + "  \"type\": \"any\",\n"
        + "  \"scripts\": [\n"
        + "    {\n"
        + "      \"type\": \"all\",\n"
        + "      \"scripts\": [\n"
        + "        {\n"
        + "          \"type\": \"after\",\n"
        + "          \"slot\": 25732159\n"
        + "        },\n"
        + "        {\n"
        + "          \"type\": \"atLeast\",\n"
        + "          \"required\": 2,\n"
        + "          \"scripts\": [\n"
        + "            {\n"
        + "              \"type\": \"sig\",\n"
        + "              \"keyHash\": \"eaeb848139b224cd0c2eaba28ef17d788989d2ec4da3cda54281bbb8\"\n"
        + "            }\n"
        + "          ]\n"
        + "        }\n"
        + "      ]\n"
        + "    }\n"
        + "  ]\n"
        + "}";
  }
}
