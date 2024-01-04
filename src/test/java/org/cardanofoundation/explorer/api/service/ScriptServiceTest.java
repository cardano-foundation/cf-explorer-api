package org.cardanofoundation.explorer.api.service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import org.cardanofoundation.explorer.api.common.enumeration.TxPurposeType;
import org.cardanofoundation.explorer.api.mapper.ScriptMapper;
import org.cardanofoundation.explorer.api.mapper.ScriptMapperImpl;
import org.cardanofoundation.explorer.api.model.request.script.smartcontract.SmartContractFilterRequest;
import org.cardanofoundation.explorer.api.model.response.tx.ContractResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxResponse;
import org.cardanofoundation.explorer.api.projection.PolicyProjection;
import org.cardanofoundation.explorer.api.repository.explorer.SmartContractInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RedeemerRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.service.impl.ScriptServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.SmartContractTxProjectionImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.SmartContractInfo;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
class ScriptServiceTest {

  @Mock
  ScriptRepository scriptRepository;
  @Mock
  StakeAddressRepository stakeAddressRepository;
  @Mock
  TxRepository txRepository;
  @Mock
  RedeemerRepository redeemerRepository;
  @Mock
  AddressRepository addressRepository;
  @Mock
  MultiAssetRepository multiAssetRepository;
  @Mock
  SmartContractInfoRepository smartContractInfoRepository;

  @Mock
  TxService txService;

  @InjectMocks
  ScriptServiceImpl scriptService;

  @BeforeEach
  void setup() {
    ScriptMapper scriptMapper = new ScriptMapperImpl();
    ReflectionTestUtils.setField(scriptService, "scriptMapper", scriptMapper);
  }

  @Test
  void testGetNativeScripts() {
    // Given
    Pageable pageable = PageRequest.of(0, 1);
    List<ScriptType> nativeScriptTypes = List.of(ScriptType.TIMELOCK, ScriptType.MULTISIG);
    List<Script> scriptList = List.of(Script.builder().hash("hash").type(ScriptType.TIMELOCK).id(1L).build());
    PolicyProjection projection = Mockito.mock(PolicyProjection.class);
    // When and Then
    when(projection.getPolicy()).thenReturn("hash");
    when(projection.getNumberOfTokens()).thenReturn(2);
    when(projection.getNumberOfAssetHolders()).thenReturn(3);
    when(scriptRepository.findAllByTypeIn(nativeScriptTypes, pageable))
        .thenReturn(new PageImpl<>(scriptList));
    when(multiAssetRepository.countMultiAssetByPolicyIn(List.of("hash")))
        .thenReturn(List.of(projection));
    when(multiAssetRepository.countAssetHoldersByPolicyIn(List.of("hash")))
        .thenReturn(List.of(projection));
    var response = scriptService.getNativeScripts(pageable);
    // Assert
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("hash", response.getData().get(0).getScriptHash());
    Assertions.assertEquals(2, response.getData().get(0).getNumberOfTokens());
    Assertions.assertEquals(3, response.getData().get(0).getNumberOfAssetHolders());
  }

  @Test
  void testGetSmartContracts() {
    // Given
    Pageable pageable = PageRequest.of(0, 1);
    SmartContractFilterRequest filterRequest = SmartContractFilterRequest.builder()
        .txPurpose(Set.of(TxPurposeType.SPEND, TxPurposeType.MINT))
        .scriptVersion(ScriptType.PLUTUSV1)
        .isScriptMint(true)
        .isScriptSpend(true)
        .isScriptNone(false)
        .isScriptAny(false)
        .isScriptReward(false)
        .isScriptCert(false)
        .build();

    SmartContractInfo smartContractInfo = SmartContractInfo.builder()
        .scriptHash("e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6")
        .type(ScriptType.PLUTUSV1)
        .txCount(10L)
        .isScriptSpend(true)
        .isScriptMint(true)
        .build();

    when(smartContractInfoRepository.findAllByFilterRequest(filterRequest.getScriptVersion(),
                                                            filterRequest.getIsScriptReward(),
                                                            filterRequest.getIsScriptCert(),
                                                            filterRequest.getIsScriptSpend(),
                                                            filterRequest.getIsScriptMint(),
                                                            filterRequest.getIsScriptAny(),
                                                            filterRequest.getIsScriptNone(),
                                                            pageable))
        .thenReturn(new PageImpl<>(List.of(smartContractInfo)));

    var response = scriptService.getSmartContracts(filterRequest, pageable);
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6",
                            response.getData().get(0).getScriptHash());
    Assertions.assertEquals(10L, response.getData().get(0).getTxCount());
    Assertions.assertTrue(
        response.getData().get(0).getTxPurposes().contains(TxPurposeType.SPEND));
    Assertions.assertTrue(
        response.getData().get(0).getTxPurposes().contains(TxPurposeType.MINT));
    Assertions.assertEquals(ScriptType.PLUTUSV1, response.getData().get(0).getScriptVersion());

  }

  @Test
  void getSmartContractDetail_shouldReturnSmartContractDetailResponse() {
    String scriptHash = "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6";
    when(scriptRepository.findByHash(scriptHash))
        .thenReturn(
            Optional.of(Script.builder().hash(scriptHash).type(ScriptType.PLUTUSV1).build()));
    when(addressRepository.getAssociatedAddress(scriptHash))
        .thenReturn(List.of(
            "addr1z9fvxytwm8dv0aht3x8cxetm3tu4f47kaqdgxney8mu6hjxy0sm24zwzyknar6v855calh3yvxzj8lu6q0ke9a3vejrsjhed7q"));
    when(stakeAddressRepository.getAssociatedAddress(scriptHash))
        .thenReturn(List.of("stake1u8sadwjcje35pu2kmfsytxttnhjyzr948nqd449q5geh83ggjxnfd"));

    var response = scriptService.getSmartContractDetail(scriptHash);
    Assertions.assertEquals(scriptHash, response.getScriptHash());
    Assertions.assertEquals(ScriptType.PLUTUSV1, response.getScriptType());
    Assertions.assertEquals(2, response.getAssociatedAddresses().size());
    Assertions.assertEquals("stake1u8sadwjcje35pu2kmfsytxttnhjyzr948nqd449q5geh83ggjxnfd",
                            response.getAssociatedAddresses().get(0));
    Assertions.assertEquals(
        "addr1z9fvxytwm8dv0aht3x8cxetm3tu4f47kaqdgxney8mu6hjxy0sm24zwzyknar6v855calh3yvxzj8lu6q0ke9a3vejrsjhed7q",
        response.getAssociatedAddresses().get(1));
  }

  @Test
  void getSmartContractDetail_shouldThrowExceptionWhenScriptHashNotFound() {
    String scriptHash = "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6";
    when(scriptRepository.findByHash(scriptHash)).thenReturn(Optional.empty());
    Assertions.assertThrows(BusinessException.class,
                            () -> scriptService.getSmartContractDetail(scriptHash));
  }

  @Test
  void getSmartContractDetail_shouldThrowExceptionWhenScriptHashNotBelongToSC() {
    String scriptHash = "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6";
    when(scriptRepository.findByHash(scriptHash))
        .thenReturn(Optional.of(Script.builder()
                                    .hash("e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6")
                                    .type(ScriptType.TIMELOCK)
                                    .build()));
    Assertions.assertThrows(BusinessException.class,
                            () -> scriptService.getSmartContractDetail(scriptHash));
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
    when(smartContractInfoRepository.getTxCountByScriptHash(scriptHash))
        .thenReturn(1L);
    when(redeemerRepository.findTxIdsInteractWithContract(scriptHash, pageable))
        .thenReturn(txIds.getContent());

    when(txRepository.getSmartContractTxsPurpose(txIds.getContent(), scriptHash))
        .thenReturn(List.of(SmartContractTxProjectionImpl.builder()
                                .txId(1L)
                                .scriptPurposeType(ScriptPurposeType.SPEND)
                                .build()));

    var response = scriptService.getSmartContractTxs(scriptHash, pageable);
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(response.getData().get(0).getTxId(), 1L);
    Assertions.assertEquals(response.getData().get(0).getHash(), "hash1");
    Assertions.assertEquals(response.getData().get(0).getScriptPurposeTypes(),
                            List.of(ScriptPurposeType.SPEND));
  }

  @Test
  void searchScript_shouldThrowExceptionWhenScriptNotFound() {
    when(scriptRepository.findByHash("76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed")).thenReturn(Optional.empty());
    Assertions.assertThrows(BusinessException.class,
                            () -> scriptService.searchScript("76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed"));
  }

  @Test
  void searchScript_shouldReturnNativeScriptResponse() {
    when(scriptRepository.findByHash("76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed"))
        .thenReturn(Optional.of(Script.builder()
                                    .hash(
                                        "76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed")
                                    .type(ScriptType.TIMELOCK)
                                    .build()));

    var response = scriptService.searchScript(
        "76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed");
    Assertions.assertEquals("76b329bc5a22b26a6a06bf9991edef04ef182bad35ee3a482da0dbed",
                            response.getScriptHash());
    Assertions.assertFalse(response.isSmartContract());
    Assertions.assertTrue(response.isNativeScript());
  }

  @Test
  void searchScript_shouldReturnSmartContractResponse() {
    when(scriptRepository.findByHash("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79"))
        .thenReturn(Optional.of(Script.builder()
                                    .hash(
                                        "9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79")
                                    .type(ScriptType.PLUTUSV1)
                                    .build()));

    var response = scriptService.searchScript(
        "9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79");
    Assertions.assertEquals("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79",
                            response.getScriptHash());
    Assertions.assertTrue(response.isSmartContract());
    Assertions.assertFalse(response.isNativeScript());
  }

  @Test
  void getContractExecutions_shouldReturnContractExecutions() {
    String scriptHash = "9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79";
    String txHash = "3ea3cbbdb2db54df20031587896fbed1ae96d215060944b71b9508de9f3feb44";
    ContractResponse contractResponse1 = ContractResponse
        .builder()
        .scriptHash("9fb550d631a4ca55d48756923652418be96641773bc7c6097defab79")
        .executionInputs(List.of("input1", "input2"))
        .executionOutputs(List.of("output1", "output2"))
        .build();

    ContractResponse contractResponse2 = ContractResponse
        .builder()
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
}
