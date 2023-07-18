package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.mapper.AddressMapper;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.request.ScriptVerifyRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractScript;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.api.repository.*;
import org.cardanofoundation.explorer.api.service.impl.AddressServiceImpl;
import org.cardanofoundation.explorer.api.util.HexUtils;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.ledgersync.common.common.address.ShelleyAddress;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigInteger;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ExecutionException;

import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AddressServiceTest {

  @Mock
  AddressRepository addressRepository;
  @Mock
  ScriptRepository scriptRepository;

  @Mock
  AddressMapper addressMapper;

  @Mock
  AddressTxBalanceRepository addressTxBalanceRepository;

  @Mock
  AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;

  @Mock
  AddressTokenBalanceRepository addressTokenBalanceRepository;

  @Mock
  MultiAssetRepository multiAssetRepository;

  @Mock
  TokenMapper tokenMapper;

  @Mock
  AssetMetadataRepository assetMetadataRepository;

  @Mock
  AssetMetadataMapper assetMetadataMapper;

  @InjectMocks
  AddressServiceImpl addressService;

  @BeforeEach
  void preSetup() {
    ReflectionTestUtils.setField(addressService, "network", "mainnet");
  }

  @Test
  void getAddressDetail_shouldReturn() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    String stakeAddr = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    Address address = Address.builder().build();
    AddressResponse addressResponse = AddressResponse.builder().build();
    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressMapper.fromAddress(address)).thenReturn(addressResponse);
    AddressResponse expect = AddressResponse.builder().stakeAddress(stakeAddr).build();

    AddressResponse response = addressService.getAddressDetail(addr);
    Assertions.assertEquals(expect.getStakeAddress(), response.getStakeAddress());
  }

  @Test
  void getAddressDetail_shouldReturnV2() {
    String addr = "addr_test1vqtl90v0ushnqnmmpty6xu6grxydncgnzvm7rr2z33ahwacfq3d9h";
    String stakeAddr = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    ReflectionTestUtils.setField(addressService, "network", "testnet");
    AddressResponse addressResponse = AddressResponse.builder().address(addr).stakeAddress(stakeAddr).build();
    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.empty());
    when(addressMapper.fromAddress(any())).thenReturn(addressResponse);
    AddressResponse expect = AddressResponse.builder().address(addr).build();

    AddressResponse response = addressService.getAddressDetail(addr);
    Assertions.assertEquals(expect.getAddress(), response.getAddress());
  }

  @Test
  void getAddressDetail_throwException() {
    String addr = "address_error";
    Assertions.assertThrows(BusinessException.class, () -> addressService.getAddressDetail(addr));
  }

  @Test
  void getAddressAnalytics_shouldReturn() throws ExecutionException, InterruptedException {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    AnalyticType type = AnalyticType.ONE_DAY;
    Address address = Address.builder().address(addr).build();

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTxBalanceRepository.countByAddress(address)).thenReturn(1L);
    when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.of(LocalDate.MIN));

    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertNotNull(response);

  }

  @Test
  void getAddressAnalytics_shouldReturnV2() throws ExecutionException, InterruptedException {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    AnalyticType type = AnalyticType.ONE_MONTH;
    Address address = Address.builder().address(addr).build();

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTxBalanceRepository.countByAddress(address)).thenReturn(1L);
    when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.of(LocalDate.now().minusDays(1)));

    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertNotNull(response);

  }

  @Test
  void getAddressAnalytics_shouldReturnV3() throws ExecutionException, InterruptedException {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    AnalyticType type = AnalyticType.ONE_MONTH;
    Address address = Address.builder().address(addr).build();

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTxBalanceRepository.countByAddress(address)).thenReturn(1L);
    when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.empty());

    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertNotNull(response);

  }

  @Test
  void getAddressAnalytics_shouldReturnV4() throws ExecutionException, InterruptedException {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    AnalyticType type = AnalyticType.ONE_DAY;
    Address address = Address.builder().address(addr).build();

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTxBalanceRepository.countByAddress(address)).thenReturn(1L);
    when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.empty());

    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertNotNull(response);

  }

  @Test
  void getAddressAnalytics_shouldReturnListNull() throws ExecutionException, InterruptedException {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    AnalyticType type = AnalyticType.ONE_DAY;
    Address address = Address.builder().address(addr).build();

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTxBalanceRepository.countByAddress(address)).thenReturn(0L);

    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertEquals(response, List.of());

  }

  @Test
  void getAddressMinMaxBalance_shouldReturn() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    MinMaxProjection projection = Mockito.mock(MinMaxProjection.class);
    when(projection.getMinVal()).thenReturn(BigInteger.ONE);
    when(projection.getMaxVal()).thenReturn(BigInteger.ONE);

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(Address.builder().id(1L).build()));
    when(addressTxBalanceRepository.findMinMaxBalanceByAddress(1L)).thenReturn(projection);

    var expect = List.of(BigInteger.ONE, BigInteger.ONE);
    var response = addressService.getAddressMinMaxBalance(addr);
    Assertions.assertEquals(expect, response);
  }

  @Test
  void getAddressMinMaxBalance_shouldReturnListNull() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(Address.builder().id(1L).build()));
    when(addressTxBalanceRepository.findMinMaxBalanceByAddress(1L)).thenReturn(null);

    var expect = Collections.emptyList();
    var response = addressService.getAddressMinMaxBalance(addr);
    Assertions.assertEquals(expect, response);
  }

  @Test
  void getContracts_shouldReturn() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    Pageable pageable = PageRequest.of(0, 10);
    Page<Address> contractPage = new PageImpl<>(List.of(Address.builder().id(1L).address(addr).build()));
    when(addressRepository.findAllByAddressHasScriptIsTrue(pageable)).thenReturn(contractPage);
    when(addressMapper.fromAddressToContractFilter(any())).thenReturn(ContractFilterResponse.builder().address(addr).build());
    var response = addressService.getContracts(pageable);
    var expect = List.of(ContractFilterResponse.builder().address(addr).build());
    Assertions.assertEquals(expect.size(), response.getData().size());
  }

  @Test
  void getTopAddress_shouldReturn() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    Pageable pageable = PageRequest.of(0, 10);
    AddressFilterResponse addressFilterResponse = AddressFilterResponse.builder().address(addr).build();
    when(addressRepository.findAllOrderByBalance(pageable)).thenReturn(List.of(Address.builder().address(addr).build()));
    when(addressMapper.fromAddressToFilterResponse(any())).thenReturn(addressFilterResponse);

    var response = addressService.getTopAddress(pageable);
    var expect = new BaseFilterResponse<>(new PageImpl<>(List.of(addressFilterResponse)));

    Assertions.assertEquals(expect.getData(), response.getData());
  }

  @Test
  void getTokenByAddress_shouldReturn() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    Pageable pageable = PageRequest.of(0, 10);
    Address address = Address.builder().address(addr).build();
    List<AddressTokenProjection> addressTokenProjections = new ArrayList<>();
    AddressTokenProjection projection = Mockito.mock(AddressTokenProjection.class);
    when(projection.getMultiAssetId()).thenReturn(1L);
    addressTokenProjections.add(projection);

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTokenBalanceRepository.findTokenAndBalanceByAddress(address, pageable)).thenReturn(new PageImpl<>(addressTokenProjections));
    when(multiAssetRepository.findAllById(List.of(1L))).thenReturn(List.of(MultiAsset.builder().id(1L).build()));
    when(tokenMapper.fromMultiAssetAndAddressToken(any(MultiAsset.class), any(AddressTokenProjection.class))).thenReturn(TokenAddressResponse.builder().addressId(1L).policy("sub").name("ject").build());
    when(assetMetadataRepository.findBySubjectIn(anySet())).thenReturn(List.of(AssetMetadata.builder().id(1L).subject("subject").build()));
    when(assetMetadataMapper.fromAssetMetadata(any())).thenReturn(TokenMetadataResponse.builder().build());

    var response = addressService.getTokenByAddress(pageable, addr);
    Assertions.assertNotNull(response);
  }

  @Test
  void getTokenByAddress_throwAddressNotFound() {
    String addr = "address_error";
    Pageable pageable = PageRequest.of(0, 10);

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.empty());

    Assertions.assertThrows(NoContentException.class, () -> addressService.getTokenByAddress(pageable, addr));
  }

  @Test
  void getTokenByDisplayName_shouldReturn() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    Pageable pageable = PageRequest.of(0, 10);
    String displayName = "fingerprint";
    Address address = Address.builder().address(addr).build();
    List<AddressTokenProjection> addressTokenProjections = new ArrayList<>();
    AddressTokenProjection projection = Mockito.mock(AddressTokenProjection.class);
    when(projection.getFingerprint()).thenReturn("fingerprint");
    when(projection.getTokenName()).thenReturn("token");
    addressTokenProjections.add(projection);
    TokenAddressResponse tokenAddressResponse = TokenAddressResponse.builder()
            .fingerprint("fingerprint")
            .addressId(1L)
            .address("address")
            .policy("policy")
            .fingerprint("fingerprint")
            .quantity(BigInteger.ONE)
            .displayName(HexUtils.fromHex("token", "fingerprint"))
            .build();

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTokenBalanceRepository.findTokenAndBalanceByAddress(address)).thenReturn(addressTokenProjections);
    when(tokenMapper.fromAddressTokenProjection(any())).thenReturn(tokenAddressResponse);

    var response = addressService.getTokenByDisplayName(pageable, addr, displayName);
    var expect = new BaseFilterResponse<>(List.of(tokenAddressResponse), 1);
    Assertions.assertEquals(expect.getData(), response.getData());
    Assertions.assertEquals(expect.getTotalItems(), response.getTotalItems());
  }

  @Test
  void getTokenByDisplayName_emptyDisplay() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    Pageable pageable = PageRequest.of(0, 10);
    Address address = Address.builder().address(addr).build();
    List<AddressTokenProjection> addressTokenProjections = new ArrayList<>();
    AddressTokenProjection projection = Mockito.mock(AddressTokenProjection.class);
    when(projection.getMultiAssetId()).thenReturn(1L);
    addressTokenProjections.add(projection);

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTokenBalanceRepository.findTokenAndBalanceByAddress(address, pageable)).thenReturn(new PageImpl<>(addressTokenProjections));
    when(multiAssetRepository.findAllById(List.of(1L))).thenReturn(List.of(MultiAsset.builder().id(1L).build()));
    when(tokenMapper.fromMultiAssetAndAddressToken(any(MultiAsset.class), any(AddressTokenProjection.class))).thenReturn(TokenAddressResponse.builder().addressId(1L).policy("sub").name("ject").build());
    when(assetMetadataRepository.findBySubjectIn(anySet())).thenReturn(List.of(AssetMetadata.builder().id(1L).subject("subject").build()));
    when(assetMetadataMapper.fromAssetMetadata(any())).thenReturn(TokenMetadataResponse.builder().build());

    var response = addressService.getTokenByDisplayName(pageable, addr, null);
    Assertions.assertNotNull(response);
  }

  @Test
  void getTokenByDisplayName_addressNotFound() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    Pageable pageable = PageRequest.of(0, 10);
    String displayName = "fingerprint";

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.empty());

    Assertions.assertThrows(NoContentException.class, () -> addressService.getTokenByDisplayName(pageable, addr, displayName));
  }

  @Test
  void verifyNativeScript_shouldReturnTrueIfPolicyIdMatches() {

    ScriptVerifyRequest scriptVerifyRequest = ScriptVerifyRequest.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .script("{\"type\":\"all\",\"scripts\":[{\"type\":\"sig\",\"keyHash\":\"e3124f98d11157535bc61ce8db0e04e95cfcf552a86bb116e593a76e\"},{\"type\":\"sig\",\"keyHash\":\"e52515f0f5e25adb0c57ac5835f67bf703e10e494be391d4b41bcfbd\"}]}")
        .build();

    Address address = Address.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .verifiedContract(false)
        .build();

    when(addressRepository.findFirstByAddress(scriptVerifyRequest.getAddress()))
        .thenReturn(Optional.of(address));
    when(addressRepository.save(any(Address.class))).thenReturn(new Address());
    Assertions.assertTrue(addressService.verifyNativeScript(scriptVerifyRequest));
  }

  @Test
  void verifyNativeScript_shouldReturnFalseIfPolicyIdDoesNotMatch() {

    ScriptVerifyRequest scriptVerifyRequest = ScriptVerifyRequest.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .script("{\"type\":\"all\",\"scripts\":[{\"type\":\"sig\",\"keyHash\":\"e3124f97d11157535bc61ce8db0e04e95cfcf552a86bb116e593a76e\"},{\"type\":\"sig\",\"keyHash\":\"e52515f0f5e25adb0c57ac5835f67bf703e10e494be391d4b41bcfbd\"}]}")
        .build();

    Assertions.assertFalse(addressService.verifyNativeScript(scriptVerifyRequest));
  }

  @Test
  void verifyNativeScript_shouldThrowException_whenAnyErrorsOccur() {

    ScriptVerifyRequest scriptVerifyRequest = ScriptVerifyRequest.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .script("Json parse error")
        .build();

    Assertions.assertThrows(BusinessException.class, () -> addressService.verifyNativeScript(scriptVerifyRequest));
  }

  @Test
  void getJsonNativeScript_shouldReturnUnverifiedContractScript_whenContractNotVerifyYet(){
    Address address = Address.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .verifiedContract(false)
        .build();

    when(addressRepository.findFirstByAddress(address.getAddress()))
        .thenReturn(Optional.of(address));

    ContractScript contractScript = addressService.getJsonNativeScript(address.getAddress());

    Assertions.assertFalse(contractScript.getIsVerified());
    Assertions.assertNull(contractScript.getData());
  }

  @Test
  void getJsonNativeScript_shouldReturnNativeJsonScript() {
    Address address = Address.builder()
        .address(
            "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .verifiedContract(true)
        .build();

    ShelleyAddress shelleyAddress = new ShelleyAddress(address.getAddress());
    String policyId = shelleyAddress.getHexPaymentPart();
    Script script = Script.builder()
        .json("native script")
        .build();

    when(addressRepository.findFirstByAddress(address.getAddress()))
        .thenReturn(Optional.of(address));
    when(scriptRepository.findByHash(policyId))
        .thenReturn(Optional.of(script));

    ContractScript contractScript = addressService.getJsonNativeScript(address.getAddress());

    Assertions.assertTrue(contractScript.getIsVerified());
    Assertions.assertEquals(script.getJson(), contractScript.getData());
  }

  @Test
  void getJsonNativeScript_shouldReturnUnverifiedContractScript_whenNativeScripJsonNull() {
    Address address = Address.builder()
        .address(
            "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .verifiedContract(true)
        .build();

    ShelleyAddress shelleyAddress = new ShelleyAddress(address.getAddress());
    String policyId = shelleyAddress.getHexPaymentPart();
    Script script = Script.builder()
        .build();

    when(addressRepository.findFirstByAddress(address.getAddress()))
        .thenReturn(Optional.of(address));
    when(scriptRepository.findByHash(policyId))
        .thenReturn(Optional.of(script));

    ContractScript contractScript = addressService.getJsonNativeScript(address.getAddress());

    Assertions.assertFalse(contractScript.getIsVerified());
    Assertions.assertNull(contractScript.getData());
  }

  @Test
  void getJsonNativeScript_throwAddressNotFound() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.empty());
    Assertions.assertThrows(BusinessException.class, () -> addressService.getJsonNativeScript(addr));
  }

  @Test
  void getJsonNativeScript_throwScriptNotFound() {
    String addr = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(Address.builder().verifiedContract(true).address(addr).build()));
    when(scriptRepository.findByHash(any())).thenReturn(Optional.empty());
    Assertions.assertThrows(BusinessException.class, () -> addressService.getJsonNativeScript(addr));
  }
}
