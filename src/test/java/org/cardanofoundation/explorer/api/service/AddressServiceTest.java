package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.mapper.AddressMapper;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AggregateAddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.service.impl.AddressServiceImpl;
import org.cardanofoundation.explorer.common.entity.ledgersync.Address;

@ExtendWith(MockitoExtension.class)
class AddressServiceTest {

  @Mock AddressRepository addressRepository;
  @Mock ScriptRepository scriptRepository;

  @Mock AddressMapper addressMapper;

  //  @Mock AddressTxBalanceRepository addressTxBalanceRepository;

  @Mock AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;

  //  @Mock AddressTokenBalanceRepository addressTokenBalanceRepository;

  @Mock TokenMapper tokenMapper;

  @Mock AssetMetadataRepository assetMetadataRepository;

  @Mock AssetMetadataMapper assetMetadataMapper;

  @InjectMocks AddressServiceImpl addressService;

  @BeforeEach
  void preSetup() {
    ReflectionTestUtils.setField(addressService, "network", "mainnet");
  }

  //  @Test
  //  void getAddressDetail_shouldReturn() {
  //    String addr =
  //
  // "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
  //    String stakeAddr = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
  //    Address address = Address.builder().build();
  //    AddressResponse addressResponse = AddressResponse.builder().build();
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //    when(addressMapper.fromAddress(address)).thenReturn(addressResponse);
  //    AddressResponse expect = AddressResponse.builder().stakeAddress(stakeAddr).build();
  //
  //    AddressResponse response = addressService.getAddressDetail(addr);
  //    Assertions.assertEquals(expect.getStakeAddress(), response.getStakeAddress());
  //  }
  //
  //  @Test
  //  void getAddressDetail_shouldAssociatedWithNativeScript() {
  //    String addr = "addr1w9rerwzk0f5v4den9u2c7anv2d4dl88hq9cq0xgcmernsfsak7w6r";
  //    Address address = Address.builder().build();
  //    AddressResponse addressResponse = AddressResponse.builder().build();
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //    when(addressMapper.fromAddress(address)).thenReturn(addressResponse);
  //
  // when(scriptRepository.findByHash("4791b8567a68cab7332f158f766c536adf9cf70170079918de473826"))
  //        .thenReturn(Optional.of(Script.builder().type(ScriptType.TIMELOCK).build()));
  //    AddressResponse response = addressService.getAddressDetail(addr);
  //    Assertions.assertTrue(response.isAssociatedNativeScript());
  //    Assertions.assertEquals(
  //        response.getScriptHash(), "4791b8567a68cab7332f158f766c536adf9cf70170079918de473826");
  //  }
  //
  //  @Test
  //  void getAddressDetail_shouldAssociatedWithSmartContract() {
  //    String addr =
  //
  // "addr1zyqzqk0uwkcr32zmxwaylhava4976yj8lwkjf3fcm4zjgkhqy02w7ayk0lyyrf080l5zusdpkg8se9x7fcke6vaz42lq6spmug";
  //    Address address = Address.builder().build();
  //    AddressResponse addressResponse = AddressResponse.builder().build();
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //    when(addressMapper.fromAddress(address)).thenReturn(addressResponse);
  //
  // when(scriptRepository.findByHash("002059fc75b038a85b33ba4fdfaced4bed1247fbad24c538dd45245a"))
  //        .thenReturn(Optional.of(Script.builder().type(ScriptType.PLUTUSV1).build()));
  //    AddressResponse response = addressService.getAddressDetail(addr);
  //    Assertions.assertTrue(response.isAssociatedSmartContract());
  //    Assertions.assertEquals(
  //        response.getScriptHash(), "002059fc75b038a85b33ba4fdfaced4bed1247fbad24c538dd45245a");
  //  }
  //
  //  @Test
  //  void getAddressDetail_shouldReturnV2() {
  //    String addr = "addr_test1vqtl90v0ushnqnmmpty6xu6grxydncgnzvm7rr2z33ahwacfq3d9h";
  //    String stakeAddr = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
  //    ReflectionTestUtils.setField(addressService, "network", "testnet");
  //    AddressResponse addressResponse =
  //        AddressResponse.builder().address(addr).stakeAddress(stakeAddr).build();
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.empty());
  //    when(addressMapper.fromAddress(any())).thenReturn(addressResponse);
  //    AddressResponse expect = AddressResponse.builder().address(addr).build();
  //
  //    AddressResponse response = addressService.getAddressDetail(addr);
  //    Assertions.assertEquals(expect.getAddress(), response.getAddress());
  //  }
  //
  //  @Test
  //  void getAddressDetail_throwException() {
  //    String addr = "addr_test_error";
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.empty());
  //    Assertions.assertThrows(BusinessException.class, () ->
  // addressService.getAddressDetail(addr));
  //  }
  //
  //  @Test
  //  void getAddressAnalytics_shouldReturn() {
  //    String addr =
  //
  // "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
  //    AnalyticType type = AnalyticType.ONE_DAY;
  //    Address address = Address.builder().address(addr).txCount(1L).build();
  //    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
  //    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.ZERO);
  //    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ZERO);
  //
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //    when(addressTxBalanceRepository.findMinMaxBalanceByAddress(any(), any(), any(), any()))
  //        .thenReturn(minMaxProjection);
  //
  //    var response = addressService.getAddressAnalytics(addr, type);
  //    Assertions.assertNotNull(response);
  //  }
  //
  //  @Test
  //  void getAddressAnalytics_shouldReturnOneMonth() {
  //    String addr =
  //
  // "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
  //    AnalyticType type = AnalyticType.ONE_MONTH;
  //    Address address = Address.builder().address(addr).txCount(1L).build();
  //    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
  //    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.ZERO);
  //    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ZERO);
  //
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //    when(addressTxBalanceRepository.findMinMaxBalanceByAddress(any(), any(), any(), any()))
  //        .thenReturn(minMaxProjection);
  //    var response = addressService.getAddressAnalytics(addr, type);
  //    Assertions.assertNotNull(response);
  //  }
  //
  //  @Test
  //  void getAddressAnalytics_shouldReturnListNull() {
  //    String addr =
  //
  // "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
  //    AnalyticType type = AnalyticType.ONE_DAY;
  //    Address address = Address.builder().address(addr).txCount(0L).build();
  //
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //
  //    var response = addressService.getAddressAnalytics(addr, type);
  //    Assertions.assertEquals(response.getData(), List.of());
  //    Assertions.assertEquals(response.getHighestBalance(), BigInteger.ZERO);
  //    Assertions.assertEquals(response.getLowestBalance(), BigInteger.ZERO);
  //  }
  //
  //  @Test
  //  void getAddressAnalytics_shouldReturnOneWeek() {
  //    String addr =
  //
  // "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
  //    AnalyticType type = AnalyticType.ONE_WEEK;
  //    Address address = Address.builder().address(addr).txCount(1L).build();
  //    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
  //    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.ZERO);
  //    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ZERO);
  //
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //    when(addressTxBalanceRepository.findMinMaxBalanceByAddress(any(), any(), any(), any()))
  //        .thenReturn(minMaxProjection);
  //
  //    var response = addressService.getAddressAnalytics(addr, type);
  //    Assertions.assertNotNull(response);
  //  }
  //
  //  @Test
  //  void getAddressAnalytics_shouldReturnThreeMonth() {
  //    String addr =
  //
  // "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
  //    AnalyticType type = AnalyticType.THREE_MONTH;
  //    Address address = Address.builder().address(addr).txCount(1L).build();
  //    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
  //    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.ZERO);
  //    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ZERO);
  //
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //    when(addressTxBalanceRepository.findMinMaxBalanceByAddress(any(), any(), any(), any()))
  //        .thenReturn(minMaxProjection);
  //
  //    var response = addressService.getAddressAnalytics(addr, type);
  //    Assertions.assertNotNull(response);
  //  }

  @Test
  void getContracts_shouldReturn() {
    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    Pageable pageable = PageRequest.of(0, 10);
    Page<Address> contractPage =
        new PageImpl<>(List.of(Address.builder().id(1L).address(addr).build()));
    when(addressRepository.findAllByAddressHasScriptIsTrue(pageable)).thenReturn(contractPage);
    when(addressMapper.fromAddressToContractFilter(any()))
        .thenReturn(ContractFilterResponse.builder().address(addr).build());
    var response = addressService.getContracts(pageable);
    var expect = List.of(ContractFilterResponse.builder().address(addr).build());
    Assertions.assertEquals(expect.size(), response.getData().size());
  }

  @Test
  void getTopAddress_shouldReturn() {
    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    Pageable pageable = PageRequest.of(0, 10);
    AddressFilterResponse addressFilterResponse =
        AddressFilterResponse.builder().address(addr).build();
    when(addressRepository.findAllOrderByBalance(pageable))
        .thenReturn(List.of(Address.builder().address(addr).build()));
    when(addressMapper.fromAddressToFilterResponse(any())).thenReturn(addressFilterResponse);

    var response = addressService.getTopAddress(pageable);
    var expect = new BaseFilterResponse<>(new PageImpl<>(List.of(addressFilterResponse)));

    Assertions.assertEquals(expect.getData(), response.getData());
  }

  //  @Test
  //  void getTokenByDisplayName_shouldReturn() {
  //    String addr =
  //
  // "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
  //    Pageable pageable = PageRequest.of(0, 10);
  //    String displayName = "fingerprint";
  //    Address address = Address.builder().address(addr).build();
  //    List<AddressTokenProjection> addressTokenProjections = new ArrayList<>();
  //    AddressTokenProjection projection = Mockito.mock(AddressTokenProjection.class);
  //    addressTokenProjections.add(projection);
  //    TokenAddressResponse tokenAddressResponse =
  //        TokenAddressResponse.builder()
  //            .fingerprint("fingerprint")
  //            .addressId(1L)
  //            .address("address")
  //            .policy("policy")
  //            .fingerprint("fingerprint")
  //            .quantity(BigInteger.ONE)
  //            .displayName(HexUtils.fromHex("token", "fingerprint"))
  //            .build();
  //
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //    when(addressTokenBalanceRepository.findTokenAndBalanceByAddressAndNameView(any(), any(),
  // any()))
  //        .thenReturn(new PageImpl<>(addressTokenProjections));
  //    when(tokenMapper.fromAddressTokenProjection(any())).thenReturn(tokenAddressResponse);
  //
  //    var response = addressService.getTokenByDisplayName(pageable, addr, displayName);
  //    var expect = new BaseFilterResponse<>(List.of(tokenAddressResponse), 1);
  //    Assertions.assertEquals(expect.getData(), response.getData());
  //    Assertions.assertEquals(expect.getTotalItems(), response.getTotalItems());
  //  }

  //  @Test
  //  void getTokenByDisplayName_emptyDisplay() {
  //    String addr =
  //
  // "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
  //    Pageable pageable = PageRequest.of(0, 10);
  //    Address address = Address.builder().address(addr).build();
  //    List<AddressTokenProjection> addressTokenProjections = new ArrayList<>();
  //    AddressTokenProjection projection = Mockito.mock(AddressTokenProjection.class);
  //    addressTokenProjections.add(projection);
  //
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
  //    when(addressTokenBalanceRepository.findTokenAndBalanceByAddress(address, pageable))
  //        .thenReturn(new PageImpl<>(addressTokenProjections));
  //    when(tokenMapper.fromAddressTokenProjection(any(AddressTokenProjection.class)))
  //        .thenReturn(
  //            TokenAddressResponse.builder().addressId(1L).policy("sub").name("ject").build());
  //
  //    var response = addressService.getTokenByDisplayName(pageable, addr, "");
  //    Assertions.assertNotNull(response);
  //  }

  //  @Test
  //  void getTokenByDisplayName_addressNotFound() {
  //    String addr =
  //
  // "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
  //    Pageable pageable = PageRequest.of(0, 10);
  //    String displayName = "fingerprint";
  //
  //    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.empty());
  //
  //    Assertions.assertThrows(
  //        NoContentException.class,
  //        () -> addressService.getTokenByDisplayName(pageable, addr, displayName));
  //  }
}
