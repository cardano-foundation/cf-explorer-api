package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.bloxbean.cardano.client.util.HexUtil;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.conversions.CardanoConverters;
import org.cardanofoundation.conversions.ClasspathConversionsFactory;
import org.cardanofoundation.conversions.domain.NetworkType;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapperImpl;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressTxAmountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressTxCountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AggregateAddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.LatestTokenBalanceRepository;
import org.cardanofoundation.explorer.api.service.impl.AddressServiceImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.ledgersync.Script;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.Address;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AddressTxCount;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.LatestTokenBalance;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@ExtendWith(MockitoExtension.class)
class AddressServiceTest {

  @Mock AddressRepository addressRepository;
  @Mock ScriptRepository scriptRepository;
  @Mock LatestTokenBalanceRepository latestTokenBalanceRepository;
  @Mock MultiAssetRepository multiAssetRepository;
  @Mock AddressTxCountRepository addressTxCountRepository;
  @Mock AddressTxAmountRepository addressTxAmountRepository;
  @Mock AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;
  @InjectMocks AddressServiceImpl addressService;

  @BeforeEach
  void preSetup() {
    ReflectionTestUtils.setField(addressService, "network", "mainnet");
  }

  @Test
  void getAddressDetail_shouldReturn() {
    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    String stakeAddr = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";

    when(addressRepository.getAddressDetail(addr))
        .thenReturn(AddressResponse.builder().address(addr).stakeAddress(stakeAddr).build());

    AddressResponse response = addressService.getAddressDetail(addr);
    Assertions.assertEquals(stakeAddr, response.getStakeAddress());
  }

  @Test
  void getAddressDetail_shouldAssociatedWithNativeScript() {
    String addr = "addr1w9rerwzk0f5v4den9u2c7anv2d4dl88hq9cq0xgcmernsfsak7w6r";
    when(addressRepository.getAddressDetail(addr))
        .thenReturn(AddressResponse.builder().address(addr).build());

    when(scriptRepository.findByHash("4791b8567a68cab7332f158f766c536adf9cf70170079918de473826"))
        .thenReturn(Optional.of(Script.builder().type(ScriptType.TIMELOCK).build()));
    AddressResponse response = addressService.getAddressDetail(addr);
    Assertions.assertTrue(response.isAssociatedNativeScript());
    Assertions.assertEquals(
        response.getScriptHash(), "4791b8567a68cab7332f158f766c536adf9cf70170079918de473826");
  }

  @Test
  void getAddressDetail_shouldAssociatedWithSmartContract() {
    String addr =
        "addr1zyqzqk0uwkcr32zmxwaylhava4976yj8lwkjf3fcm4zjgkhqy02w7ayk0lyyrf080l5zusdpkg8se9x7fcke6vaz42lq6spmug";
    when(addressRepository.getAddressDetail(addr))
        .thenReturn(AddressResponse.builder().address(addr).build());

    when(scriptRepository.findByHash("002059fc75b038a85b33ba4fdfaced4bed1247fbad24c538dd45245a"))
        .thenReturn(Optional.of(Script.builder().type(ScriptType.PLUTUSV1).build()));
    AddressResponse response = addressService.getAddressDetail(addr);
    Assertions.assertTrue(response.isAssociatedSmartContract());
    Assertions.assertEquals(
        response.getScriptHash(), "002059fc75b038a85b33ba4fdfaced4bed1247fbad24c538dd45245a");
  }

  @Test
  void getAddressDetail_shouldReturnV2() {
    String addr = "addr_test1vqtl90v0ushnqnmmpty6xu6grxydncgnzvm7rr2z33ahwacfq3d9h";
    ReflectionTestUtils.setField(addressService, "network", "testnet");

    when(addressRepository.getAddressDetail(addr)).thenReturn(null);
    AddressResponse expect = AddressResponse.builder().address(addr).build();

    AddressResponse response = addressService.getAddressDetail(addr);
    Assertions.assertEquals(expect.getAddress(), response.getAddress());
  }

  @Test
  void getAddressDetail_throwException() {
    String addr = "addr_test_error";
    Assertions.assertThrows(BusinessException.class, () -> addressService.getAddressDetail(addr));
  }

  @Test
  void getAddressAnalytics_shouldReturn() {
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(addressService, "cardanoConverters", cardanoConverters);

    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    AnalyticType type = AnalyticType.ONE_DAY;
    Address address = Address.builder().address(addr).build();
    AddressTxCount addressTxCount = AddressTxCount.builder().address(addr).txCount(1L).build();
    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.ZERO);
    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ZERO);

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTxCountRepository.findById(addr)).thenReturn(Optional.of(addressTxCount));
    when(addressTxAmountRepository.findMinMaxBalanceByAddress(any(), any(), any(), any()))
        .thenReturn(minMaxProjection);

    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertNotNull(response);
  }

  @Test
  void getAddressAnalytics_shouldReturnOneMonth() {
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(addressService, "cardanoConverters", cardanoConverters);

    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    AnalyticType type = AnalyticType.ONE_MONTH;
    Address address = Address.builder().address(addr).build();
    AddressTxCount addressTxCount = AddressTxCount.builder().address(addr).txCount(1L).build();

    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.ZERO);
    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ZERO);

    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTxCountRepository.findById(addr)).thenReturn(Optional.of(addressTxCount));
    when(addressTxAmountRepository.findMinMaxBalanceByAddress(any(), any(), any(), any()))
        .thenReturn(minMaxProjection);
    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertNotNull(response);
  }

  @Test
  void getAddressAnalytics_shouldReturnListNull() {
    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    AnalyticType type = AnalyticType.ONE_DAY;
    Address address = Address.builder().address(addr).build();
    AddressTxCount addressTxCount = AddressTxCount.builder().address(addr).txCount(0L).build();
    when(addressTxCountRepository.findById(addr)).thenReturn(Optional.of(addressTxCount));
    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));

    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertEquals(response.getData(), List.of());
    Assertions.assertEquals(response.getHighestBalance(), BigInteger.ZERO);
    Assertions.assertEquals(response.getLowestBalance(), BigInteger.ZERO);
  }

  @Test
  void getAddressAnalytics_shouldReturnOneWeek() {
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(addressService, "cardanoConverters", cardanoConverters);

    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    AnalyticType type = AnalyticType.ONE_WEEK;
    Address address = Address.builder().address(addr).build();

    AddressTxCount addressTxCount = AddressTxCount.builder().address(addr).txCount(1L).build();
    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.ZERO);
    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ZERO);

    when(addressTxCountRepository.findById(addr)).thenReturn(Optional.of(addressTxCount));
    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTxAmountRepository.findMinMaxBalanceByAddress(any(), any(), any(), any()))
        .thenReturn(minMaxProjection);

    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertNotNull(response);
  }

  @Test
  void getAddressAnalytics_shouldReturnThreeMonth() {
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(addressService, "cardanoConverters", cardanoConverters);

    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";

    AddressTxCount addressTxCount = AddressTxCount.builder().address(addr).txCount(1L).build();
    AnalyticType type = AnalyticType.THREE_MONTH;
    Address address = Address.builder().address(addr).build();
    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.ZERO);
    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ZERO);

    when(addressTxCountRepository.findById(addr)).thenReturn(Optional.of(addressTxCount));
    when(addressRepository.findFirstByAddress(addr)).thenReturn(Optional.of(address));
    when(addressTxAmountRepository.findMinMaxBalanceByAddress(any(), any(), any(), any()))
        .thenReturn(minMaxProjection);

    var response = addressService.getAddressAnalytics(addr, type);
    Assertions.assertNotNull(response);
  }

  @Test
  void getTokenByDisplayName_withNotEmptyDisplayName() {
    TokenMapper customTokenMapper = new TokenMapperImpl();
    ReflectionTestUtils.setField(addressService, "tokenMapper", customTokenMapper);

    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    String displayName = "test";
    Pageable pageable = PageRequest.of(0, 10);

    LatestTokenBalance latestTokenBalance =
        LatestTokenBalance.builder().unit("unit1").address(addr).quantity(BigInteger.ONE).build();

    AddressTokenProjection addressTokenProjection = Mockito.mock(AddressTokenProjection.class);
    when(addressTokenProjection.getUnit()).thenReturn("unit1");
    when(addressTokenProjection.getTokenName())
        .thenReturn(HexUtil.encodeHexString("test1".getBytes()));

    when(multiAssetRepository.findTokenMetadataByUnitIn(any()))
        .thenReturn(List.of(addressTokenProjection));
    when(latestTokenBalanceRepository.findAllByAddress(addr))
        .thenReturn(List.of(latestTokenBalance));

    var response = addressService.getTokenByDisplayName(pageable, addr, displayName);

    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(BigInteger.ONE, response.getData().get(0).getQuantity());
    Assertions.assertTrue(response.getData().get(0).getDisplayName().contains(displayName));
    Assertions.assertEquals(addr, response.getData().get(0).getAddress());
  }

  @Test
  void getTokenByDisplayName_withEmptyDisplayName() {
    TokenMapper customTokenMapper = new TokenMapperImpl();
    ReflectionTestUtils.setField(addressService, "tokenMapper", customTokenMapper);

    String addr =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    Pageable pageable = PageRequest.of(0, 10);

    LatestTokenBalance latestTokenBalance =
        LatestTokenBalance.builder().unit("unit1").address(addr).quantity(BigInteger.ONE).build();

    AddressTokenProjection addressTokenProjection = Mockito.mock(AddressTokenProjection.class);
    when(addressTokenProjection.getUnit()).thenReturn("unit1");

    when(multiAssetRepository.findTokenMetadataByUnitIn(any()))
        .thenReturn(List.of(addressTokenProjection));

    when(latestTokenBalanceRepository.findAllByAddress(addr))
        .thenReturn(List.of(latestTokenBalance));

    var response = addressService.getTokenByDisplayName(pageable, addr, "");

    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(BigInteger.ONE, response.getData().get(0).getQuantity());
    Assertions.assertEquals(addr, response.getData().get(0).getAddress());
  }
}
