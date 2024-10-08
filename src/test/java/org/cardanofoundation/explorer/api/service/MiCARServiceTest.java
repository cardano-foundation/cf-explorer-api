package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Optional;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressTxCountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.StakeAddressTxCountRepository;
import org.cardanofoundation.explorer.api.service.impl.MiCARServiceImpl;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.Address;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AddressTxCount;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.StakeAddressTxCount;

@ExtendWith(MockitoExtension.class)
public class MiCARServiceTest {
  @InjectMocks private MiCARServiceImpl miCARService;

  @Mock private AddressTxCountRepository addressTxCountRepository;

  @Mock private StakeAddressTxCountRepository stakeAddressTxCountRepository;
  @Mock private StakeAddressRepository stakeAddressRepository;
  @Mock private AddressRepository addressRepository;

  @Test
  void testGetCarbonEmissionsByAddress_matchingAddress() {
    String address =
        "addr1q8elqhkuvtyelgcedpup58r893awhg3l87a4rz5d5acatuj9y84nruafrmta2rewd5l46g8zxy4l49ly8kye79ddr3ksqal35g";

    AddressTxCount addressTxCount = AddressTxCount.builder().address(address).txCount(20L).build();
    when(addressRepository.findFirstByAddress(anyString()))
        .thenReturn(Optional.of(Address.builder().address(address).build()));
    when(addressTxCountRepository.findByAddress(anyString()))
        .thenReturn(Optional.of(addressTxCount));

    var actual = miCARService.getCarbonEmissionsByAddressAndPool(address);

    Assertions.assertEquals(actual.getAddress(), address);
    Assertions.assertEquals(actual.getTxCount(), 20L);
    Assertions.assertEquals(actual.getCarbonEmissionPerTx(), 0.000018987);
    Assertions.assertNull(actual.getStakeAddress());
  }

  @Test
  void testGetCarbonEmissionsByAddress_matchingStakeAddress() {
    String stakeAddress = "stake1u9zjr6e37w53a474puhx606ayr3rz2l6jljrmzvlzkk3cmg0m2zw0";

    StakeAddressTxCount stakeAddressTxCount =
        StakeAddressTxCount.builder().stakeAddress(stakeAddress).txCount(20L).build();

    when(stakeAddressRepository.findByView(anyString()))
        .thenReturn(Optional.of(StakeAddress.builder().view(stakeAddress).build()));
    when(stakeAddressTxCountRepository.findByStakeAddress(anyString()))
        .thenReturn(Optional.ofNullable(stakeAddressTxCount));

    var actual = miCARService.getCarbonEmissionsByAddressAndPool(stakeAddress);

    Assertions.assertEquals(actual.getStakeAddress(), stakeAddress);
    Assertions.assertEquals(actual.getTxCount(), 20L);
    Assertions.assertEquals(actual.getCarbonEmissionPerTx(), 0.000018987);
    Assertions.assertNull(actual.getAddress());
  }

  @Test
  void testGetCarbonEmissionsByAddress_AddressNotValid() {
    String address =
        "addr11q8elqhkuvtyelgcedpup58r893awhg3l87a4rz5d5acatuj9y84nruafrmta2rewd5l46g8zxy4l49ly8kye79ddr3ksqal35g";

    var actual = miCARService.getCarbonEmissionsByAddressAndPool(address);

    Assertions.assertNull(actual.getAddress());
    Assertions.assertNull(actual.getTxCount());
    Assertions.assertNull(actual.getStakeAddress());
  }

  @Test
  void testGetCarbonEmissionsByAddress_StakeAddressNotExist() {
    String address = "stake1";

    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.ofNullable(null));

    var actual = miCARService.getCarbonEmissionsByAddressAndPool(address);

    Assertions.assertNull(actual.getAddress());
    Assertions.assertNull(actual.getTxCount());
    Assertions.assertNull(actual.getStakeAddress());
  }
}
