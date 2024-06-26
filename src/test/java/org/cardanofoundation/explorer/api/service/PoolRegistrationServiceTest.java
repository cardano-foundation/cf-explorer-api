package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.model.response.pool.PoolTxResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolOwnerProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.TxBlockEpochProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolOwnerRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.service.impl.PoolRegistrationServiceImpl;

@ExtendWith(MockitoExtension.class)
class PoolRegistrationServiceTest {

  @InjectMocks private PoolRegistrationServiceImpl poolRegistrationService;

  @Mock private PoolUpdateRepository poolUpdateRepository;

  @Mock private PoolOwnerRepository poolOwnerRepository;

  @Mock private PoolRetireRepository poolRetireRepository;

  @Mock private TxRepository txRepository;

  @Test
  void whenPoolRegistrationIsEmpty_returnEmptyPage() {
    Pageable pageable = PageRequest.of(0, 10);
    when(poolUpdateRepository.getDataForPoolRegistration(pageable)).thenReturn(Page.empty());
    when(poolOwnerRepository.getStakeKeyList(Collections.emptySet())).thenReturn(List.of());
    when(txRepository.findTxIn(Collections.emptySet())).thenReturn(List.of());
    Assertions.assertEquals(
        0, poolRegistrationService.getDataForPoolRegistration(pageable).getTotalItems());
    Assertions.assertEquals(
        List.of(), poolRegistrationService.getDataForPoolRegistration(pageable).getData());
  }

  @Test
  void whenPoolDeRegistrationIsEmpty_returnEmptyPage() {
    Pageable pageable = PageRequest.of(0, 10);
    when(poolRetireRepository.getDataForPoolDeRegistration(pageable)).thenReturn(Page.empty());
    when(poolOwnerRepository.getStakeKeyList(Collections.emptySet())).thenReturn(List.of());
    when(txRepository.findTxIn(Collections.emptySet())).thenReturn(List.of());
    Assertions.assertEquals(
        0, poolRegistrationService.getDataForPoolDeRegistration(pageable).getTotalItems());
    Assertions.assertEquals(
        List.of(), poolRegistrationService.getDataForPoolDeRegistration(pageable).getData());
  }

  @Test
  void whenPoolRegistrationIsNotEmptyAndStakeKeyIsEmpty_returnResponseWithStakeKeyIsNull() {
    Pageable pageable = PageRequest.of(0, 10);
    TxBlockEpochProjection projection = Mockito.mock(TxBlockEpochProjection.class);
    when(projection.getTxId()).thenReturn(1L);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(0.1);
    when(projection.getCost()).thenReturn(BigInteger.TWO);
    when(projection.getPoolId()).thenReturn(1L);

    TxIOProjection tx = Mockito.mock(TxIOProjection.class);
    when(tx.getId()).thenReturn(1L);
    when(tx.getHash())
        .thenReturn("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(tx.getTime()).thenReturn(LocalDateTime.now());
    when(tx.getEpochNo()).thenReturn(420);
    when(txRepository.findTxIn(new HashSet<>(List.of(1L)))).thenReturn(List.of(tx));
    when(projection.getPoolView())
        .thenReturn("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    when(poolUpdateRepository.getDataForPoolRegistration(pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(projection)));
    when(poolOwnerRepository.getStakeKeyList(new HashSet<>(List.of(1L)))).thenReturn(List.of());
    Assertions.assertEquals(
        1, poolRegistrationService.getDataForPoolRegistration(pageable).getTotalItems());
    List<PoolTxResponse> data =
        poolRegistrationService.getDataForPoolRegistration(pageable).getData();
    Assertions.assertNull(data.get(0).getStakeKey());
  }

  @Test
  void whenPoolDeRegistrationIsNotEmptyAndStakeKeyIsEmpty_returnResponseWithStakeKeyIsNull() {
    Pageable pageable = PageRequest.of(0, 10);
    TxBlockEpochProjection projection = Mockito.mock(TxBlockEpochProjection.class);
    when(projection.getTxId()).thenReturn(1L);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(0.1);
    when(projection.getCost()).thenReturn(BigInteger.TWO);
    when(projection.getPoolId()).thenReturn(1L);
    when(projection.getPoolView())
        .thenReturn("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");

    TxIOProjection tx = Mockito.mock(TxIOProjection.class);
    when(tx.getId()).thenReturn(1L);
    when(tx.getHash())
        .thenReturn("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(tx.getTime()).thenReturn(LocalDateTime.now());
    when(tx.getEpochNo()).thenReturn(420);
    when(txRepository.findTxIn(new HashSet<>(List.of(1L)))).thenReturn(List.of(tx));
    when(poolRetireRepository.getDataForPoolDeRegistration(pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(projection)));
    when(poolOwnerRepository.getStakeKeyList(new HashSet<>(List.of(1L)))).thenReturn(List.of());
    Assertions.assertEquals(
        1, poolRegistrationService.getDataForPoolDeRegistration(pageable).getTotalItems());
    List<PoolTxResponse> data =
        poolRegistrationService.getDataForPoolDeRegistration(pageable).getData();
    Assertions.assertNull(data.get(0).getStakeKey());
  }

  @Test
  void whenPoolRegistrationIsNotEmpty_returnResponse() {
    Pageable pageable = PageRequest.of(0, 10);
    TxBlockEpochProjection projection = Mockito.mock(TxBlockEpochProjection.class);
    when(projection.getTxId()).thenReturn(1L);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(0.1);
    when(projection.getCost()).thenReturn(BigInteger.TWO);
    when(projection.getPoolId()).thenReturn(1L);
    when(projection.getPoolView())
        .thenReturn("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    PoolOwnerProjection poolOwnerProjection = Mockito.mock(PoolOwnerProjection.class);
    when(poolOwnerProjection.getPoolId()).thenReturn(1L);
    when(poolOwnerProjection.getAddress())
        .thenReturn("stake1uyas69dqgnjnsk02jdklxedqane4xxag7ezgguymdqdra3s3cuydh");

    TxIOProjection tx = Mockito.mock(TxIOProjection.class);
    when(tx.getId()).thenReturn(1L);
    when(tx.getHash())
        .thenReturn("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(tx.getTime()).thenReturn(LocalDateTime.now());
    when(tx.getEpochNo()).thenReturn(420);
    when(txRepository.findTxIn(new HashSet<>(List.of(1L)))).thenReturn(List.of(tx));

    when(poolUpdateRepository.getDataForPoolRegistration(pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(projection)));
    when(poolOwnerRepository.getStakeKeyList(new HashSet<>(List.of(1L))))
        .thenReturn(List.of(poolOwnerProjection));
    Assertions.assertEquals(
        1, poolRegistrationService.getDataForPoolRegistration(pageable).getTotalItems());
    List<PoolTxResponse> data =
        poolRegistrationService.getDataForPoolRegistration(pageable).getData();
    Assertions.assertEquals(
        "stake1uyas69dqgnjnsk02jdklxedqane4xxag7ezgguymdqdra3s3cuydh",
        data.get(0).getStakeKey().get(0));
  }

  @Test
  void whenPoolDeRegistrationIsNotEmpty_returnResponse() {
    Pageable pageable = PageRequest.of(0, 10);
    TxBlockEpochProjection projection = Mockito.mock(TxBlockEpochProjection.class);
    when(projection.getTxId()).thenReturn(1L);

    TxIOProjection tx = Mockito.mock(TxIOProjection.class);
    when(tx.getId()).thenReturn(1L);
    when(tx.getHash())
        .thenReturn("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(tx.getTime()).thenReturn(LocalDateTime.now());
    when(tx.getEpochNo()).thenReturn(420);
    when(txRepository.findTxIn(new HashSet<>(List.of(1L)))).thenReturn(List.of(tx));

    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(0.1);
    when(projection.getCost()).thenReturn(BigInteger.TWO);
    when(projection.getPoolId()).thenReturn(1L);
    when(projection.getPoolView())
        .thenReturn("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    PoolOwnerProjection poolOwnerProjection = Mockito.mock(PoolOwnerProjection.class);
    when(poolOwnerProjection.getPoolId()).thenReturn(1L);
    when(poolOwnerProjection.getAddress())
        .thenReturn("stake1uyas69dqgnjnsk02jdklxedqane4xxag7ezgguymdqdra3s3cuydh");
    when(poolRetireRepository.getDataForPoolDeRegistration(pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(projection)));
    when(poolOwnerRepository.getStakeKeyList(new HashSet<>(List.of(1L))))
        .thenReturn(List.of(poolOwnerProjection));
    Assertions.assertEquals(
        1, poolRegistrationService.getDataForPoolDeRegistration(pageable).getTotalItems());
    List<PoolTxResponse> data =
        poolRegistrationService.getDataForPoolDeRegistration(pageable).getData();
    Assertions.assertEquals(
        "stake1uyas69dqgnjnsk02jdklxedqane4xxag7ezgguymdqdra3s3cuydh",
        data.get(0).getStakeKey().get(0));
  }
}
