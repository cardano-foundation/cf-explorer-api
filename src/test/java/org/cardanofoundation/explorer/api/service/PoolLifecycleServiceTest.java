package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.when;

import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochRewardProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.LifeCycleRewardProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.StakeKeyProjection;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.RewardRepository;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.service.impl.PoolLifecycleServiceImpl;
import org.cardanofoundation.explorer.consumercommon.entity.PoolUpdate;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.Assertions;
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

@ExtendWith(MockitoExtension.class)
class PoolLifecycleServiceTest {

  @Mock
  private StakeAddressRepository stakeAddressRepository;

  @Mock
  private PoolHashRepository poolHashRepository;

  @Mock
  private PoolUpdateRepository poolUpdateRepository;

  @Mock
  private RewardRepository rewardRepository;

  @Mock
  private PoolRetireRepository poolRetireRepository;

  @Mock
  private EpochStakeRepository epochStakeRepository;

  @Mock
  private EpochRepository epochRepository;

  @InjectMocks
  private PoolLifecycleServiceImpl poolLifecycleService;

  @Test
  void whenStakeKeyIsNotExist_returnEmptyPage() {
    Pageable pageable = PageRequest.of(0, 10);
    when(stakeAddressRepository.getPoolViewByStakeKey("stakeKeyNotFound", pageable)).thenReturn(
        Page.empty());
    Assertions.assertEquals(0,
        poolLifecycleService.getPoolViewByStakeKey("stakeKeyNotFound", pageable).getTotalItems());
    Assertions.assertEquals(List.of(),
        poolLifecycleService.getPoolViewByStakeKey("stakeKeyNotFound", pageable).getData());
  }

  @Test
  void whenStakeKeyIsExist_returnPoolViewList() {
    Pageable pageable = PageRequest.of(0, 10);
    List<String> dataList = new ArrayList<>();
    dataList.add("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    dataList.add("pool1wz27a9pvd2jat98s375lxaaj7qxy405km4vzp83wn5vtwp59kvh");
    dataList.add("pool1qeyjycp9ef0drrzaq3u9ylwclqa56zl5n4yd75txw92csug3mzn");
    when(stakeAddressRepository.getPoolViewByStakeKey(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p", pageable)).thenReturn(
        new PageImpl<>(dataList));
    Assertions.assertEquals(3,
        poolLifecycleService.getPoolViewByStakeKey(
                "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p", pageable)
            .getTotalItems());
  }

//  @Test
//  void whenPoolViewIsNotExist_returnEmptyOrNull() {
//    Pageable pageable = PageRequest.of(0, 10);
//    when(poolUpdateRepository.findPoolUpdateByPool("poolViewNotFound", null, null, null,
//        pageable)).thenReturn(
//        Page.empty());
//    when(rewardRepository.getRewardInfoByPool("poolViewNotFound", pageable)).thenReturn(
//        Page.empty());
//    when(poolHashRepository.getPoolInfo("poolViewNotFound")).thenReturn(null);
//    when(poolRetireRepository.findByPoolView("poolViewNotFound")).thenReturn(null);
//    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(69));
//    when(poolRetireRepository.getPoolDeRegistration("poolViewNotFound", null, null, null,
//        pageable)).thenReturn(
//        Page.empty());
//    when(poolHashRepository.getPoolRegistrationByPool("poolViewNotFound", pageable)).thenReturn(
//        Page.empty());
//    when(poolUpdateRepository.findPoolUpdateByPool("poolViewNotFound", pageable)).thenReturn(
//        Page.empty());
//    Assertions.assertEquals(0,
//        poolLifecycleService.registration("poolViewNotFound", null, null, null, pageable)
//            .getTotalItems());
//    Assertions.assertEquals(0,
//        poolLifecycleService.poolUpdate("poolViewNotFound", null, null, null, pageable)
//            .getTotalItems());
//    Assertions.assertEquals(0,
//        poolLifecycleService.listReward("poolViewNotFound", pageable).getTotalItems());
//    Assertions.assertNull(poolLifecycleService.poolInfo("poolViewNotFound").getPoolName());
//    Assertions.assertNull(poolLifecycleService.poolInfo("poolViewNotFound").getPoolId());
//    Assertions.assertEquals(0,
//        poolLifecycleService.deRegistration("poolViewNotFound", null, null, null, pageable)
//            .getTotalItems());
//    Assertions.assertEquals(0,
//        poolLifecycleService.registrationList("poolViewNotFound", pageable)
//            .getTotalItems());
//    Assertions.assertEquals(0,
//        poolLifecycleService.poolUpdateList("poolViewNotFound", pageable)
//            .getTotalItems());
//  }

//  @Test
//  void whenPoolViewIsExist_returnResponse() {
//    Pageable pageable = PageRequest.of(0, 10);
//    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
//    when(projection.getFee()).thenReturn(BigInteger.TEN);
//    when(projection.getPoolUpdateId()).thenReturn(1L);
//    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
//    when(projection.getMargin()).thenReturn(1.0);
//    when(projection.getTxHash()).thenReturn(
//        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
//    when(poolUpdateRepository.findPoolUpdateByPool(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, null, null,
//        pageable)).thenReturn(new PageImpl<>(List.of(projection)));
//    Assertions.assertEquals(1,
//        poolLifecycleService.registration(
//                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, null, null, pageable)
//            .getTotalItems());
//    Assertions.assertEquals(1,
//        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//                null, null, null, pageable)
//            .getTotalItems());
//  }

  @Test
  void whenPoolViewIsExistAndTxHashIsNotExist_returnEmptyPage() {
    Pageable pageable = PageRequest.of(0, 10);
    when(poolUpdateRepository.findPoolUpdateByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", "txHashNotFound", null,
        null,
        pageable)).thenReturn(
        Page.empty());
    Assertions.assertEquals(0,
        poolLifecycleService.registration(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", "txHashNotFound", null,
                null,
                pageable)
            .getTotalItems());
    Assertions.assertEquals(0,
        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
                "txHashNotFound", null, null, pageable)
            .getTotalItems());
  }

//  @Test
//  void whenPoolViewIsExistAndTxHashIsExist_returnResponse() {
//    Pageable pageable = PageRequest.of(0, 10);
//    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
//    when(projection.getFee()).thenReturn(BigInteger.TEN);
//    when(projection.getPoolUpdateId()).thenReturn(1L);
//    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
//    when(projection.getMargin()).thenReturn(1.0);
//    when(projection.getTxHash()).thenReturn(
//        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
//    when(poolUpdateRepository.findPoolUpdateByPool(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", null, null,
//        pageable)).thenReturn(new PageImpl<>(List.of(projection)));
//    Assertions.assertEquals(1,
//        poolLifecycleService.registration(
//                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//                "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", null, null,
//                pageable)
//            .getTotalItems());
//    Assertions.assertEquals(1,
//        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//                "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", null, null,
//                pageable)
//            .getTotalItems());
//  }

  @Test
  void whenPoolViewIsExistAndTransactionTimeIsNotBetweenFromDateInToDate_returnEmptyPage() {
    Pageable pageable = PageRequest.of(0, 10);
    Timestamp fromDate = Timestamp.valueOf("2023-01-01 00:00:00");
    Timestamp toDate = Timestamp.valueOf("2023-04-01 00:00:00");
    when(poolUpdateRepository.findPoolUpdateByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, fromDate,
        toDate,
        pageable)).thenReturn(
        Page.empty());
    Assertions.assertEquals(0, poolLifecycleService.registration(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, fromDate,
        toDate,
        pageable).getTotalItems());
    Assertions.assertEquals(0,
        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
            null, fromDate, toDate, pageable).getTotalItems());
  }

//  @Test
//  void whenPoolViewIsExistAndTransactionTimeIsBetweenFromDateInToDate_returnResponse() {
//    Pageable pageable = PageRequest.of(0, 10);
//    Timestamp fromDate = Timestamp.valueOf("2023-01-01 00:00:00");
//    Timestamp toDate = Timestamp.valueOf("2023-04-01 00:00:00");
//    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
//    when(projection.getFee()).thenReturn(BigInteger.TEN);
//    when(projection.getPoolUpdateId()).thenReturn(1L);
//    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
//    when(projection.getMargin()).thenReturn(1.0);
//    when(projection.getTxHash()).thenReturn(
//        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
//    when(poolUpdateRepository.findPoolUpdateByPool(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, fromDate,
//        toDate,
//        pageable)).thenReturn(
//        new PageImpl<>(List.of(projection)));
//    Assertions.assertEquals(1, poolLifecycleService.registration(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, fromDate,
//        toDate,
//        pageable).getTotalItems());
//    Assertions.assertEquals(1,
//        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//            null, fromDate, toDate, pageable).getTotalItems());
//  }

  @Test
  void whenPoolViewIsExistAndTxHashIsExistAndTransactionTimeIsNotBetweenFromDateInToDate_returnEmptyPage() {
    Pageable pageable = PageRequest.of(0, 10);
    Timestamp fromDate = Timestamp.valueOf("2023-01-01 00:00:00");
    Timestamp toDate = Timestamp.valueOf("2023-04-01 00:00:00");
    when(poolUpdateRepository.findPoolUpdateByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate,
        toDate,
        pageable)).thenReturn(
        Page.empty());
    Assertions.assertEquals(0, poolLifecycleService.registration(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate,
        toDate,
        pageable).getTotalItems());
    Assertions.assertEquals(0,
        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
            "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate, toDate,
            pageable).getTotalItems());
  }

//  @Test
//  void whenPoolViewIsExistAndTxHashIsExistAndTransactionTimeIsBetweenFromDateInToDate_returnResponse() {
//    Pageable pageable = PageRequest.of(0, 10);
//    Timestamp fromDate = Timestamp.valueOf("2023-01-01 00:00:00");
//    Timestamp toDate = Timestamp.valueOf("2023-04-01 00:00:00");
//    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
//    when(projection.getFee()).thenReturn(BigInteger.TEN);
//    when(projection.getPoolUpdateId()).thenReturn(1L);
//    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
//    when(projection.getMargin()).thenReturn(1.0);
//    when(projection.getTxHash()).thenReturn(
//        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
//    when(poolUpdateRepository.findPoolUpdateByPool(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate,
//        toDate,
//        pageable)).thenReturn(
//        new PageImpl<>(List.of(projection)));
//    Assertions.assertEquals(1, poolLifecycleService.registration(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate,
//        toDate,
//        pageable).getTotalItems());
//    Assertions.assertEquals(1,
//        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//            "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate, toDate,
//            pageable).getTotalItems());
//  }

  @Test
  void whenPoolViewIsExistAndTxHashIsNotExistAndTransactionTimeIsNotBetweenFromDateInToDate_returnEmptyPage() {
    Pageable pageable = PageRequest.of(0, 10);
    Timestamp fromDate = Timestamp.valueOf("2023-01-01 00:00:00");
    Timestamp toDate = Timestamp.valueOf("2023-04-01 00:00:00");
    when(poolUpdateRepository.findPoolUpdateByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", "txHashNotForm", fromDate,
        toDate,
        pageable)).thenReturn(
        Page.empty());
    Assertions.assertEquals(0, poolLifecycleService.registration(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", "txHashNotForm", fromDate,
        toDate,
        pageable).getTotalItems());
    Assertions.assertEquals(0,
        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
            "txHashNotForm", fromDate, toDate, pageable).getTotalItems());
  }

  @Test
  void whenPoolViewIsNotExistAndPoolUpdateIdIsNotExist_returnEmptyObject() {
    when(poolHashRepository.getPoolInfo("poolViewNotFound")).thenReturn(null);
    when(poolHashRepository.getPoolRegistration(1111111111111111111L)).thenReturn(null);
    when(poolUpdateRepository.findOwnerAccountByPoolUpdate(1111111111111111111L)).thenReturn(null);
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1111111111111111111L)
            .getPoolId());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1111111111111111111L)
            .getPoolView());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1111111111111111111L)
            .getPoolName());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1111111111111111111L)
            .getTxHash());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1111111111111111111L)
            .getDeposit());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1111111111111111111L)
            .getPledge());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1111111111111111111L)
            .getRewardAccount());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1111111111111111111L)
            .getFee());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1111111111111111111L)
            .getTime());
  }

  @Test
  void whenPoolViewIsExistAndPoolUpdateIdIsNotExist_returnResponse() {
    PoolInfoProjection projection = Mockito.mock(PoolInfoProjection.class);
    when(projection.getPoolId()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    when(projection.getPoolName()).thenReturn("Test");
    when(projection.getPoolView()).thenReturn(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    when(poolHashRepository.getPoolInfo(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(projection);
    when(poolHashRepository.getPoolRegistration(1111111111111111111L)).thenReturn(null);
    when(poolUpdateRepository.findOwnerAccountByPoolUpdate(1111111111111111111L)).thenReturn(null);
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535",
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1111111111111111111L)
            .getPoolId());
    Assertions.assertEquals("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1111111111111111111L)
            .getPoolView());
    Assertions.assertEquals("Test",
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1111111111111111111L)
            .getPoolName());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1111111111111111111L)
            .getTxHash());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1111111111111111111L)
            .getDeposit());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1111111111111111111L)
            .getPledge());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1111111111111111111L)
            .getRewardAccount());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1111111111111111111L)
            .getFee());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1111111111111111111L)
            .getTime());
  }

  @Test
  void whenPoolViewIsNotExistAndPoolUpdateIdIsExist_returnResponse() {
    Timestamp time = Timestamp.valueOf("2023-01-01 00:00:00");
    PoolRegistrationProjection projection = Mockito.mock(PoolRegistrationProjection.class);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    when(projection.getDeposit()).thenReturn(BigInteger.TEN);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getTime()).thenReturn(time);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getRewardAccount()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    when(poolHashRepository.getPoolInfo(
        "poolViewNotFound")).thenReturn(null);
    when(poolHashRepository.getPoolRegistration(1L)).thenReturn(projection);
    when(poolUpdateRepository.findOwnerAccountByPoolUpdate(1L)).thenReturn(
        Collections.singletonList(
            "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1L)
            .getPoolId());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1L)
            .getPoolView());
    Assertions.assertNull(
        poolLifecycleService.registrationDetail("poolViewNotFound", 1L)
            .getPoolName());
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535",
        poolLifecycleService.registrationDetail(
                "poolViewNotFound", 1L)
            .getTxHash());
    Assertions.assertEquals(BigInteger.TEN,
        poolLifecycleService.registrationDetail(
                "poolViewNotFound", 1L)
            .getDeposit());
    Assertions.assertEquals(BigInteger.TEN,
        poolLifecycleService.registrationDetail(
                "poolViewNotFound", 1L)
            .getPledge());
    Assertions.assertEquals("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p",
        poolLifecycleService.registrationDetail(
                "poolViewNotFound", 1L)
            .getRewardAccount());
    Assertions.assertEquals(time,
        poolLifecycleService.registrationDetail(
                "poolViewNotFound", 1L)
            .getTime());
    Assertions.assertEquals(1, poolLifecycleService.registrationDetail(
        "poolViewNotFound", 1L).getStakeKeys().size());
  }

  @Test
  void whenPoolViewIsExistAndPoolUpdateIdIsExist_returnResponse() {
    Timestamp time = Timestamp.valueOf("2023-01-01 00:00:00");
    PoolRegistrationProjection projection = Mockito.mock(PoolRegistrationProjection.class);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    when(projection.getDeposit()).thenReturn(BigInteger.TEN);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getTime()).thenReturn(time);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    PoolInfoProjection poolInfoProjection = Mockito.mock(PoolInfoProjection.class);
    when(poolInfoProjection.getPoolId()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    when(poolInfoProjection.getPoolName()).thenReturn("Test");
    when(poolInfoProjection.getPoolView()).thenReturn(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    when(projection.getRewardAccount()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    when(poolHashRepository.getPoolInfo(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(poolInfoProjection);
    when(poolHashRepository.getPoolRegistration(1L)).thenReturn(projection);
    when(poolUpdateRepository.findOwnerAccountByPoolUpdate(1L)).thenReturn(
        Collections.singletonList(
            "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535",
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1L)
            .getPoolId());
    Assertions.assertEquals("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1L)
            .getPoolView());
    Assertions.assertEquals("Test",
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1L)
            .getPoolName());
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535",
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1L)
            .getTxHash());
    Assertions.assertEquals(BigInteger.TEN,
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1L)
            .getDeposit());
    Assertions.assertEquals(BigInteger.TEN,
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1L)
            .getPledge());
    Assertions.assertEquals("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p",
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1L)
            .getRewardAccount());
    Assertions.assertEquals(time,
        poolLifecycleService.registrationDetail(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1L)
            .getTime());
    Assertions.assertEquals(1, poolLifecycleService.registrationDetail(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", 1L).getStakeKeys().size());
  }

  @Test
  void whenPoolUpdateIdIsNotExist_returnNull() {
    when(poolUpdateRepository.findPoolUpdateDetailById(1111111111111111111L)).thenReturn(null);
    Assertions.assertNull(
        poolLifecycleService.poolUpdateDetail(1111111111111111111L));
  }

  @Test
  void whenPoolUpdateIdIsExist_returnResponse() {
    Timestamp time = Timestamp.valueOf("2023-01-01 00:00:00");
    PoolUpdateDetailProjection projection = Mockito.mock(PoolUpdateDetailProjection.class);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    when(projection.getHashId()).thenReturn(1L);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getTime()).thenReturn(time);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getPoolName()).thenReturn("Test");
    when(projection.getRewardAccount()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    when(projection.getCost()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(1.0);
    when(projection.getVrfKey()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(poolUpdateRepository.findPoolUpdateDetailById(1L)).thenReturn(
        projection);
    PoolUpdate poolUpdate = Mockito.mock(PoolUpdate.class);
    when(poolUpdate.getPledge()).thenReturn(BigInteger.TWO);
    when(poolUpdate.getMargin()).thenReturn(1.5);
    when(poolUpdateRepository.findOwnerAccountByPoolUpdate(1L)).thenReturn(
        List.of("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    when(poolUpdateRepository.findTopByIdLessThanAndPoolHashIdOrderByIdDesc(1L, 1L)).thenReturn(
        poolUpdate);
    Assertions.assertNotNull(
        poolLifecycleService.poolUpdateDetail(1L));
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535",
        poolLifecycleService.poolUpdateDetail(1L)
            .getTxHash());
    Assertions.assertEquals("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p",
        poolLifecycleService.poolUpdateDetail(1L)
            .getRewardAccount());
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda",
        poolLifecycleService.poolUpdateDetail(1L)
            .getVrfKey());
    Assertions.assertEquals(BigInteger.TEN,
        poolLifecycleService.poolUpdateDetail(1L)
            .getPledge());
    Assertions.assertEquals(time,
        poolLifecycleService.poolUpdateDetail(1L)
            .getTime());
    Assertions.assertEquals(BigInteger.TEN,
        poolLifecycleService.poolUpdateDetail(1L)
            .getFee());
    Assertions.assertEquals(1.0,
        poolLifecycleService.poolUpdateDetail(1L)
            .getMargin());
    Assertions.assertEquals(1.5,
        poolLifecycleService.poolUpdateDetail(1L)
            .getPreviousMargin());
    Assertions.assertEquals(BigInteger.TWO,
        poolLifecycleService.poolUpdateDetail(1L)
            .getPreviousPledge());

  }

  @Test
  void whenPoolUpdateIdIsExistAndIsFirstUpdateOrUnique_returnResponse() {
    Timestamp time = Timestamp.valueOf("2023-01-01 00:00:00");
    PoolUpdateDetailProjection projection = Mockito.mock(PoolUpdateDetailProjection.class);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    when(projection.getHashId()).thenReturn(1L);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getTime()).thenReturn(time);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getPoolName()).thenReturn("Test");
    when(projection.getRewardAccount()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    when(projection.getCost()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(1.0);
    when(projection.getVrfKey()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(poolUpdateRepository.findPoolUpdateDetailById(1L)).thenReturn(
        projection);
    when(poolUpdateRepository.findOwnerAccountByPoolUpdate(1L)).thenReturn(
        List.of("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    when(poolUpdateRepository.findTopByIdLessThanAndPoolHashIdOrderByIdDesc(1L, 1L)).thenReturn(
        null);
    Assertions.assertNotNull(
        poolLifecycleService.poolUpdateDetail(1L));
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535",
        poolLifecycleService.poolUpdateDetail(1L)
            .getTxHash());
    Assertions.assertEquals("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p",
        poolLifecycleService.poolUpdateDetail(1L)
            .getRewardAccount());
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda",
        poolLifecycleService.poolUpdateDetail(1L)
            .getVrfKey());
    Assertions.assertEquals(BigInteger.TEN,
        poolLifecycleService.poolUpdateDetail(1L)
            .getPledge());
    Assertions.assertEquals(time,
        poolLifecycleService.poolUpdateDetail(1L)
            .getTime());
    Assertions.assertEquals(BigInteger.TEN,
        poolLifecycleService.poolUpdateDetail(1L)
            .getFee());
    Assertions.assertEquals(1.0,
        poolLifecycleService.poolUpdateDetail(1L)
            .getMargin());
    Assertions.assertNull(
        poolLifecycleService.poolUpdateDetail(1L)
            .getPreviousMargin());
    Assertions.assertNull(
        poolLifecycleService.poolUpdateDetail(1L)
            .getPreviousPledge());

  }

//  @Test
//  void whenPoolViewIsExist_returnRewardResponse() {
//    Timestamp time = Timestamp.valueOf("2023-01-01 00:00:00");
//    Pageable pageable = PageRequest.of(0, 10);
//    LifeCycleRewardProjection projection = Mockito.mock(LifeCycleRewardProjection.class);
//    when(projection.getAddress()).thenReturn(
//        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
//    when(projection.getAmount()).thenReturn(BigInteger.TWO);
//    when(projection.getEpochNo()).thenReturn(69);
//    when(projection.getTime()).thenReturn(time);
//    when(rewardRepository.getRewardInfoByPool(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", pageable)).thenReturn(
//        new PageImpl<>(List.of(projection)));
//    Assertions.assertEquals(1,
//        poolLifecycleService.listReward("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//            pageable).getTotalItems());
//    Assertions.assertEquals(1,
//        poolLifecycleService.listReward("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
//            pageable).getData().size());
//  }

//  @Test
//  void whenPoolViewIsExist_returnInfoResponse() {
//    PoolInfoProjection projection = Mockito.mock(PoolInfoProjection.class);
//    when(projection.getPoolView()).thenReturn(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
//    when(projection.getId()).thenReturn(69L);
//    when(projection.getPoolName()).thenReturn("Test");
//    when(projection.getPoolId()).thenReturn(
//        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
//    when(poolHashRepository.getPoolInfo(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(projection);
//    when(epochStakeRepository.activeStakeByPool(69L)).thenReturn(BigInteger.TEN);
//    when(poolUpdateRepository.findOwnerAccountByPoolView(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(
//        List.of("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
//    when(rewardRepository.getTotalRewardByPool(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(
//        BigInteger.valueOf(10000));
//    when(poolRetireRepository.findByPoolView(
//        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(List.of(69));
//    Assertions.assertEquals("Test",
//        poolLifecycleService.poolInfo("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")
//            .getPoolName());
//    Assertions.assertEquals(BigInteger.TEN,
//        poolLifecycleService.poolInfo("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")
//            .getPoolSize());
//    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda",
//        poolLifecycleService.poolInfo("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")
//            .getPoolId());
//  }

  @Test
  void whenPoolViewIsExist_returnPoolRetireResponse() {
    Pageable pageable = PageRequest.of(0, 10);
    PoolDeRegistrationProjection retireProjection = Mockito.mock(
        PoolDeRegistrationProjection.class);
    when(retireProjection.getRetiringEpoch()).thenReturn(69);
    when(retireProjection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(retireProjection.getFee()).thenReturn(BigInteger.TEN);
    when(poolRetireRepository.getPoolDeRegistration(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        null, null, null, pageable)).thenReturn(new PageImpl<>(List.of(retireProjection)));
    PoolInfoProjection projection = Mockito.mock(PoolInfoProjection.class);
    when(projection.getPoolView()).thenReturn(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    when(projection.getPoolName()).thenReturn("Test");
    when(projection.getPoolId()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(poolHashRepository.getPoolInfo(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(projection);
    EpochRewardProjection rewardProjection = Mockito.mock(EpochRewardProjection.class);
    when(rewardProjection.getEpochNo()).thenReturn(69);
    when(rewardProjection.getAmount()).thenReturn(BigInteger.valueOf(1001));
    when(rewardRepository.getRewardRefundByEpoch(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", Set.of(69))).thenReturn(
        List.of(rewardProjection));
    Assertions.assertEquals(1,
        poolLifecycleService.deRegistration(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, null, null, pageable)
            .getTotalItems());
    Assertions.assertEquals(69,
        poolLifecycleService.deRegistration(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, null, null, pageable)
            .getData().get(0).getRetiringEpoch());
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda",
        poolLifecycleService.deRegistration(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, null, null, pageable)
            .getData().get(0).getTxHash());
  }

  @Test
  void whenPoolViewIsExist_returnPoolRegistrationListResponse() {
    Pageable pageable = PageRequest.of(0, 10);
    PoolRegistrationProjection registrationProjection = Mockito.mock(
        PoolRegistrationProjection.class);
    when(registrationProjection.getPoolUpdateId()).thenReturn(69L);
    when(registrationProjection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(registrationProjection.getFee()).thenReturn(BigInteger.TEN);
    when(registrationProjection.getDeposit()).thenReturn(BigInteger.valueOf(500));
    when(poolHashRepository.getPoolRegistrationByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", pageable)).thenReturn(
        new PageImpl<>(List.of(registrationProjection)));
    StakeKeyProjection projection = Mockito.mock(StakeKeyProjection.class);
    when(projection.getPoolUpdateId()).thenReturn(69L);
    when(projection.getView()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    when(poolUpdateRepository.findOwnerAccountByPoolUpdate(Set.of(69L))).thenReturn(
        List.of(projection));
    Assertions.assertEquals(1,
        poolLifecycleService.registrationList(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", pageable)
            .getTotalItems());
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda",
        poolLifecycleService.registrationList(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", pageable)
            .getData().get(0).getTxHash());
  }

  @Test
  void whenPoolViewIsExist_returnPoolUpdateListResponse() {
    Pageable pageable = PageRequest.of(0, 10);
    PoolUpdateDetailProjection projection = Mockito.mock(
        PoolUpdateDetailProjection.class);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(projection.getHashId()).thenReturn(1L);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getPoolName()).thenReturn("Test");
    when(projection.getRewardAccount()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    when(projection.getCost()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(1.0);
    when(projection.getVrfKey()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(projection.getPoolUpdateId()).thenReturn(69L);
    when(poolUpdateRepository.findPoolUpdateByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", pageable)).thenReturn(
        new PageImpl<>(List.of(projection)));
    when(poolUpdateRepository.findOwnerAccountByPoolUpdate(69L)).thenReturn(
        List.of("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    PoolUpdate poolUpdate = Mockito.mock(PoolUpdate.class);
    when(poolUpdate.getPledge()).thenReturn(BigInteger.TWO);
    when(poolUpdate.getMargin()).thenReturn(1.5);
    when(poolUpdateRepository.findTopByIdLessThanAndPoolHashIdOrderByIdDesc(69L, 1L)).thenReturn(
        poolUpdate);
    Assertions.assertEquals(1,
        poolLifecycleService.poolUpdateList(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", pageable)
            .getTotalItems());
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda",
        poolLifecycleService.poolUpdateList(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", pageable)
            .getData().get(0).getTxHash());
  }
}
