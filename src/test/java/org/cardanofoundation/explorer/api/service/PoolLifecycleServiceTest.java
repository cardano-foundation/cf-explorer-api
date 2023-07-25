package org.cardanofoundation.explorer.api.service;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.SPOStatusResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochRewardProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.LifeCycleRewardProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.StakeKeyProjection;
import org.cardanofoundation.explorer.api.repository.*;
import org.cardanofoundation.explorer.api.service.impl.PoolLifecycleServiceImpl;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.*;

import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.consumercommon.entity.PoolHash;
import org.cardanofoundation.explorer.consumercommon.entity.PoolUpdate;
import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;
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

  @Mock
  private FetchRewardDataService fetchRewardDataService;

  @Mock
  private PoolInfoRepository poolInfoRepository;

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

  @Test
  void whenPoolViewIsNotExist_returnEmptyOrNull() {
    Pageable pageable = PageRequest.of(0, 10);
    when(poolUpdateRepository.findPoolUpdateByPool("poolViewNotFound", null, null, null,
        pageable)).thenReturn(
        Page.empty());
    when(rewardRepository.getRewardInfoByPool("poolViewNotFound", pageable)).thenReturn(
        Page.empty());
    when(poolHashRepository.getPoolInfo("poolViewNotFound")).thenReturn(null);
    when(poolRetireRepository.findByPoolView("poolViewNotFound")).thenReturn(null);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(69));
    when(poolRetireRepository.getPoolDeRegistration("poolViewNotFound", null, null, null,
        pageable)).thenReturn(
        Page.empty());
    when(poolHashRepository.getPoolRegistrationByPool("poolViewNotFound", pageable)).thenReturn(
        Page.empty());
    when(poolUpdateRepository.findPoolUpdateByPool("poolViewNotFound", pageable)).thenReturn(
        Page.empty());
    Assertions.assertEquals(0,
        poolLifecycleService.registration("poolViewNotFound", null, null, null, pageable)
            .getTotalItems());
    Assertions.assertEquals(0,
        poolLifecycleService.poolUpdate("poolViewNotFound", null, null, null, pageable)
            .getTotalItems());
    Assertions.assertEquals(0,
        poolLifecycleService.listReward("poolViewNotFound", pageable).getTotalItems());
    Assertions.assertNull(poolLifecycleService.poolInfo("poolViewNotFound").getPoolName());
    Assertions.assertNull(poolLifecycleService.poolInfo("poolViewNotFound").getPoolId());
    Assertions.assertEquals(0,
        poolLifecycleService.deRegistration("poolViewNotFound", null, null, null, pageable)
            .getTotalItems());
    Assertions.assertEquals(0,
        poolLifecycleService.registrationList("poolViewNotFound", pageable)
            .getTotalItems());
    Assertions.assertEquals(0,
        poolLifecycleService.poolUpdateList("poolViewNotFound", pageable)
            .getTotalItems());
  }

  @Test
  void whenPoolViewIsExist_returnResponse() {
    Pageable pageable = PageRequest.of(0, 10);
    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getMargin()).thenReturn(1.0);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(poolUpdateRepository.findPoolUpdateByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, null, null,
        pageable)).thenReturn(new PageImpl<>(List.of(projection)));
    when(poolUpdateRepository.findPoolRegistrationByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, null, null,
        pageable)).thenReturn(new PageImpl<>(List.of(projection)));
    Assertions.assertEquals(1,
        poolLifecycleService.registration(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, null, null, pageable)
            .getTotalItems());
    Assertions.assertEquals(1,
        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
                null, null, null, pageable)
            .getTotalItems());
  }

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

  @Test
  void whenPoolViewIsExistAndTxHashIsExist_returnResponse() {
    Pageable pageable = PageRequest.of(0, 10);
    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getMargin()).thenReturn(1.0);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(poolUpdateRepository.findPoolUpdateByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", null, null,
        pageable)).thenReturn(new PageImpl<>(List.of(projection)));
    when(poolUpdateRepository.findPoolRegistrationByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", null, null,
        pageable)).thenReturn(new PageImpl<>(List.of(projection)));
    Assertions.assertEquals(1,
        poolLifecycleService.registration(
                "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
                "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", null, null,
                pageable)
            .getTotalItems());
    Assertions.assertEquals(1,
        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
                "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", null, null,
                pageable)
            .getTotalItems());
  }

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

  @Test
  void whenPoolViewIsExistAndTransactionTimeIsBetweenFromDateInToDate_returnResponse() {
    Pageable pageable = PageRequest.of(0, 10);
    Timestamp fromDate = Timestamp.valueOf("2023-01-01 00:00:00");
    Timestamp toDate = Timestamp.valueOf("2023-04-01 00:00:00");
    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getMargin()).thenReturn(1.0);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(poolUpdateRepository.findPoolUpdateByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, fromDate,
        toDate,
        pageable)).thenReturn(
        new PageImpl<>(List.of(projection)));
    when(poolUpdateRepository.findPoolRegistrationByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, fromDate,
        toDate,
        pageable)).thenReturn(new PageImpl<>(List.of(projection)));
    Assertions.assertEquals(1, poolLifecycleService.registration(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", null, fromDate,
        toDate,
        pageable).getTotalItems());
    Assertions.assertEquals(1,
        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
            null, fromDate, toDate, pageable).getTotalItems());
  }

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

  @Test
  void whenPoolViewIsExistAndTxHashIsExistAndTransactionTimeIsBetweenFromDateInToDate_returnResponse() {
    Pageable pageable = PageRequest.of(0, 10);
    Timestamp fromDate = Timestamp.valueOf("2023-01-01 00:00:00");
    Timestamp toDate = Timestamp.valueOf("2023-04-01 00:00:00");
    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getMargin()).thenReturn(1.0);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(poolUpdateRepository.findPoolUpdateByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate,
        toDate,
        pageable)).thenReturn(
        new PageImpl<>(List.of(projection)));
    when(poolUpdateRepository.findPoolRegistrationByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate,
        toDate,
        pageable)).thenReturn(new PageImpl<>(List.of(projection)));
    Assertions.assertEquals(1, poolLifecycleService.registration(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate,
        toDate,
        pageable).getTotalItems());
    Assertions.assertEquals(1,
        poolLifecycleService.poolUpdate("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
            "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e", fromDate, toDate,
            pageable).getTotalItems());
  }

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

  @Test
  void whenPoolViewIsExist_returnRewardResponse() {
    Timestamp time = Timestamp.valueOf("2023-01-01 00:00:00");
    Pageable pageable = PageRequest.of(0, 10);
    LifeCycleRewardProjection projection = Mockito.mock(LifeCycleRewardProjection.class);
    when(projection.getAddress()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    when(projection.getAmount()).thenReturn(BigInteger.TWO);
    when(projection.getEpochNo()).thenReturn(69);
    when(projection.getTime()).thenReturn(time);
    when(rewardRepository.getRewardInfoByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s", pageable)).thenReturn(
        new PageImpl<>(List.of(projection)));
    Assertions.assertEquals(1,
        poolLifecycleService.listReward("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
            pageable).getTotalItems());
    Assertions.assertEquals(1,
        poolLifecycleService.listReward("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s",
            pageable).getData().size());
  }

  @Test
  void whenPoolViewIsExist_returnInfoResponse() {
    PoolInfoProjection projection = Mockito.mock(PoolInfoProjection.class);
    when(projection.getPoolView()).thenReturn(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    when(projection.getId()).thenReturn(69L);
    when(projection.getPoolName()).thenReturn("Test");
    when(projection.getPoolId()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(poolHashRepository.getPoolInfo(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(projection);
    when(poolUpdateRepository.findOwnerAccountByPoolView(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(
        List.of("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    when(rewardRepository.getTotalRewardByPool(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(
        BigInteger.valueOf(10000));
    when(poolRetireRepository.findByPoolView(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")).thenReturn(List.of(69));
    when(fetchRewardDataService.isKoiOs()).thenReturn(false);
    Assertions.assertEquals("Test",
        poolLifecycleService.poolInfo("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")
            .getPoolName());
    Assertions.assertEquals("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda",
        poolLifecycleService.poolInfo("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")
            .getPoolId());
  }

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

  @Test
  void whenPoolViewIsNotExist_returnException() {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    when(poolUpdateRepository.countPoolUpdateByPool(poolView)).thenReturn(0);
    when(poolHashRepository.findByView(poolView)).thenReturn(Optional.empty());
    BusinessException exception = assertThrows(BusinessException.class, () -> {
      poolLifecycleService.poolLifecycleStatus(poolView);
    });
    Assertions.assertEquals(CommonErrorCode.UNKNOWN_ERROR.getServiceErrorCode(), exception.getErrorCode());
  }

  @Test
  void whenPoolViewIsExist_noReward_returnSPOStatusResponse() {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolHash poolHash = Mockito.mock(PoolHash.class);
    when(poolUpdateRepository.countPoolUpdateByPool(poolView)).thenReturn(2);
    when(poolHashRepository.findByView(poolView)).thenReturn(Optional.of(poolHash));
    when(rewardRepository.existsByPoolAndType(poolHash, RewardType.LEADER)).thenReturn(false);
    when(poolRetireRepository.existsByPoolHash(poolHash)).thenReturn(true);
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsRegistration());
    Assertions.assertEquals(false,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsReward());
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsDeRegistration());
  }

  @Test
  void whenPoolViewIsExist_hasReward_returnSPOStatusResponse() {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolHash poolHash = Mockito.mock(PoolHash.class);
    when(poolUpdateRepository.countPoolUpdateByPool(poolView)).thenReturn(2);
    when(poolHashRepository.findByView(poolView)).thenReturn(Optional.of(poolHash));
    when(rewardRepository.existsByPoolAndType(poolHash, RewardType.LEADER)).thenReturn(true);
    when(poolRetireRepository.existsByPoolHash(poolHash)).thenReturn(true);
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsRegistration());
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsReward());
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsDeRegistration());
  }

  @Test
  void whenPoolViewIsExist_noDeRegis_returnSPOStatusResponse() {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolHash poolHash = Mockito.mock(PoolHash.class);
    when(poolUpdateRepository.countPoolUpdateByPool(poolView)).thenReturn(2);
    when(poolHashRepository.findByView(poolView)).thenReturn(Optional.of(poolHash));
    when(rewardRepository.existsByPoolAndType(poolHash, RewardType.LEADER)).thenReturn(true);
    when(poolRetireRepository.existsByPoolHash(poolHash)).thenReturn(false);
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsRegistration());
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsReward());
    Assertions.assertEquals(false,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsDeRegistration());
  }

  @Test
  void whenPoolViewIsExist_hasDeRegis_returnSPOStatusResponse() {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolHash poolHash = Mockito.mock(PoolHash.class);
    when(poolUpdateRepository.countPoolUpdateByPool(poolView)).thenReturn(2);
    when(poolHashRepository.findByView(poolView)).thenReturn(Optional.of(poolHash));
    when(rewardRepository.existsByPoolAndType(poolHash, RewardType.LEADER)).thenReturn(true);
    when(poolRetireRepository.existsByPoolHash(poolHash)).thenReturn(true);
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsRegistration());
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsReward());
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsDeRegistration());
  }

  @Test
  void whenPoolViewIsExist_noUpdate_returnSPOStatusResponse() {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolHash poolHash = Mockito.mock(PoolHash.class);
    when(poolUpdateRepository.countPoolUpdateByPool(poolView)).thenReturn(1);
    when(poolHashRepository.findByView(poolView)).thenReturn(Optional.of(poolHash));
    when(rewardRepository.existsByPoolAndType(poolHash, RewardType.LEADER)).thenReturn(true);
    when(poolRetireRepository.existsByPoolHash(poolHash)).thenReturn(true);
    Assertions.assertEquals(false,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsUpdate());
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsReward());
    Assertions.assertEquals(true,
        poolLifecycleService.poolLifecycleStatus(poolView).getIsDeRegistration());
  }

  @Test
  void testListReward_thenReturnKoiOs() {
    String poolView = "poolView";
    Pageable pageable = PageRequest.of(0, 10);
    List<String> rewards = List.of("reward");

    when(fetchRewardDataService.isKoiOs()).thenReturn(true);
    when(poolUpdateRepository.findRewardAccountByPoolView(poolView)).thenReturn(rewards);
    when(fetchRewardDataService.checkRewardForPool(rewards)).thenReturn(false);
    when(fetchRewardDataService.fetchRewardForPool(rewards)).thenReturn(false);

    var response = poolLifecycleService.listReward(poolView, pageable);
    Assertions.assertEquals(response.getData(), null);
    Assertions.assertEquals(response.getTotalPages(), 0);
    Assertions.assertEquals(response.getTotalItems(), 0);
    Assertions.assertEquals(response.getCurrentPage(), 0);
  }

  @Test
  void testPoolInfo_thenReturnKoiOs() {
    String poolView = "poolView";
    PoolInfoProjection projection = Mockito.mock(PoolInfoProjection.class);
    when(projection.getPoolId()).thenReturn("1");
    when(projection.getPoolName()).thenReturn("name");
    when(projection.getPoolView()).thenReturn("view");
    when(projection.getId()).thenReturn(1L);
    List<String> accounts = List.of("account");

    when(poolHashRepository.getPoolInfo(poolView)).thenReturn(projection);
    when(poolUpdateRepository.findRewardAccountByPoolId(1L)).thenReturn(accounts);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(1));
    when(fetchRewardDataService.isKoiOs()).thenReturn(true);
    when(poolInfoRepository.getActiveStakeByPoolAndEpoch(poolView, 1)).thenReturn(BigInteger.ONE);
    when(fetchRewardDataService.checkRewardForPool(accounts)).thenReturn(false);
    when(fetchRewardDataService.fetchRewardForPool(accounts)).thenReturn(false);
    when(poolUpdateRepository.findOwnerAccountByPoolView(poolView)).thenReturn(List.of("stake"));
    when(rewardRepository.getTotalRewardByPool(poolView)).thenReturn(BigInteger.ONE);
    when(poolRetireRepository.findByPoolView(poolView)).thenReturn(List.of(1));

    var response = poolLifecycleService.poolInfo(poolView);
    Assertions.assertEquals(response.getPoolId(), "1");
    Assertions.assertEquals(response.getPoolName(), "name");
    Assertions.assertEquals(response.getPoolView(), "view");
    Assertions.assertEquals(response.getStatus(), "RETIRING");
    Assertions.assertEquals(response.getPoolSize(), BigInteger.ONE);
    Assertions.assertEquals(response.getRewardAvailable(), BigInteger.ONE);
    Assertions.assertEquals(response.getEpochNo(), 1);
    Assertions.assertEquals(response.getStakeKeys().get(0), "stake");
    Assertions.assertEquals(response.getRewardAccounts().get(0), "account");
  }

  @Test
  void testDeRegistration_thenReturnKoiOs() {
    String poolView = "poolView";
    Pageable pageable = PageRequest.of(0, 10);
    String txHash = "";
    Date fromDate = new Date();
    Date toDate = fromDate;
    PoolInfoProjection projection = Mockito.mock(PoolInfoProjection.class);
    when(projection.getId()).thenReturn(1L);
    when(projection.getPoolId()).thenReturn("1");
    when(projection.getPoolName()).thenReturn("name");
    when(projection.getPoolView()).thenReturn("view");
    PoolDeRegistrationProjection pdrp = Mockito.mock(PoolDeRegistrationProjection.class);
    when(pdrp.getRetiringEpoch()).thenReturn(1);
    when(pdrp.getRefundFlag()).thenReturn(true);
    when(pdrp.getFee()).thenReturn(BigInteger.ONE);
    List<String> accounts = List.of("account");
    EpochRewardProjection erp = Mockito.mock(EpochRewardProjection.class);
    when(erp.getAmount()).thenReturn(BigInteger.ONE);
    when(erp.getEpochNo()).thenReturn(1);

    when(poolHashRepository.getPoolInfo(poolView)).thenReturn(projection);
    when(poolRetireRepository.getPoolDeRegistration(poolView, null, new Timestamp(fromDate.getTime()), new Timestamp(toDate.getTime()), pageable)).thenReturn(new PageImpl<>(List.of(pdrp)));
    when(fetchRewardDataService.isKoiOs()).thenReturn(true);
    when(poolUpdateRepository.findRewardAccountByPoolId(1L)).thenReturn(accounts);
    when(fetchRewardDataService.checkRewardForPool(accounts)).thenReturn(false);
    when(fetchRewardDataService.fetchRewardForPool(accounts)).thenReturn(true);
    when(rewardRepository.getRewardRefundByEpoch(any(), any())).thenReturn(List.of(erp));
    when(poolUpdateRepository.findOwnerAccountByPoolView(poolView)).thenReturn(List.of("stake"));

    var response = poolLifecycleService.deRegistration(poolView, txHash, fromDate, toDate, pageable);
    Assertions.assertEquals(response.getTotalItems(), 1);
    Assertions.assertEquals(response.getTotalPages(), 1);
    Assertions.assertEquals(response.getCurrentPage(), 0);
    Assertions.assertEquals(response.getData().get(0).getPoolId(), "1");
    Assertions.assertEquals(response.getData().get(0).getPoolName(), "name");
    Assertions.assertEquals(response.getData().get(0).getPoolView(), "view");
    Assertions.assertEquals(response.getData().get(0).getStakeKeys(), List.of("stake"));
    Assertions.assertEquals(response.getData().get(0).getTotalFee(), BigInteger.TWO);
    Assertions.assertEquals(response.getData().get(0).getPoolHold(), BigInteger.ONE);
    Assertions.assertEquals(response.getData().get(0).getFee(), BigInteger.ONE);
    Assertions.assertEquals(response.getData().get(0).getRetiringEpoch(), 1);
  }

  @Test
  void testPoolLifecycleStatus_thenReturnKoiOs() {
    String poolView = "poolView";
    List<String> accounts = List.of("account");
    PoolHash pool = PoolHash.builder().build();

    when(poolUpdateRepository.countPoolUpdateByPool(poolView)).thenReturn(1);
    when(poolHashRepository.findByView(poolView)).thenReturn(Optional.of(pool));
    when(fetchRewardDataService.isKoiOs()).thenReturn(true);
    when(poolUpdateRepository.findRewardAccountByPoolView(poolView)).thenReturn(accounts);
    when(fetchRewardDataService.checkRewardForPool(accounts)).thenReturn(false);
    when(fetchRewardDataService.fetchRewardForPool(accounts)).thenReturn(true);
    when(rewardRepository.existsByPoolAndType(pool, RewardType.LEADER)).thenReturn(true);
    when(poolRetireRepository.existsByPoolHash(pool)).thenReturn(true);

    var response = poolLifecycleService.poolLifecycleStatus(poolView);
    Assertions.assertTrue(response.getIsRegistration());
    Assertions.assertTrue(response.getIsDeRegistration());
    Assertions.assertTrue(response.getIsReward());
    Assertions.assertFalse(response.getIsUpdate());
  }

  @Test
  void testListRewardFilter_thenReturn() {
    String poolView = "view";
    Pageable pageable = PageRequest.of(0, 10);
    int beginEpoch = 400;
    int endEpoch = 410;
    List<String> rewards = List.of("account");
    LifeCycleRewardProjection lcrp = Mockito.mock(LifeCycleRewardProjection.class);
    when(lcrp.getAddress()).thenReturn("address");
    when(lcrp.getEpochNo()).thenReturn(1);
    when(lcrp.getAmount()).thenReturn(BigInteger.ONE);

    when(fetchRewardDataService.isKoiOs()).thenReturn(false);
    when(rewardRepository.getRewardInfoByPoolFiler(poolView, beginEpoch, endEpoch, pageable)).thenReturn(new PageImpl<>(List.of(lcrp)));

    var response = poolLifecycleService.listRewardFilter(poolView, beginEpoch, endEpoch, pageable);
    Assertions.assertEquals(response.getData().get(0).getAmount(), BigInteger.ONE);
    Assertions.assertEquals(response.getData().get(0).getEpochNo(), 1);
    Assertions.assertEquals(response.getData().get(0).getRewardAccount(), "address");
    Assertions.assertEquals(response.getTotalPages(), 0);
    Assertions.assertEquals(response.getTotalItems(), 1);
    Assertions.assertEquals(response.getCurrentPage(), 0);
  }

  @Test
  void testListRewardFilter_thenReturnKoiOs() {
    String poolView = "view";
    Pageable pageable = PageRequest.of(0, 10);
    int beginEpoch = 400;
    int endEpoch = 410;
    List<String> rewards = List.of("account");

    when(fetchRewardDataService.isKoiOs()).thenReturn(true);
    when(poolUpdateRepository.findRewardAccountByPoolView(poolView)).thenReturn(rewards);
    when(fetchRewardDataService.checkRewardForPool(rewards)).thenReturn(false);
    when(fetchRewardDataService.fetchRewardForPool(rewards)).thenReturn(false);

    var response = poolLifecycleService.listRewardFilter(poolView, beginEpoch, endEpoch, pageable);
    Assertions.assertEquals(response.getData(), null);
    Assertions.assertEquals(response.getTotalPages(), 0);
    Assertions.assertEquals(response.getTotalItems(), 0);
    Assertions.assertEquals(response.getCurrentPage(), 0);
  }
}
