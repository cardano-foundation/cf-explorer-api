package org.cardanofoundation.explorer.api.service.impl;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.DelegationResponse;
import org.cardanofoundation.explorer.api.model.response.PoolDetailDelegatorResponse;
import org.cardanofoundation.explorer.api.model.response.address.DelegationPoolResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.model.response.pool.DelegationHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailEpochResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolResponse;
import org.cardanofoundation.explorer.api.model.response.pool.chart.PoolDetailAnalyticsResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCountProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailDelegatorProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.projection.DelegationProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.explorer.AggregatePoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.service.DelegationService;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity_;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation_;
import org.cardanofoundation.explorer.consumercommon.entity.PoolOfflineData_;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.AggregatePoolInfo;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class DelegationServiceImpl implements DelegationService {

  private final DelegationRepository delegationRepository;

  private final BlockRepository blockRepository;

  private final EpochRepository epochRepository;

  private final PoolHashRepository poolHashRepository;

  private final PoolUpdateRepository poolUpdateRepository;

  private final RedisTemplate<String, Object> redisTemplate;

  private final TxRepository txRepository;

  private final EpochService epochService;

  private final AggregatePoolInfoRepository aggregatePoolInfoRepository;

  private static final int MAX_TOTAL_ELEMENTS = 1000;

  @Value("${spring.data.web.pageable.default-page-size}")
  private int defaultSize;

  @Value("${application.network}")
  private String network;

  @Override
  public BaseFilterResponse<DelegationResponse> getDelegations(Pageable pageable) {
    Page<Long> txIdPage = delegationRepository.findAllDelegations(pageable);
    List<TxIOProjection> txs = txRepository.findTxIn(txIdPage.getContent());
    Map<Long, TxIOProjection> txMap
        = txs.stream().collect(Collectors.toMap(TxIOProjection::getId, Function.identity()));
    List<DelegationProjection> delegations = delegationRepository.findDelegationByTxIdIn(
        txIdPage.getContent());
    Map<Long, List<String>> delegationStakeKeyMap = delegations.stream()
        .collect(Collectors.groupingBy(DelegationProjection::getTxId,
            Collectors.mapping(DelegationProjection::getStakeAddress, Collectors.toList())));
    Map<Long, Set<DelegationPoolResponse>> delegationPoolMap = delegations.stream()
        .collect(Collectors.groupingBy(DelegationProjection::getTxId,
            Collectors.mapping(item ->
                DelegationPoolResponse.builder()
                    .poolName(item.getPoolName())
                    .tickerName(item.getTickerName())
                    .poolId(item.getPoolView())
                    .build(), Collectors.toSet())));
    List<DelegationResponse> responses = txIdPage.stream().map(
        item -> DelegationResponse.builder()
            .txHash(txMap.get(item).getHash())
            .blockNo(txMap.get(item).getBlockNo())
            .epochNo(txMap.get(item).getEpochNo())
            .epochSlotNo(txMap.get(item).getEpochSlotNo())
            .time(txMap.get(item).getTime())
            .epochSlotNo(txMap.get(item).getEpochSlotNo())
            .slotNo(txMap.get(item).getSlot())
            .stakeKeys(delegationStakeKeyMap.get(item))
            .pools(delegationPoolMap.get(item).stream().toList())
            .build()
    ).toList();
    return new BaseFilterResponse<>(txIdPage, responses);
  }

  @Override
  public DelegationHeaderResponse getDataForDelegationHeader() {
    EpochSummary epoch = epochService.getCurrentEpochSummary();
    Integer epochNo = epoch.getNo();
    Object poolActiveObj = redisTemplate.opsForValue()
        .get(CommonConstant.REDIS_POOL_ACTIVATE + network);
    Object poolInActiveObj = redisTemplate.opsForValue()
        .get(CommonConstant.REDIS_POOL_INACTIVATE + network);
    LocalDateTime endTime = epoch.getEndTime();
    Integer slot = epoch.getSlot();
    long countDownTime
        = Timestamp.valueOf(endTime).getTime() - Timestamp.valueOf(LocalDateTime.now(ZoneOffset.UTC)).getTime();
    Object delegatorCached = redisTemplate.opsForValue()
        .get(CommonConstant.REDIS_TOTAL_DELEGATOR + network);
    Integer delegators =
        Objects.nonNull(delegatorCached) ? Integer.parseInt(String.valueOf(delegatorCached)) : CommonConstant.ZERO;
    return DelegationHeaderResponse.builder().epochNo(epochNo).epochSlotNo(slot)
        .liveStake(null).delegators(delegators)
        .activePools(Objects.nonNull(poolActiveObj) ? (Integer) poolActiveObj : CommonConstant.ZERO)
        .retiredPools(
            Objects.nonNull(poolInActiveObj) ? (Integer) poolInActiveObj : CommonConstant.ZERO)
        .countDownEndTime(countDownTime > CommonConstant.ZERO ? countDownTime : CommonConstant.ZERO)
        .build();
  }

  @Override
  public BaseFilterResponse<PoolResponse> getDataForPoolTable(Pageable pageable, String search,
                                                              boolean showRetired) {
    BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>();
    Set<Long> poolRetiredIds = new HashSet<>();
    if (!showRetired) {
      String poolRetiredIdKey = CommonConstant.POOL_IDS_INACTIVATE + network;
      poolRetiredIds = redisTemplate.opsForHash().values(poolRetiredIdKey).stream()
          .map(item -> Long.parseLong(String.valueOf(item))).collect(Collectors.toSet());
    }
    // add -1L to poolRetiredIds to avoid empty list
    poolRetiredIds.add(-1L);
    boolean isQueryEmpty = DataUtil.isNullOrEmpty(search);
    if (isQueryEmpty) {
      pageable = createPageableWithSort(pageable, Sort.by(Sort.Direction.ASC, BaseEntity_.ID));
    } else {
      search = search.toLowerCase();
      String poolNameLength = "poolNameLength";
      if (MAX_TOTAL_ELEMENTS / pageable.getPageSize() <= pageable.getPageNumber()) {
        throw new BusinessException(BusinessCode.OUT_OF_QUERY_LIMIT);
      }
      pageable = createPageableWithSort(pageable, Sort.by(Sort.Direction.ASC, poolNameLength,
                                                          PoolOfflineData_.POOL_NAME));
    }
    Page<PoolResponse> poolResponse = getPoolResponseList(search, pageable, poolRetiredIds);
    response.setData(poolResponse.getContent());
    response.setTotalItems(poolResponse.getTotalElements());
    response.setTotalPages(poolResponse.getTotalPages());
    response.setCurrentPage(pageable.getPageNumber());
    if (!isQueryEmpty) {
      response.setIsDataOverSize(poolResponse.getTotalElements() >= 1000);
    }
    return response;
  }

  private Page<PoolResponse> getPoolResponseList(String queryParam, Pageable pageable,
                                                 Set<Long> retiredIds) {
    List<String> sortProperties = pageable.getSort().stream().map(Sort.Order::getProperty).toList();
    boolean isSortedOnAggTable = sortProperties.contains("numberDelegators")
        || sortProperties.contains("epochBlock")
        || sortProperties.contains("lifetimeBlock");
    boolean isQueryEmpty = DataUtil.isNullOrEmpty(queryParam);
    Page<PoolListProjection> poolInfoProjections;
    List<PoolResponse> poolResponseList;
    if (!isSortedOnAggTable) {
      if (isQueryEmpty) {
        poolInfoProjections = poolHashRepository
            .findAllWithoutQueryParam(retiredIds, pageable);
      } else {
        poolInfoProjections = poolHashRepository
            .findAllByPoolViewOrPoolNameOrPoolHash(queryParam, retiredIds, pageable);
      }
      poolResponseList = mapAggPoolInfoToPoolResponse(retiredIds, poolInfoProjections.getContent());
    } else {
      if (isQueryEmpty) {
        poolInfoProjections = aggregatePoolInfoRepository.findAllByPoolIdNotIn(retiredIds,
                                                                               pageable);
        poolResponseList = mapPoolInfoForPoolResponse(retiredIds,
                                                      poolInfoProjections.getContent());
      } else {
        return getAllByQueryAndSortByAggField(pageable, queryParam, retiredIds);
      }
    }

    return new PageImpl<>(poolResponseList, pageable, poolInfoProjections.getTotalElements());
  }

  private Page<PoolResponse> getAllByQueryAndSortByAggField(Pageable pageable, String queryParam,
                                                            Set<Long> retiredIds) {
    List<String> sortProperties = pageable.getSort().stream().map(Sort.Order::getProperty).toList();
    List<Direction> sortDirections = pageable.getSort().stream().map(Sort.Order::getDirection)
        .toList();
    List<PoolListProjection> poolListProjections = poolHashRepository
        .findAllByPoolViewOrPoolNameOrPoolHash(queryParam, retiredIds);

    List<PoolResponse> poolResponseList = mapAggPoolInfoToPoolResponse(retiredIds,
                                                                       poolListProjections);
    if (sortProperties.contains("numberDelegators")) {
      poolResponseList.sort(Comparator.comparing(PoolResponse::getNumberDelegators)
                                .thenComparing(PoolResponse::getPoolId));
    } else if (sortProperties.contains("epochBlock")) {
      poolResponseList.sort(Comparator.comparing(PoolResponse::getEpochBlock)
                                .thenComparing(PoolResponse::getPoolId));
    } else if (sortProperties.contains("lifetimeBlock")) {
      poolResponseList.sort(Comparator.comparing(PoolResponse::getLifetimeBlock)
                                .thenComparing(PoolResponse::getPoolId));
    }

    if (sortDirections.get(0).equals(Direction.DESC)) {
      Collections.reverse(poolResponseList);
    }

    if (poolResponseList.size() > MAX_TOTAL_ELEMENTS) {
      poolResponseList = poolResponseList.subList(0, MAX_TOTAL_ELEMENTS);
    }

    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), poolResponseList.size());
    if(start > poolResponseList.size()) {
      return Page.empty();
    }
    return new PageImpl<>(poolResponseList.subList(start, end), pageable, poolResponseList.size());
  }

  /**
   * Map aggregate pool info for pool response
   *
   * @param retiredIds          set of retired pool ids
   * @param poolListProjections
   * @return
   */
  private List<PoolResponse> mapAggPoolInfoToPoolResponse(Set<Long> retiredIds,
                                                          List<PoolListProjection> poolListProjections) {
    List<Long> poolIds = poolListProjections.stream().map(PoolListProjection::getPoolId).toList();
    Map<Long, AggregatePoolInfo> aggPoolInfoMap = aggregatePoolInfoRepository
        .getAllByPoolIdIn(poolIds)
        .stream()
        .collect(Collectors.toMap(AggregatePoolInfo::getPoolId, Function.identity()));

    return poolListProjections.stream().map(projection -> {
      AggregatePoolInfo aggPoolInfo = aggPoolInfoMap.get(projection.getPoolId());
      return PoolResponse.builder()
          .poolId(projection.getPoolView())
          .id(projection.getPoolId())
          .poolName(projection.getPoolName())
          .tickerName(projection.getTickerName())
          .pledge(projection.getPledge())
          .feeAmount(projection.getFee())
          .feePercent(projection.getMargin())
          .poolSize(projection.getPoolSize())
          .saturation(projection.getSaturation())
          .numberDelegators(aggPoolInfo.getDelegatorCount())
          .lifetimeBlock(aggPoolInfo.getBlockLifeTime())
          .epochBlock(aggPoolInfo.getBlockInEpoch())
          .retired(retiredIds.contains(projection.getPoolId()))
          .build();
    }).toList();
  }

  private List<PoolResponse> mapPoolInfoForPoolResponse(Set<Long> retiredIds,
                                                        List<PoolListProjection> poolListProjections) {
    List<PoolResponse> poolResponseList;
    List<Long> poolIds = poolListProjections.stream().map(PoolListProjection::getPoolId).toList();
    Map<Long, PoolListProjection> poolListProjectionMap = poolHashRepository
        .findAllByPoolIdIn(poolIds)
        .stream()
        .collect(Collectors.toMap(PoolListProjection::getPoolId, Function.identity()));

    poolResponseList = poolListProjections.stream().map(pool -> {
      PoolListProjection poolListProjection = poolListProjectionMap.get(pool.getPoolId());
      return PoolResponse.builder()
          .poolId(poolListProjection.getPoolView())
          .id(poolListProjection.getPoolId())
          .poolName(poolListProjection.getPoolName())
          .tickerName(poolListProjection.getTickerName())
          .pledge(poolListProjection.getPledge())
          .feeAmount(poolListProjection.getFee())
          .feePercent(poolListProjection.getMargin())
          .poolSize(poolListProjection.getPoolSize())
          .saturation(poolListProjection.getSaturation())
          .numberDelegators(pool.getNumberDelegators())
          .lifetimeBlock(pool.getLifetimeBlock())
          .epochBlock(pool.getEpochBlock())
          .retired(retiredIds.contains(pool.getPoolId()))
          .build();
    }).toList();
    return poolResponseList;
  }

  /**
   * Create pageable with sort, if sort is unsorted then use default sort
   * @param pageable page information
   * @param defaultSort default sort condition
   * @return pageable with sort
   */
  private Pageable createPageableWithSort(Pageable pageable, Sort defaultSort) {
    Sort sort = pageable.getSort();
    if (sort.isUnsorted()) {
      sort = defaultSort;
    }
    if (Objects.isNull(sort.getOrderFor(BaseEntity_.ID))) {
      sort = sort.and(Sort.by(Sort.Direction.ASC, BaseEntity_.ID));
    }
    pageable = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize(), sort);
    return pageable;
  }

  @Override
  public List<PoolResponse> findTopDelegationPool(Pageable pageable) {
    int size = pageable.getPageSize();
    if (size > defaultSize) {
      pageable = PageRequest.of(pageable.getPageNumber(), defaultSize);
    }

    List<PoolResponse> response;

    Integer currentEpoch = epochRepository.findCurrentEpochNo()
        .orElseThrow(() -> new NoContentException(CommonErrorCode.UNKNOWN_ERROR));

    List<PoolCountProjection> poolCountProjections = blockRepository.findTopDelegationByEpochBlock(
        currentEpoch, pageable);
    Set<String> poolViewsTop = poolCountProjections.stream().map(PoolCountProjection::getPoolView)
        .collect(Collectors.toSet());
    Map<Long, Integer> blockEpochsMap = poolCountProjections.stream().collect(
        Collectors.toMap(PoolCountProjection::getPoolId, PoolCountProjection::getCountValue));

    Set<Long> poolIds = poolHashRepository.getListPoolIdIn(poolViewsTop);
    List<PoolDelegationSummaryProjection> pools = delegationRepository.findDelegationPoolsSummary(
        poolIds);
    List<PoolCountProjection> blockLifetimeProjections = blockRepository.getCountBlockByPools(
        poolIds);
    Map<Long, Integer> blockLifetimesMap = blockLifetimeProjections.stream().collect(
        Collectors.toMap(PoolCountProjection::getPoolId,
            PoolCountProjection::getCountValue));
    response = pools.stream().map(pool ->
        PoolResponse.builder().poolId(pool.getPoolView()).poolName(pool.getPoolName())
            .feeAmount(pool.getFee())
            .feePercent(pool.getMargin()).pledge(pool.getPledge())
            .id(pool.getPoolId())
            .epochBlock(blockEpochsMap.get(pool.getPoolId()))
            .lifetimeBlock(blockLifetimesMap.get(pool.getPoolId()))
            .build()
    ).toList();

    return response.stream().sorted(Comparator.comparing(PoolResponse::getEpochBlock).reversed())
        .toList();
  }

  @Override
  public PoolDetailHeaderResponse getDataForPoolDetail(String poolViewOrHash) {
    Integer currentEpoch = epochRepository.findCurrentEpochNo().orElse(CommonConstant.ZERO);

    PoolDetailUpdateProjection projection = poolHashRepository.getDataForPoolDetail(
        poolViewOrHash, currentEpoch);
    Long poolId = projection.getPoolId();
    AggregatePoolInfo aggregatePoolInfo = aggregatePoolInfoRepository.findByPoolId(poolId);
    PoolDetailHeaderResponse poolDetailResponse =
        new PoolDetailHeaderResponse(projection, aggregatePoolInfo);
    Gson gson = new Gson();
    JsonObject jsonObject = gson
        .fromJson(projection.getJson(), JsonObject.class);
    if (Objects.nonNull(jsonObject) && jsonObject.has("homepage")) {
      poolDetailResponse.setHomepage(jsonObject.get("homepage").getAsString());
    }
    if (Objects.nonNull(jsonObject) && jsonObject.has("description")) {
      poolDetailResponse.setDescription(jsonObject.get("description").getAsString());
    }
    poolDetailResponse.setStakeLimit(null);
    poolDetailResponse.setCreateDate(poolUpdateRepository.getCreatedTimeOfPool(poolId));

    List<String> ownerAddress = poolUpdateRepository.findOwnerAccountByPool(poolId);
    Collections.sort(ownerAddress);

    poolDetailResponse.setOwnerAccounts(ownerAddress);
    poolDetailResponse.setPoolSize(null);
    poolDetailResponse.setSaturation(null);

    poolDetailResponse.setTotalBalanceOfPoolOwners(null);

    return poolDetailResponse;
  }

  @Override
  public BaseFilterResponse<PoolDetailEpochResponse> getEpochListForPoolDetail(Pageable pageable,
      String poolViewOrHash) {
    BaseFilterResponse<PoolDetailEpochResponse> epochRes = new BaseFilterResponse<>();
    epochRes.setData(null);
    epochRes.setTotalItems(0);
    epochRes.setTotalPages(0);
    epochRes.setCurrentPage(pageable.getPageNumber());
    return epochRes;
  }

  @Override
  public PoolDetailAnalyticsResponse getAnalyticsForPoolDetail(String findByViewOrHashRaw) {
    return PoolDetailAnalyticsResponse.builder().epochChart(null)
        .delegatorChart(null).build();
  }

  @Override
  public BaseFilterResponse<PoolDetailDelegatorResponse> getDelegatorsForPoolDetail(
      Pageable pageable, String poolViewOrHash) {
    if(pageable.getSort().isUnsorted()) {
      pageable = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize(),
                                Sort.by(Sort.Direction.DESC, Delegation_.TX_ID));
    }
    BaseFilterResponse<PoolDetailDelegatorResponse> delegatorResponse = new BaseFilterResponse<>();
    Page<Long> addressIdPage = delegationRepository.liveDelegatorsList(poolViewOrHash, pageable);
    if (!addressIdPage.isEmpty()) {
      Set<Long> addressIds = addressIdPage.stream().collect(Collectors.toSet());
      List<PoolDetailDelegatorProjection> delegatorPage = delegationRepository.getDelegatorsByAddress(
          addressIds);
      List<PoolDetailDelegatorResponse> delegatorList = delegatorPage.stream()
          .map(PoolDetailDelegatorResponse::new).toList();
      delegatorResponse.setTotalItems(addressIdPage.getTotalElements());
      delegatorResponse.setData(delegatorList);
      delegatorResponse.setTotalPages(addressIdPage.getTotalPages());
      delegatorResponse.setCurrentPage(pageable.getPageNumber());
    } else {
      delegatorResponse.setData(new ArrayList<>());
    }
    return delegatorResponse;
  }
}
