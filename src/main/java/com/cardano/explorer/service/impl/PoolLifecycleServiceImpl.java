package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationResponse;
import com.cardano.explorer.model.response.pool.projection.PoolInfoProjection;
import com.cardano.explorer.model.response.pool.projection.PoolRegistrationProjection;
import com.cardano.explorer.model.response.pool.projection.StakeKeyProjection;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.service.PoolLifecycleService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class PoolLifecycleServiceImpl implements PoolLifecycleService {

  private final StakeAddressRepository stakeAddressRepository;

  private final PoolHashRepository poolHashRepository;

  private final PoolUpdateRepository poolUpdateRepository;

  @Override
  public BaseFilterResponse<String> getPoolViewByStakeKey(String stakeKey, Pageable pageable) {
    BaseFilterResponse<String> res = new BaseFilterResponse<>();
    Page<String> poolViews = stakeAddressRepository.getPoolViewByStakeKey(stakeKey,
        pageable);
    res.setData(poolViews.getContent());
    res.setTotalItems(poolViews.getTotalElements());
    return res;
  }

  @Override
  public RegistrationAllResponse registration(String poolView) {
    RegistrationAllResponse res = new RegistrationAllResponse();
    PoolInfoProjection poolInfo = poolHashRepository.getPoolInfo(poolView);
    if (Objects.nonNull(poolInfo)) {
      res.setPoolId(poolInfo.getPoolId());
      res.setPoolName(poolInfo.getPoolName());
      res.setPoolView(poolInfo.getPoolView());
    }
    List<PoolRegistrationProjection> poolRegistrations = poolHashRepository.getPoolRegistration(
        poolView);
    List<RegistrationResponse> regisList = new ArrayList<>();
    Set<Long> poolUpdateIds = new HashSet<>();
    if (Objects.nonNull(poolRegistrations)) {
      poolRegistrations.forEach(poolRegis -> {
        poolUpdateIds.add(poolRegis.getPoolUpdateId());
        RegistrationResponse regis = new RegistrationResponse(poolRegis);
        regisList.add(regis);
      });
    }
    List<StakeKeyProjection> stakeKeyProjections = poolUpdateRepository.findOwnerAccountByPoolUpdate(
        poolUpdateIds);
    Map<Long, List<StakeKeyProjection>> stakeKeyProjectionMap = stakeKeyProjections.stream()
        .collect(Collectors.groupingBy(StakeKeyProjection::getPoolUpdateId));
    Map<Long, List<String>> stakeKeyStrMap = new HashMap<>();
    stakeKeyProjectionMap.forEach((k, v) -> stakeKeyStrMap.put(k,
        v.stream().map(StakeKeyProjection::getView).collect(Collectors.toList())));
    regisList.forEach(regisRes -> {
      regisRes.setStakeKeys(stakeKeyStrMap.get(regisRes.getPoolUpdateId()));
    });
    res.setRegistrations(regisList);
    return res;
  }
}
