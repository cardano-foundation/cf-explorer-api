package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.service.PoolLifecycleService;
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

  @Override
  public BaseFilterResponse<String> getPoolViewByStakeKey(String stakeKey, Pageable pageable) {
    BaseFilterResponse<String> res = new BaseFilterResponse<>();
    Page<String> poolViews = stakeAddressRepository.getPoolViewByStakeKey(stakeKey,
        pageable);
    res.setData(poolViews.getContent());
    res.setTotalItems(poolViews.getTotalElements());
    return res;
  }
}
