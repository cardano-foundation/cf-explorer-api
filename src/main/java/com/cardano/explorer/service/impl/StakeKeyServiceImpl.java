package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.stake.StakeTxResponse;
import com.cardano.explorer.model.response.stake.TrxBlockEpochStake;
import com.cardano.explorer.repository.PoolHashRepository;
import com.cardano.explorer.repository.PoolOfflineDataRepository;
import com.cardano.explorer.repository.StakeDeRegistrationRepository;
import com.cardano.explorer.repository.StakeRegistrationRepository;
import com.cardano.explorer.service.StakeKeyService;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.exceptions.enums.CommonErrorCode;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class StakeKeyServiceImpl implements StakeKeyService {

  private final StakeRegistrationRepository stakeRegistrationRepository;

  private final StakeDeRegistrationRepository stakeDeRegistrationRepository;

  private final PoolOfflineDataRepository poolOfflineDataRepository;

  private final PoolHashRepository poolHashRepository;

  @Override
  public BaseFilterResponse<StakeTxResponse> getDataForStakeKeyRegistration(Pageable pageable) {
    Page<TrxBlockEpochStake> trxBlockEpochStakePage = stakeRegistrationRepository.getDataForStakeRegistration(pageable);
    return createStakeKeyResponse(trxBlockEpochStakePage, pageable);
  }

  @Override
  public BaseFilterResponse<StakeTxResponse> getDataForStakeKeyDeRegistration(Pageable pageable) {
    Page<TrxBlockEpochStake> trxBlockEpochStakePage = stakeDeRegistrationRepository.getDataForStakeDeRegistration(pageable);
    return createStakeKeyResponse(trxBlockEpochStakePage, pageable);
  }

  private BaseFilterResponse<StakeTxResponse> createStakeKeyResponse(Page<TrxBlockEpochStake> page, Pageable pageable){
    BaseFilterResponse<StakeTxResponse> response = new BaseFilterResponse<>();
    List<StakeTxResponse> responseList = page.stream().map(StakeTxResponse::new)
            .collect(Collectors.toList());
    List<Long> blockIdList = responseList.stream().map(StakeTxResponse::getBlock).collect(Collectors.toList());
    List<Long> poolIds = poolHashRepository.getListPoolIdIn(blockIdList);
    if(!poolIds.isEmpty()){
      List<PoolOfflineData> listAllPoolOfflineName = poolOfflineDataRepository.findAllByListPool(poolIds);
      // map with key: pool id, value: list pool offline
      Map<Long, List<PoolOfflineData>> mapPoolIdPoolName = listAllPoolOfflineName.stream()
              .collect(Collectors.groupingBy(item -> item.getPool().getId(), Collectors.toList()));
      for(int i = 0; i < responseList.size(); i++){
        if(poolIds.size() > i){
          responseList.get(i).setPoolNames(mapPoolIdPoolName
                  .get(poolIds.get(i))
                  .stream()
                  .map(item -> getNameValueFromJson(item.getJson()))
                  .collect(Collectors.toList()));
        }
      }
    }
    response.setData(responseList);
    response.setTotalItems(page.getTotalElements());
    response.setTotalPages(page.getTotalPages());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  private String getNameValueFromJson(String json) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(json))) {
      throw new BusinessException(CommonErrorCode.UNKNOWN_ERROR);
    }
    JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
    return jsonObject.get("name").getAsString();
  }

}
