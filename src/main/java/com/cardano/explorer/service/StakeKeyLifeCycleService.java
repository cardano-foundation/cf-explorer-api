package com.cardano.explorer.service;

import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.springframework.data.domain.Pageable;

public interface StakeKeyLifeCycleService {

  /**
   * Get list registration in stake key life cycle
   * @param stakeKey stake address view
   * @param condition condition filter
   * @param pageable page information
   * @return list stake key registration
   */
  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeRegistrations(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable);

  /**
   * Get list de registration in stake key life cycle
   * @param stakeKey stake address view
   * @param condition condition filter
   * @param pageable page information
   * @return list stake key de registration
   */
  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeDeRegistrations(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable);

  /**
   * Get list delegation in stake key life cycle
   * @param stakeKey stake address view
   * @param condition condition filter
   * @param pageable page information
   * @return list stake key delegation
   */
  BaseFilterResponse<StakeDelegationFilterResponse> getStakeDelegations(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable);

  /**
   * Get detail delegation in stake key life cycle
   * @param stakeKey stake address view
   * @param hash hash of transaction
   * @return detail delegation
   */
  StakeDelegationDetailResponse getStakeDelegationDetail(String stakeKey, String hash);

  /**
   * Get list reward in stake key life cycle
   * @param stakeKey stake address view
   * @param pageable page information
   * @return list stake key reward
   */
  BaseFilterResponse<StakeRewardResponse> getStakeRewards(String stakeKey, Pageable pageable);

  /**
   * Get list withdrawal in stake key life cycle
   * @param stakeKey stake address view
   * @param condition condition filter
   * @param pageable page information
   * @return list stake key withdrawal
   */
  BaseFilterResponse<StakeWithdrawalFilterResponse> getStakeWithdrawals(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable);

  /**
   * Get detail withdrawal in stake key life cycle
   * @param stakeKey stake address view
   * @param hash hash of transaction
   * @return withdrawal detail
   */
  StakeWithdrawalDetailResponse getStakeWithdrawalDetail(String stakeKey, String hash);
}
