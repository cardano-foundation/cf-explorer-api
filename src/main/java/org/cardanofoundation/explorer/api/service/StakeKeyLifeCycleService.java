package org.cardanofoundation.explorer.api.service;

import java.util.Date;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.*;
import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;
import org.springframework.data.domain.Pageable;

public interface StakeKeyLifeCycleService {

  /**
   * Check stake key has step in life cycle: registration, delegation, reward, withdrawal, de registration
   * @param stakeKey stake address view
   * @return stake key life cycle step check response
   */
  StakeLifecycleResponse getStakeLifeCycle(String stakeKey);

  /**
   * Get list registration in stake key life cycle
   * @param stakeKey stake address view
   * @param condition condition filter
   * @param pageable page information
   * @return list stake key registration
   */
  BaseFilterResponse<StakeRegistrationFilterResponse> getStakeRegistrations(String stakeKey,
                                                                            StakeLifeCycleFilterRequest condition, Pageable pageable);


  /**
   * Get detail registration in stake key life cycle
   * @param stakeKey stake address view
   * @param hash hash of transaction
   * @return detail registration
   */
  StakeRegistrationDetailResponse getStakeRegistrationDetail(String stakeKey, String hash);

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
   * @param fromDate from date
   * @param toDate to date
   * @param type type of reward
   * @return list stake key reward
   */
  BaseFilterResponse<StakeRewardResponse> getStakeRewards(String stakeKey, Date fromDate,
                                                          Date toDate, RewardType type, Pageable pageable);

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

  /**
   * Get list de registration in stake key life cycle
   * @param stakeKey stake address view
   * @param condition condition filter
   * @param pageable page information
   * @return list stake key de registration
   */
  BaseFilterResponse<StakeRegistrationFilterResponse> getStakeDeRegistrations(String stakeKey,
                                                                              StakeLifeCycleFilterRequest condition, Pageable pageable);

  /**
   * Get detail registration in stake key life cycle
   * @param stakeKey stake address view
   * @param hash hash of transaction
   * @return detail registration
   */
  StakeRegistrationDetailResponse getStakeDeRegistrationDetail(String stakeKey, String hash);

  /**
   * Get list activity in stake key life cycle
   *
   * @param stakeKey stake address view
   * @param pageable page information
   * @return list stake key activity transaction
   */
  BaseFilterResponse<StakeWalletActivityResponse> getStakeWalletActivities(String stakeKey,
      Pageable pageable);

  /**
   * Get list reward activity in stake key life cycle
   * @param stakeKey stake address view
   * @param pageable page information
   * @return list stake key reward activity
   */
  BaseFilterResponse<StakeRewardActivityResponse> getStakeRewardActivities(String stakeKey, Pageable pageable);

  /**
   * Get list activity in stake key life cycle filter by date range
   *
   * @param stakeKey stake address view
   * @param pageable page information
   * @return list stake key activity transaction
   */
  BaseFilterResponse<StakeWalletActivityResponse> getStakeWalletActivitiesByDateRange(
      String stakeKey, StakeLifeCycleFilterRequest condition, Pageable pageable);
}
