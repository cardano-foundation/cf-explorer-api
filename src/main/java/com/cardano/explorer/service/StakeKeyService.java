package com.cardano.explorer.service;

import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.StakeAnalyticResponse;
import com.cardano.explorer.model.response.address.AddressFilterResponse;
import com.cardano.explorer.model.response.address.StakeAddressResponse;
import com.cardano.explorer.model.response.stake.StakeAnalyticBalanceResponse;
import com.cardano.explorer.model.response.stake.StakeAnalyticRewardResponse;
import com.cardano.explorer.model.response.stake.StakeFilterResponse;
import com.cardano.explorer.model.response.stake.StakeTxResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import com.cardano.explorer.projection.StakeDelegationProjection;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.cardano.explorer.projection.StakeInstantaneousRewardsProjection;
import com.cardano.explorer.projection.StakeWithdrawalProjection;
import java.math.BigInteger;
import java.util.List;
import org.springframework.data.domain.Pageable;

public interface StakeKeyService {

  /**
   * Get stake key registration list
   *
   * @param pageable
   * @return StakeTxResponse
   */
  BaseFilterResponse<StakeTxResponse> getDataForStakeKeyRegistration(Pageable pageable);

  /**
   * Get stake key de registration list
   *
   * @param pageable
   * @return StakeTxResponse
   */
  BaseFilterResponse<StakeTxResponse> getDataForStakeKeyDeRegistration(Pageable pageable);

  /**
   * Get detail stake key info
   *
   * @param address address
   * @return stake key info
   */
  StakeAddressResponse getStakeByAddress(String address);

  /**
   * Get detail stake key info
   *
   * @param stakeKey stake key
   * @return stake key info
   */
  StakeAddressResponse getStake(String stakeKey);

  /**
   * Get stake key delegation history
   *
   * @param stakeKey stake address
   * @return stake key delegation history
   */
  BaseFilterResponse<StakeDelegationProjection> getDelegationHistories(String stakeKey, Pageable pageable);

  /**
   * Get stake key stake history
   *
   * @param stakeKey stake address
   * @return stake key stake history
   */
  BaseFilterResponse<StakeHistoryProjection> getStakeHistories(String stakeKey, Pageable pageable);

  /**
   * Get stake key withdrawal history
   *
   * @param stakeKey stake address
   * @return stake key withdrawal history
   */
  BaseFilterResponse<StakeWithdrawalProjection> getWithdrawalHistories(String stakeKey, Pageable pageable);

  /**
   * Get stake key instantaneous rewards
   *
   * @param stakeKey stake address
   * @return stake key instantaneous rewards
   */
  BaseFilterResponse<StakeInstantaneousRewardsProjection> getInstantaneousRewards(String stakeKey, Pageable pageable);


  /**
   * Get top delegators
   *
   * @param pageable page information
   * @return return list active stake address sort by balance
   */
  BaseFilterResponse<StakeFilterResponse> getTopDelegators(Pageable pageable);

  /**
   * Get all address of stake key
   * @param stakeKey stake address
   * @return list address of stake address in this page
   */
  BaseFilterResponse<AddressFilterResponse> getAddresses(String stakeKey, Pageable pageable);

  /**
   * Get live stake, active stake and total stake
   *
   * @return live stake, active stake and total stake
   */
  StakeAnalyticResponse getStakeAnalytics();

  /**
   * Get stake balance analytics
   * @param stakeKey stake address
   * @param type type of analytics (day, week, month, 3month)
   * @return list balance value by stake
   */
  List<StakeAnalyticBalanceResponse> getStakeBalanceAnalytics(String stakeKey, AnalyticType type);

  /**
   * Get stake reward analytics
   * @param stakeKey stake address
   * @return list reward value by stake
   */
  List<StakeAnalyticRewardResponse> getStakeRewardAnalytics(String stakeKey);

  /**
   *
   * @param stakeKey stake address
   * @return min and max balance of stake address
   */
  List<BigInteger> getAddressMinMaxBalance(String stakeKey);

  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeRegistrations(String stakeKey,
      Pageable pageable);

  BaseFilterResponse<StakeDelegationFilterResponse> getStakeDelegations(String stakeKey,
    StakeLifeCycleFilterRequest condition, Pageable pageable);

  StakeDelegationDetailResponse getStakeDelegationDetail(String stakeKey, String hash);

  BaseFilterResponse<StakeRewardResponse> getStakeReward(String stakeKey, Pageable pageable);

  BaseFilterResponse<StakeWithdrawalFilterResponse> getStakeWithdrawals(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable);

  StakeWithdrawalDetailResponse getStakeWithdrawalDetail(String stakeKey, String hash);

  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeDeRegistrations(String stakeKey,
      Pageable pageable);
}
