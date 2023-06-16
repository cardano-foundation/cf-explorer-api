package org.cardanofoundation.explorer.api.service;

import java.util.concurrent.ExecutionException;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.StakeAnalyticResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.StakeAddressResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticBalanceResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeTxResponse;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.api.projection.StakeInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.StakeWithdrawalProjection;
import org.springframework.data.domain.Pageable;

import java.math.BigInteger;
import java.util.List;

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
  List<StakeAnalyticBalanceResponse> getStakeBalanceAnalytics(String stakeKey, AnalyticType type)
      throws ExecutionException, InterruptedException;

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
}
