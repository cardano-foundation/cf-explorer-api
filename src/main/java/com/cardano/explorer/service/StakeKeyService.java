package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.address.StakeAddressResponse;
import com.cardano.explorer.model.response.stake.StakeTxResponse;
import com.cardano.explorer.projection.StakeDelegationProjection;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.cardano.explorer.projection.StakeTreasuryProjection;
import com.cardano.explorer.projection.StakeWithdrawalProjection;
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
  BaseFilterResponse<StakeTreasuryProjection> getInstantaneousRewards(String stakeKey, Pageable pageable);
}
