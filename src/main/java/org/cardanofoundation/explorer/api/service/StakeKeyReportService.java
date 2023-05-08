package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.request.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.report.StakeKeyReportResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWalletActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.cardanofoundation.explorer.consumercommon.entity.StakeKeyReportHistory;

import org.springframework.data.domain.Pageable;

/**
 * StakeKeyReportService interface for stake key report
 */
public interface StakeKeyReportService extends StorageService {

  /**
   * Generate stake key report of current user
   *
   * @param stakeKeyReportRequest request
   * @param username              username of current request user
   * @return StakeKeyReportHistoryResponse
   */
  StakeKeyReportHistoryResponse generateStakeKeyReport(StakeKeyReportRequest stakeKeyReportRequest,
                                                       String username);


  /**
   * Get stake key report history by username and stake key
   *
   * @param stakeKey stake key
   * @param username username
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistoryByStakeKey(
      String stakeKey, String username,
      Pageable pageable);


  /**
   * Get all stake key report history by username
   *
   * @param username username
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistory(String username,
                                                                             Pageable pageable);

  /**
   * Download the export file of stake key report from storage of current user
   *
   * @param reportId report id
   * @return StakeKeyReportResponse
   */
  StakeKeyReportResponse exportStakeKeyReport(Long reportId, String username, String fileExtension);


  /**
   * Generate csv and Excel file for stake key report, then push them to storage
   *
   * @param stakeKeyReportHistory stake key report history
   */
  void exportStakeKeyReport(StakeKeyReportHistory stakeKeyReportHistory);

  /**
   * Get stake key report history by report id
   *
   * @param reportId report id
   * @param username username
   * @return StakeKeyReportHistoryResponse
   */
  StakeKeyReportHistoryResponse getStakeKeyReportHistoryByReportId(Long reportId, String username);

  /**
   * Get stake registrations by report id
   *
   * @param reportId report id
   * @param username username
   * @param pageable pageable
   * @return BaseFilterResponse<StakeRegistrationLifeCycle>
   */
  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeRegistrationsByReportId(Long reportId,
                                                                                 String username,
                                                                                 Pageable pageable);

  /**
   * Get stake de-registrations by report id
   *
   * @param reportId report id
   * @param username username
   * @param pageable pageable
   * @return BaseFilterResponse<StakeRegistrationLifeCycle>
   */
  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeDeRegistrationsByReportId(Long reportId,
                                                                                   String username,
                                                                                   Pageable pageable);

  /**
   * Get stake delegations by report id
   *
   * @param reportId report id
   * @param username username
   * @param pageable pageable
   * @return BaseFilterResponse<StakeDelegationFilterResponse>
   */
  BaseFilterResponse<StakeDelegationFilterResponse> getStakeDelegationsByReportId(Long reportId,
                                                                                  String username,
                                                                                  Pageable pageable);

  /**
   * Get stake rewards by report id
   *
   * @param reportId report id
   * @param username username
   * @param pageable pageable
   * @return BaseFilterResponse<StakeRewardResponse>
   */
  BaseFilterResponse<StakeRewardResponse> getStakeRewardsByReportId(Long reportId, String username,
                                                                    Pageable pageable);

  /**
   * Get stake withdrawals by report id
   *
   * @param reportId report id
   * @param username username
   * @param pageable pageable
   * @return BaseFilterResponse<StakeWithdrawalFilterResponse>
   */
  BaseFilterResponse<StakeWithdrawalFilterResponse> getStakeWithdrawalsByReportId(Long reportId,
                                                                                  String username,
                                                                                  Pageable pageable);

  /**
   * Get stake wallet activities by report id
   *
   * @param reportId report id
   * @param username username
   * @param pageable pageable
   * @return BaseFilterResponse<StakeWalletActivityResponse>
   */
  BaseFilterResponse<StakeWalletActivityResponse> getWalletActivitiesByReportId(Long reportId,
                                                                                String username,
                                                                                Pageable pageable);

  /**
   * Get stake reward activities by report id
   *
   * @param reportId report id
   * @param username username
   * @param pageable pageable
   * @return BaseFilterResponse<StakeRewardActivityResponse>
   */
  BaseFilterResponse<StakeRewardActivityResponse> getRewardActivitiesByReportId(Long reportId,
                                                                                String username,
                                                                                Pageable pageable);
}
