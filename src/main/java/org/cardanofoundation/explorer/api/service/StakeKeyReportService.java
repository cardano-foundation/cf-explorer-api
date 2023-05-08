package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.request.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.request.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.report.ReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.report.StakeKeyReportResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWalletActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.springframework.data.domain.Pageable;

/**
 * StakeKeyReportService interface for stake key report
 */
public interface StakeKeyReportService extends StorageService {

  /**
   * Generate stake key report
   *
   * @param stakeKeyReportRequest stake key report
   * @return StakeKeyReportHistoryResponse
   */
  StakeKeyReportHistoryResponse generateStakeKeyReport(StakeKeyReportRequest stakeKeyReportRequest,
      String username);


  /**
   * Get stake key report history
   *
   * @param stakeKey stake key
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistoryByStakeKey(
      String stakeKey, String username,
      Pageable pageable);


  /**
   * Get all stake key report history
   *
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistory(String username,
      Pageable pageable);

  /**
   * Get report history
   *
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<ReportHistoryResponse> getReportHistory(
      ReportHistoryFilterRequest filterRequest, String username, Pageable pageable);

  /**
   * Export stake key report by report id
   *
   * @param reportId report id
   * @return StakeKeyReportResponse
   */
  StakeKeyReportResponse exportStakeKeyReport(Long reportId, String username, String fileExtension);


  /**
   * Get stake key report history by report id
   *
   * @param reportId report id
   * @return StakeKeyReportHistoryResponse
   */
  StakeKeyReportHistoryResponse getStakeKeyReportHistoryByReportId(Long reportId, String username);

  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeRegistrationsByReportId(Long reportId,
      String username,
      Pageable pageable);


  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeDeRegistrationsByReportId(Long reportId,
      String username,
      Pageable pageable);

  BaseFilterResponse<StakeDelegationFilterResponse> getStakeDelegationsByReportId(Long reportId,
      String username,
      Pageable pageable);

  BaseFilterResponse<StakeRewardResponse> getStakeRewardsByReportId(Long reportId, String username,
      Pageable pageable);

  BaseFilterResponse<StakeWithdrawalFilterResponse> getStakeWithdrawalsByReportId(Long reportId,
      String username,
      Pageable pageable);

  BaseFilterResponse<StakeWalletActivityResponse> getWalletActivitiesByReportId(Long reportId,
      String username,
      Pageable pageable);

  BaseFilterResponse<StakeRewardActivityResponse> getRewardActivitiesByReportId(Long reportId,
      String username,
      Pageable pageable);
}
