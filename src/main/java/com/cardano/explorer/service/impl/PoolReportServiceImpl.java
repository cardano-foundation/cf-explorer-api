package com.cardano.explorer.service.impl;

import com.cardano.explorer.model.request.pool.report.PoolReportCreateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.projection.PoolReportProjection;
import com.cardano.explorer.model.response.pool.report.PoolReportDetailResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportListResponse;
import com.cardano.explorer.repository.*;
import com.cardano.explorer.service.PoolReportService;
import com.google.gson.Gson;
import com.sotatek.cardano.common.entity.PoolReport;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class PoolReportServiceImpl implements PoolReportService {

    private final PoolReportRepository poolReportRepository;

    private final EpochStakeRepository epochStakeRepository;

    private final PoolUpdateRepository poolUpdateRepository;

    private final RewardRepository rewardRepository;

    private final PoolRetireRepository poolRetireRepository;

    @Override
    public Boolean create(PoolReportCreateRequest poolReportCreateRequest) {
        try {
            //TODO check not null for fields
            //TODO check if duplicate report?
            poolReportRepository.save(poolReportCreateRequest.toEntity());
            return true;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return false;
        }
    }

    @Override
    public BaseFilterResponse<PoolReportListResponse> list(Pageable pageable) {
        try {
            //FIXME replace: find by username
            List<PoolReport> poolReports = poolReportRepository.findAll(pageable).getContent();
            List<PoolReportListResponse>  poolReportListResponses = poolReports.stream().map(PoolReportListResponse::toDomain).collect(Collectors.toList());
            return new BaseFilterResponse<>(poolReportListResponses, poolReportListResponses.size());
        } catch(Exception e) {
            log.error(e.getMessage(), e);
            return null;
        }
    }

    @Override
    public PoolReportDetailResponse detail(String reportId, Pageable pageable) {
        try {
            //FIXME replace: find by username
            PoolReport poolReport = poolReportRepository.findById(Long.parseLong(reportId)).get();
            /// epoch size
            List<PoolReportProjection> epochSizeProjections = epochStakeRepository.getEpochSizeByPoolReport(poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch(), pageable).getContent();
            List<PoolReportDetailResponse.EpochSize> epochSizes = epochSizeProjections.stream().map(PoolReportDetailResponse.EpochSize::toDomain).collect(Collectors.toList());
            BaseFilterResponse<PoolReportDetailResponse.EpochSize> epochSizeBaseFilterResponse =
                    new BaseFilterResponse<>(epochSizes, epochSizes.size());
            /// pool registrations
            List<PoolReportProjection> poolRegistrationsProjections = poolUpdateRepository.getPoolRegistrationByPoolReport(poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch());
            List<PoolReportDetailResponse.PoolRegistration> poolRegistrations = poolRegistrationsProjections.stream().map(PoolReportDetailResponse.PoolRegistration::toDomain).collect(Collectors.toList());
            BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> poolRegistrationBaseFilterResponse = new BaseFilterResponse<>(poolRegistrations, poolRegistrations.size());
            // pool update
            List<PoolReportDetailResponse.PoolUpdate> poolUpdates = poolRegistrationsProjections.stream().map(PoolReportDetailResponse.PoolUpdate::toDomain).collect(Collectors.toList());
            BaseFilterResponse<PoolReportDetailResponse.PoolUpdate> poolUpdateBaseFilterResponse = new BaseFilterResponse<>(poolUpdates, poolUpdates.size());
            // reward distribution
            List<PoolReportProjection> rewardDistributionProjections = rewardRepository.getRewardDistributionByPoolReport(poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch());
            List<PoolReportDetailResponse.RewardDistribution> rewardDistributions = rewardDistributionProjections.stream().map(PoolReportDetailResponse.RewardDistribution::toDomain).collect(Collectors.toList());
            BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> rewardDistributionBaseFilterResponse = new BaseFilterResponse<>(rewardDistributions, rewardDistributions.size());
            // deregistration
            List<PoolReportProjection> deregistrationProjections = poolRetireRepository.getDeregistrationByPoolReport(poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch());
            List<PoolReportDetailResponse.Deregistration> deregistrations = deregistrationProjections.stream().map(PoolReportDetailResponse.Deregistration::toDomain).collect(Collectors.toList());
            BaseFilterResponse<PoolReportDetailResponse.Deregistration> deregistrationBaseFilterResponse = new BaseFilterResponse<>(deregistrations, deregistrations.size());

            PoolReportDetailResponse poolReportDetailResponse = new PoolReportDetailResponse();
            poolReportDetailResponse.setEpochSizes(epochSizeBaseFilterResponse);
            poolReportDetailResponse.setPoolRegistrations(poolRegistrationBaseFilterResponse);
            poolReportDetailResponse.setPoolUpdates(poolUpdateBaseFilterResponse);
            poolReportDetailResponse.setRewardDistributions(rewardDistributionBaseFilterResponse);
            poolReportDetailResponse.setDeregistrations(deregistrationBaseFilterResponse);
            return poolReportDetailResponse;

        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return null;
        }
    }

    @Override
    public BaseFilterResponse<PoolReportDetailResponse.EpochSize> detailEpochSize(String reportId, Pageable pageable) {
        try {
            //FIXME replace: find by username
            PoolReport poolReport = poolReportRepository.findById(Long.parseLong(reportId)).get();
            List<PoolReportProjection> epochSizeProjections = epochStakeRepository.getEpochSizeByPoolReport(poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch(), pageable).getContent();
            List<PoolReportDetailResponse.EpochSize> epochSizes = epochSizeProjections.stream().map(PoolReportDetailResponse.EpochSize::toDomain).collect(Collectors.toList());
            return new BaseFilterResponse<>(epochSizes, epochSizes.size());
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            return null;
        }
    }
}
