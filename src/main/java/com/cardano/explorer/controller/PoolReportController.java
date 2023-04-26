package com.cardano.explorer.controller;

import com.cardano.explorer.common.enumeration.PoolReportEvent;
import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.request.pool.report.PoolReportCreateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportDetailResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportListResponse;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Compose Pool Report file for each poolId, List all report for each Wallet User
 *
 * @author  an.nguyen4
 * @version 0.0.1
 * @since   26/04/2023
 */

@RestController
@RequestMapping("api/v1/pool-report")
@RequiredArgsConstructor
public class PoolReportController {

    @PostMapping("create")
    public ResponseEntity<Boolean> createPoolReport(@RequestBody PoolReportCreateRequest poolReportCreateRequest) {
        //TODO Implement service processing
        return ResponseEntity.ok(true);
    }

    @GetMapping("list")
    public ResponseEntity<BaseFilterResponse<PoolReportListResponse>> listPoolReport(@ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
        //TODO Implement service processing
        // Temp
        BaseFilterResponse<PoolReportListResponse> res = new BaseFilterResponse<>();
        List<PoolReportListResponse> poolReportListResponses = List.of(
                PoolReportListResponse.builder().reportName("Report Name").epochRanges(new Integer[]{300, 304}).isPoolSize(true).isFreePaid(true).event(transformEvent(List.of(PoolReportEvent.REWARD, PoolReportEvent.POOL_UPDATE))).build(),
                PoolReportListResponse.builder().reportName("Report Name").epochRanges(new Integer[]{300, 304}).isPoolSize(true).isFreePaid(true).event(transformEvent(List.of(PoolReportEvent.REWARD, PoolReportEvent.POOL_UPDATE))).build(),
                PoolReportListResponse.builder().reportName("Report Name").epochRanges(new Integer[]{300, 304}).isPoolSize(true).isFreePaid(true).event(transformEvent(List.of(PoolReportEvent.REWARD, PoolReportEvent.POOL_UPDATE))).build(),
                PoolReportListResponse.builder().reportName("Report Name").epochRanges(new Integer[]{300, 304}).isPoolSize(true).isFreePaid(true).event(transformEvent(List.of(PoolReportEvent.REWARD, PoolReportEvent.POOL_UPDATE))).build(),
                PoolReportListResponse.builder().reportName("Report Name").epochRanges(new Integer[]{300, 304}).isPoolSize(true).isFreePaid(true).event(transformEvent(List.of(PoolReportEvent.REWARD, PoolReportEvent.POOL_UPDATE))).build(),
                PoolReportListResponse.builder().reportName("Report Name").epochRanges(new Integer[]{300, 304}).isPoolSize(true).isFreePaid(true).event(transformEvent(List.of(PoolReportEvent.REWARD, PoolReportEvent.POOL_UPDATE))).build(),
                PoolReportListResponse.builder().reportName("Report Name").epochRanges(new Integer[]{300, 304}).isPoolSize(true).isFreePaid(true).event(transformEvent(List.of(PoolReportEvent.REWARD, PoolReportEvent.POOL_UPDATE))).build(),
                PoolReportListResponse.builder().reportName("Report Name").epochRanges(new Integer[]{300, 304}).isPoolSize(true).isFreePaid(true).event(transformEvent(List.of(PoolReportEvent.REWARD, PoolReportEvent.POOL_UPDATE))).build(),
                PoolReportListResponse.builder().reportName("Report Name").epochRanges(new Integer[]{300, 304}).isPoolSize(true).isFreePaid(true).event(transformEvent(List.of(PoolReportEvent.REWARD, PoolReportEvent.POOL_UPDATE))).build()
        );
        res.setData(poolReportListResponses);
        res.setTotalItems(poolReportListResponses.size());

        return ResponseEntity.ok(res);
    }

    @GetMapping("detail/{reportId}")
    public ResponseEntity<PoolReportDetailResponse> detailPoolReport(@PathVariable String reportId) {
        //TODO Implement service processing
        // Temp
        PoolReportDetailResponse res = PoolReportDetailResponse.builder().poolRegistrations(temp1()).poolUpdates(temp2()).rewardDistributions(temp3()).deregistrations(temp4()).epochSizes(temp5()).build();

        return ResponseEntity.ok(res);
    }

    @GetMapping("detail/{reportId}/epoch-size")
    public ResponseEntity<BaseFilterResponse<PoolReportDetailResponse.EpochSize>> detailEpochSizePoolReport(@PathVariable String reportId, @ParameterObject @PageableDefault(size = 10, page = 0) Pageable pageable) {
        //TODO Implement service processing
        // Temp
        return ResponseEntity.ok(temp5());
    }

    private BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> temp1() {
        BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> res = new BaseFilterResponse<>();
        List<PoolReportDetailResponse.PoolRegistration> poolReportDetailResponses = List.of(
                PoolReportDetailResponse.PoolRegistration.builder().txnHash("2ca384e18e4eab235194666e66e4c195a155bb2fe7b23d254f08f6a887ed28ab").timestamp(new Date()).adaValueHold(BigDecimal.valueOf(1.2)).adaValueFees(BigDecimal.valueOf(0.3)).owner("stake1u93hxcutpd6l3cp3m2szp3avw36yj7penqp3ddntu0qxxwsftum9s").build(),
                PoolReportDetailResponse.PoolRegistration.builder().txnHash("2ca384e18e4eab235194666e66e4c195a155bb2fe7b23d254f08f6a887ed28ab").timestamp(new Date()).adaValueHold(BigDecimal.valueOf(1.2)).adaValueFees(BigDecimal.valueOf(0.3)).owner("stake1u93hxcutpd6l3cp3m2szp3avw36yj7penqp3ddntu0qxxwsftum9s").build()
        );
        res.setData(poolReportDetailResponses);
        res.setTotalItems(poolReportDetailResponses.size());
        return res;
    }

    private BaseFilterResponse<PoolReportDetailResponse.PoolUpdate> temp2() {
        BaseFilterResponse<PoolReportDetailResponse.PoolUpdate> res = new BaseFilterResponse<>();
        List<PoolReportDetailResponse.PoolUpdate> poolReportDetailResponses = List.of(
                PoolReportDetailResponse.PoolUpdate.builder().txnHash("2ca384e18e4eab235194666e66e4c195a155bb2fe7b23d254f08f6a887ed28ab").timestamp(new Date()).adaValueHold(BigDecimal.valueOf(1.2)).adaValueFees(BigDecimal.valueOf(0.3)).build(),
                PoolReportDetailResponse.PoolUpdate.builder().txnHash("2ca384e18e4eab235194666e66e4c195a155bb2fe7b23d254f08f6a887ed28ab").timestamp(new Date()).adaValueHold(BigDecimal.valueOf(1.2)).adaValueFees(BigDecimal.valueOf(0.3)).build()
        );
        res.setData(poolReportDetailResponses);
        res.setTotalItems(poolReportDetailResponses.size());
        return res;
    }

    private BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> temp3() {
        BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> res = new BaseFilterResponse<>();
        List<PoolReportDetailResponse.RewardDistribution> poolReportDetailResponses = List.of(
                PoolReportDetailResponse.RewardDistribution.builder().epoch("404").date(new Date()).operatorReward(BigDecimal.valueOf(1234.32)).rewardAccount("stake1u95vd8c9gwhp4f6xzhj7nnkqz94tdcypkvpset52kua7s9s8fvh5d").build(),
                PoolReportDetailResponse.RewardDistribution.builder().epoch("404").date(new Date()).operatorReward(BigDecimal.valueOf(1234.32)).rewardAccount("stake1u95vd8c9gwhp4f6xzhj7nnkqz94tdcypkvpset52kua7s9s8fvh5d").build()
        );
        res.setData(poolReportDetailResponses);
        res.setTotalItems(poolReportDetailResponses.size());
        return res;
    }

    private BaseFilterResponse<PoolReportDetailResponse.Deregistration> temp4() {
        BaseFilterResponse<PoolReportDetailResponse.Deregistration> res = new BaseFilterResponse<>();
        List<PoolReportDetailResponse.Deregistration> poolReportDetailResponses = List.of(
                PoolReportDetailResponse.Deregistration.builder().txnHash("2ca384e18e4eab235194666e66e4c195a155bb2fe7b23d254f08f6a887ed28ab").date(new Date()).adaValueHold(BigDecimal.valueOf(1.2)).adaValueFees(BigDecimal.valueOf(0.3)).owner("stake1u95vd8c9gwhp4f6xzhj7nnkqz94tdcypkvpset52kua7s9s8fvh5d").build(),
                PoolReportDetailResponse.Deregistration.builder().txnHash("2ca384e18e4eab235194666e66e4c195a155bb2fe7b23d254f08f6a887ed28ab").date(new Date()).adaValueHold(BigDecimal.valueOf(1.2)).adaValueFees(BigDecimal.valueOf(0.3)).owner("stake1u95vd8c9gwhp4f6xzhj7nnkqz94tdcypkvpset52kua7s9s8fvh5d").build()
        );
        res.setData(poolReportDetailResponses);
        res.setTotalItems(poolReportDetailResponses.size());
        return res;
    }

    private BaseFilterResponse<PoolReportDetailResponse.EpochSize> temp5() {
        BaseFilterResponse<PoolReportDetailResponse.EpochSize> res = new BaseFilterResponse<>();
        List<PoolReportDetailResponse.EpochSize> poolReportDetailResponses = List.of(
                PoolReportDetailResponse.EpochSize.builder().epoch("401").size(BigDecimal.valueOf(123456721.21)).build(),
                PoolReportDetailResponse.EpochSize.builder().epoch("402").size(BigDecimal.valueOf(123456721.21)).build(),
                PoolReportDetailResponse.EpochSize.builder().epoch("403").size(BigDecimal.valueOf(123456721.21)).build(),
                PoolReportDetailResponse.EpochSize.builder().epoch("404").size(BigDecimal.valueOf(123456721.21)).build()
        );
        res.setData(poolReportDetailResponses);
        res.setTotalItems(poolReportDetailResponses.size());
        return res;
    }

    private String transformEvent(List<PoolReportEvent> poolReportEvents) {
        return poolReportEvents.stream().map(PoolReportEvent::getValue).collect(Collectors.joining(","));
    }
}
