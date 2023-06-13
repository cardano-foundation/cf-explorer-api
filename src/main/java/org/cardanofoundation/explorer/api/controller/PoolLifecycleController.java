package org.cardanofoundation.explorer.api.controller;

import java.util.Date;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolInfoResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.SPOStatusResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.service.PoolLifecycleService;
import org.cardanofoundation.explorer.common.validate.pagination.Pagination;
import org.cardanofoundation.explorer.common.validate.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validate.pagination.PaginationValid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.repository.query.Param;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/pool-lifecycle")
@RequiredArgsConstructor
public class PoolLifecycleController {

  private final PoolLifecycleService poolLifecycleService;


  @GetMapping(value = "/registration")
  public ResponseEntity<BaseFilterResponse<PoolUpdateResponse>> registration(
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination,
      @Param("poolView") String poolView, @Param("txHash") String txHash,
      @Param("fromDate") Date fromDate,
      @Param("toDate") Date toDate) {
    return ResponseEntity.ok(
        poolLifecycleService.registration(poolView, txHash, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping(value = "/registration-detail")
  public ResponseEntity<RegistrationResponse> registrationDetail(@Param("poolView") String poolView,
      @RequestParam("id") Long id) {
    return ResponseEntity.ok(poolLifecycleService.registrationDetail(poolView, id));
  }

  @GetMapping(value = "/pool-update")
  public ResponseEntity<BaseFilterResponse<PoolUpdateResponse>> poolUpdate(
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination,
      @Param("poolView") String poolView, @Param("txHash") String txHash,
      @Param("fromDate") Date fromDate,
      @Param("toDate") Date toDate) {
    return ResponseEntity.ok(
        poolLifecycleService.poolUpdate(poolView, txHash, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping(value = "/pool-update-detail")
  public ResponseEntity<PoolUpdateDetailResponse> poolUpdate(@RequestParam("id") Long id) {
    return ResponseEntity.ok(poolLifecycleService.poolUpdateDetail(id));
  }

  @GetMapping(value = "/reward")
  public ResponseEntity<BaseFilterResponse<RewardResponse>> reward(
      @RequestParam("poolView") String poolView,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination) {
    return ResponseEntity.ok(poolLifecycleService.listReward(poolView, pagination.toPageable()));
  }

  @GetMapping(value = "/de-registration")
  public ResponseEntity<BaseFilterResponse<DeRegistrationResponse>> deRegistration(
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination,
      @Param("poolView") String poolView, @Param("txHash") String txHash,
      @Param("fromDate") Date fromDate,
      @Param("toDate") Date toDate) {
    return ResponseEntity.ok(
        poolLifecycleService.deRegistration(poolView, txHash, fromDate, toDate, pagination.toPageable()));
  }

  @GetMapping(value = "/owner")
  public ResponseEntity<BaseFilterResponse<String>> poolOwner(
      @RequestParam("stakeKey") String stakeKey,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination) {
    return ResponseEntity.ok(poolLifecycleService.getPoolViewByStakeKey(stakeKey, pagination.toPageable()));
  }

  @GetMapping(value = "/pool-info")
  public ResponseEntity<PoolInfoResponse> poolInfo(@RequestParam("poolView") String poolView) {
    return ResponseEntity.ok(poolLifecycleService.poolInfo(poolView));
  }

  @GetMapping(value = "/registration-list")
  public ResponseEntity<BaseFilterResponse<TabularRegisResponse>> registrationList(
      @RequestParam("poolView") String poolView,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination) {
    return ResponseEntity.ok(
        poolLifecycleService.registrationList(poolView, pagination.toPageable()));
  }

  @GetMapping(value = "/pool-update-list")
  public ResponseEntity<BaseFilterResponse<PoolUpdateDetailResponse>> poolUpdate(
      @RequestParam("poolView") String poolView,
      @ParameterObject @PaginationValid @PaginationDefault(size = 10, page = 0) Pagination pagination) {
    return ResponseEntity.ok(
        poolLifecycleService.poolUpdateList(poolView, pagination.toPageable()));
  }

  @GetMapping(value = "/status")
  public ResponseEntity<SPOStatusResponse> poolStatus(@RequestParam("poolView") String poolView) {
    return ResponseEntity.ok(poolLifecycleService.poolLifecycleStatus(poolView));
  }
}
