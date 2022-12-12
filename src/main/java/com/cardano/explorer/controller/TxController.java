package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.request.TxFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.TxResponse;
import com.cardano.explorer.model.response.dashboard.TxGraph;
import com.cardano.explorer.model.response.dashboard.TxSummary;
import com.cardano.explorer.service.TxService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/tx")
@RequiredArgsConstructor
public class TxController {

  private final TxService txService;

  //  @Cacheable(value = "tx_page", key = "#pageable.pageNumber+''+#pageable.pageSize+''+#pageable.sort")
  @GetMapping("/list")
  @LogMessage
  @Operation(summary = "Filter transaction")
  public BaseFilterResponse<TxFilterResponse> filter(
      @Parameter(description = "Condition for filter (Set all properties to null for get all)")
      TxFilterRequest request,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return txService.filterTx(pageable, request);
  }

  @GetMapping("/{hash}")
  @LogMessage
  @Operation(summary = "Get transaction detail by hash")
  public TxResponse getTransactionDetail(@PathVariable
  @Parameter(description = "Hash value of transaction") String hash) {
    return txService.getTxDetailByHash(hash);
  }

  @GetMapping("/current")
  @LogMessage
  @Operation(summary = "Get current transactions")
  public ResponseEntity<List<TxSummary>> findCurrentEpoch(){
    return ResponseEntity.ok(txService.findLatestTxSummary());
  }

  @GetMapping("/graph")
  @LogMessage
  @Operation(summary = "Get Number Transaction On Last 15 Days")
  public ResponseEntity<List<TxGraph>> getNumberTransactionOnLast15Days() {
    return ResponseEntity.ok(txService.getTxsAfterTime());
  }
}
