package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxSummary;
import org.cardanofoundation.explorer.api.model.response.tx.TxResponse;
import org.cardanofoundation.explorer.api.service.TxService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import java.math.BigInteger;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
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
@RequestMapping("/api/v1/txs")
@RequiredArgsConstructor
public class TxController {

  private final TxService txService;
  private static final long DEFALUT_DAYS = 7;

  @GetMapping
  @LogMessage
  @Operation(summary = "Filter transaction")
  public ResponseEntity<BaseFilterResponse<TxFilterResponse>> filter(
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "blockId", "blockIndex"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(txService.getAll(pageable));
  }

  @GetMapping("/{hash}")
  @LogMessage
  @Operation(summary = "Get transaction detail by hash")
  public ResponseEntity<TxResponse> getTransactionDetail(@PathVariable
  @Parameter(description = "Hash value of transaction") String hash) {
    return ResponseEntity.ok(txService.getTxDetailByHash(hash));
  }

  @GetMapping("/current")
  @LogMessage
  @Operation(summary = "Get current transactions")
  public ResponseEntity<List<TxSummary>> findCurrentTransaction() {
    return ResponseEntity.ok(txService.findLatestTxSummary());
  }

  @GetMapping("/graph/{type}")
  @LogMessage
  @Operation(summary = "Get Number Transaction On Fixable Days Max 1 month")
  public ResponseEntity<List<TxGraph>> getNumberTransactionOnLastDays(@PathVariable
  @Parameter(description = "Type analytics: 1d, 1w, 2w, 1m") AnalyticType type) {
    long days = DEFALUT_DAYS;
    switch (type) {
      case ONE_DAY:
        days = BigInteger.ONE.longValue();
        break;
      case ONE_WEEK:
        days = DEFALUT_DAYS;
        break;
      case TWO_WEEK:
        days = 14L;
        break;
      case ONE_MONTH:
        days = Math.abs(ChronoUnit.DAYS.between(LocalDateTime.now(), LocalDateTime.now().minusMonths(1L)));
        break;
    }
    return ResponseEntity.ok(txService.getTxsAfterTime(days));
  }

}
