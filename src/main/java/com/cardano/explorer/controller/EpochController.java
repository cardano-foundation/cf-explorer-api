package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.BaseFilterResponse;
import com.cardano.explorer.model.EpochResponse;
import com.cardano.explorer.service.EpochService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/epoch")
@RequiredArgsConstructor
public class EpochController {

  private final EpochService epochService;

  @GetMapping("/{no}")
  @LogMessage
  @Operation(summary = "Get a epoch detail by its no")
  public EpochResponse getEpochDetail(@PathVariable
      @Parameter(description="Epoch Number") Integer no) {
    return epochService.getEpochDetail(no);
  }

  @GetMapping("/current")
  @LogMessage
  @Operation(summary = "Get current epoch")
  public Integer getCurrentEpoch() {
    return epochService.getCurrentEpoch();
  }

  @GetMapping("/list")
  @LogMessage
  @Operation(summary = "Get all epoch")
  public BaseFilterResponse<EpochResponse> getAll(@ParameterObject
            @PageableDefault(sort = {"id"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return epochService.filterEpoch(pageable);
  }
}
