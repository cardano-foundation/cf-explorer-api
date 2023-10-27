package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.service.ScriptService;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;
import org.cardanofoundation.explorer.consumercommon.entity.Script_;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/scripts")
@RequiredArgsConstructor
@Validated
@Tag(name = "script", description = "The script APIs")
public class ScriptController {

  private final ScriptService scriptService;

  @GetMapping("/native-scripts")
  public ResponseEntity<BaseFilterResponse<NativeScriptFilterResponse>> getNativeScripts(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {Script_.TX},
          direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(scriptService.getNativeScripts(pagination.toPageable()));
  }

  @GetMapping("/smart-contracts")
  public ResponseEntity<BaseFilterResponse<SmartContractFilterResponse>> getSmartContracts(
      @ParameterObject @PaginationValid @PaginationDefault(size = 20, sort = {Script_.TX},
          direction = Sort.Direction.DESC) @Valid Pagination pagination) {
    return ResponseEntity.ok(scriptService.getSmartContracts(pagination.toPageable()));
  }

}
