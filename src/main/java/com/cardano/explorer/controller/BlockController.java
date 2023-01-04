package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.request.BlockFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.BlockFilterResponse;
import com.cardano.explorer.model.response.BlockResponse;
import com.cardano.explorer.service.BlockService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
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
@RequestMapping("/api/v1/block")
@RequiredArgsConstructor
public class BlockController {

  private final BlockService blockService;

  @GetMapping("/{no}")
  @LogMessage
  @Operation(summary = "Get a block detail by its no")
  public ResponseEntity<BlockResponse> getBlockDetailByBlockNo(
      @PathVariable @Parameter(description = "Block number") String no) {
    return ResponseEntity.ok(blockService.getBlockDetailByBlockNo(no));
  }

  @GetMapping("/hash/{hash}")
  @LogMessage
  @Operation(summary = "Get a block detail by its hash")
  public ResponseEntity<BlockResponse> getBlockDetailByHash(
      @PathVariable @Parameter(description = "Block hash") String hash) {
    return ResponseEntity.ok(blockService.getBlockDetailByHash(hash));
  }

  //  @Cacheable(value = "block_list", key = "#pageable.pageNumber+''+#pageable.pageSize+''+#pageable.sort+''+#epochNo")
  @GetMapping("/list")
  @LogMessage
  @Operation(summary = "Filter block")
  public ResponseEntity<BaseFilterResponse<BlockFilterResponse>> filter(
      @Parameter(description = "Condition for filter (Set all properties to null for get all)")
      BlockFilterRequest request,
      @ParameterObject @PageableDefault(size = 20, value = 20, sort = {
          "id"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(blockService.filterBlock(pageable, request));
  }

}
