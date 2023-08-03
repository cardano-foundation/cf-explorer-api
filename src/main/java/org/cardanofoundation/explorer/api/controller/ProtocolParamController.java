package org.cardanofoundation.explorer.api.controller;

import java.math.BigInteger;
import java.util.List;

import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.protocol.FixedProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;

@RestController
@RequestMapping("/api/v1/protocols")
@RequiredArgsConstructor
@FieldDefaults(level = AccessLevel.PRIVATE)
@Tag(name = "protocols", description = "The protocols APIs")
public class ProtocolParamController {

  final ProtocolParamService protocolParamService;

  @GetMapping("/histories/filter/{protocolsTypes}")
  @LogMessage
  @Operation(summary = "Get current protocol history change", tags = {"protocols"})
  public ResponseEntity<HistoriesProtocol> getCurrentProtocolWithFilter(
      @PathVariable(value = "protocolsTypes", required = false)
      @Parameter(description = "protocol want to filter")
      List<ProtocolType> protocolTypes,
      @RequestParam(value = "startTime", required = false)
      BigInteger startTime,
      @RequestParam(value = "endTime", required = false)
      BigInteger endTime) {

    return ResponseEntity
        .ok(protocolParamService.getHistoryProtocolParameters(protocolTypes, startTime, endTime));
  }

  @GetMapping("/latest")
  @LogMessage
  @Operation(summary = "Get current protocol latest change", tags = {"protocols"})
  public ResponseEntity<Protocols> getLatestChange() {
    return ResponseEntity.ok(protocolParamService.getLatestChange());
  }

  @GetMapping("/fixed")
  @LogMessage
  @Operation(summary = "Get fixed protocols parameters ", tags = {"protocols"})
  public ResponseEntity<FixedProtocol> getFixedProtocols() {
    return ResponseEntity.ok(protocolParamService.getFixedProtocols());
  }
}
