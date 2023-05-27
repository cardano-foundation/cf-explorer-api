package org.cardanofoundation.explorer.api.controller;

import java.util.List;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
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
public class ProtocolParamController {

  final ProtocolParamService protocolParamService;

  @GetMapping("histories/filter/{protocolsTypes}")
  @LogMessage
  @Operation(summary = "Get current protocol history change")
  public HistoriesProtocol getCurrentProtocolWithFilter(
      @PathVariable(value = "protocolsTypes", required = false)
      @Parameter(description = "protocol want to filter")
      List<ProtocolType> protocolTypes) {
    if (ObjectUtils.isEmpty(protocolTypes)) {
      protocolTypes = ProtocolType.getAll();
    } else if (protocolTypes.contains(ProtocolType.ALL)) {
      protocolTypes = ProtocolType.getAll();
    }

    return protocolParamService.getHistoryProtocolParameters(protocolTypes);
  }

  @GetMapping("latest")
  @LogMessage
  @Operation(summary = "Get current protocol history change")
  public Protocols getLatestChange() {
    return protocolParamService.getLatestChange();
  }

  @GetMapping("fixed")
  @LogMessage
  @Operation(summary = "Get current protocol history change")
  public FixedProtocol getFixedProtocols() {
    return protocolParamService.getFixedProtocols();
  }
}
