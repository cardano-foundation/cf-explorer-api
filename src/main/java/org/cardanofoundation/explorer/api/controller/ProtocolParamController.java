package org.cardanofoundation.explorer.api.controller;

import java.util.List;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;

@RestController
@RequestMapping("/api/v1/protocols")
@RequiredArgsConstructor
@FieldDefaults(level = AccessLevel.PRIVATE)
public class ProtocolParamController {

  final ProtocolParamService protocolParamService;

  @GetMapping("histories")
  @LogMessage
  @Operation(summary = "Get current protocol history change")
  public List<Protocols> getCurrentProtocol() {
    return protocolParamService.getHistoryProtocolParam();
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
  public Protocols getFixedProtocols() {
    return protocolParamService.getFixedProtocols();
  }
}
