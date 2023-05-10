package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import java.util.List;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/protocols")
@RequiredArgsConstructor
@FieldDefaults(level = AccessLevel.PRIVATE)
public class ProtocolParamController {

  final ProtocolParamService protocolParamService;

  @GetMapping("histories")
  @LogMessage
  @Operation(summary = "Get current protocol history change")
  public HistoriesProtocol  getCurrentProtocol() {
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
