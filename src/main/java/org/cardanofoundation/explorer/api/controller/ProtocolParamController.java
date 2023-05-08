package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.model.response.protocol.ProtocolHistory;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;
import io.swagger.v3.oas.annotations.Operation;
import java.util.Set;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/protocol")
@RequiredArgsConstructor
@FieldDefaults(level = AccessLevel.PRIVATE)
public class ProtocolParamController {

  final ProtocolParamService protocolParamService;

  @GetMapping("/{type}/history")
  @LogMessage
  @Operation(summary = "Get protocol history change")
  public Set<ProtocolHistory> getProtocolHistory(@PathVariable ProtocolType type) {
    return protocolParamService.getProtocolHistory(type);
  }

  @GetMapping("/current")
  @LogMessage
  @Operation(summary = "Get current protocol history change")
  public Protocols getCurrentProtocol (){
    return protocolParamService.getProtocolCurrentHistory();
  }
}
