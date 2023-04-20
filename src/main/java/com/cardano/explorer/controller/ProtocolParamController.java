package com.cardano.explorer.controller;

import com.cardano.explorer.common.enumeration.ProtocolType;
import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.response.protocol.ProtocolHistory;
import com.cardano.explorer.service.ProtocolParamService;
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
}
