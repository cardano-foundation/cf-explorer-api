package org.cardanofoundation.explorer.api.controller;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.response.protocol.FixedProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;

@RestController
@RequestMapping("/api/v1/protocols")
@RequiredArgsConstructor
@FieldDefaults(level = AccessLevel.PRIVATE)
public class ProtocolParamController {

  final ProtocolParamService protocolParamService;

  @GetMapping("/histories/filter/{protocolsTypes}")
  @LogMessage
  @Operation(summary = "Get current protocol history change")
  public ResponseEntity<HistoriesProtocol> getCurrentProtocolWithFilter(
      @PathVariable(value = "protocolsTypes", required = false)
      @Parameter(description = "protocol want to filter")
      List<ProtocolType> protocolTypes,
      @RequestParam(value = "startTime", required = false)
      BigInteger startTime,
      @RequestParam(value = "endTime", required = false)
      BigInteger endTime) {

    if (ObjectUtils.isEmpty(protocolTypes) || protocolTypes.contains(ProtocolType.ALL)) {
      protocolTypes = ProtocolType.getAll();
    }

    if ((Objects.isNull(startTime) && Objects.nonNull(endTime)) ||
        (Objects.isNull(endTime) && Objects.nonNull(startTime))) {
      throw new BusinessException(BusinessCode.TIME_RANGE_ILLEGAL);
    }

    Timestamp startTimeFilter = null;
    Timestamp endTimeFilter = null;

    if (Objects.nonNull(startTime) && Objects.nonNull(endTime)) {
      if (endTime.subtract(startTime).compareTo(BigInteger.ZERO) < BigInteger.ZERO.intValue()) {
        throw new BusinessException(BusinessCode.TIME_RANGE_ILLEGAL);
      }

      startTimeFilter = Timestamp.valueOf(
          LocalDateTime.ofEpochSecond(startTime.longValue(), BigInteger.ZERO.intValue(), ZoneOffset.UTC));
      endTimeFilter = Timestamp.valueOf(
          LocalDateTime.ofEpochSecond(endTime.longValue(), BigInteger.ZERO.intValue(), ZoneOffset.UTC));
    }

    return ResponseEntity.ok(
        protocolParamService.getHistoryProtocolParameters(protocolTypes, startTimeFilter,
            endTimeFilter));
  }

  @GetMapping("/latest")
  @LogMessage
  @Operation(summary = "Get current protocol latest change")
  public ResponseEntity<Protocols> getLatestChange() {
    return ResponseEntity.ok(protocolParamService.getLatestChange());
  }

  @GetMapping("/fixed")
  @LogMessage
  @Operation(summary = "Get fixed protocols parameters ")
  public ResponseEntity<FixedProtocol> getFixedProtocols() {
    return ResponseEntity.ok(protocolParamService.getFixedProtocols());
  }
}
