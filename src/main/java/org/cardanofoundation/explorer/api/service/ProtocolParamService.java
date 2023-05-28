package org.cardanofoundation.explorer.api.service;

import java.sql.Timestamp;
import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.model.response.protocol.FixedProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;

public interface ProtocolParamService {

  @Transactional(readOnly = true)
  HistoriesProtocol getHistoryProtocolParameters(List<ProtocolType> protocolTypes,
                                                 Timestamp startTime, Timestamp endTime);

  @Transactional(readOnly = true)
  Protocols getLatestChange();

  @Transactional(readOnly = true)
  FixedProtocol getFixedProtocols();

}
