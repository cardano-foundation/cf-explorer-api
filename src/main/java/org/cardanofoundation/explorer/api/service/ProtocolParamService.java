package org.cardanofoundation.explorer.api.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import org.cardanofoundation.explorer.api.model.response.protocol.FixedProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;

public interface ProtocolParamService {

  @Transactional(readOnly = true)
  HistoriesProtocol getHistoryProtocolParameters();

  @Transactional(readOnly = true)
  Protocols getLatestChange();

  @Transactional(readOnly = true)
  FixedProtocol getFixedProtocols();

}
