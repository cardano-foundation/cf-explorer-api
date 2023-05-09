package org.cardanofoundation.explorer.api.service;

import java.util.Set;

import org.springframework.transaction.annotation.Transactional;

import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.model.response.protocol.ProtocolHistory;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;

public interface ProtocolParamService {

  @Transactional(readOnly = true)
  Set<ProtocolHistory> getProtocolHistory(ProtocolType protocolType);

  @Transactional(readOnly = true)
  Protocols getProtocolCurrentHistory();
}
