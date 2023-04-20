package com.cardano.explorer.service;

import com.cardano.explorer.common.enumeration.ProtocolType;
import com.cardano.explorer.model.response.protocol.ProtocolHistory;
import java.util.Set;
import org.springframework.transaction.annotation.Transactional;

public interface ProtocolParamService {

  @Transactional(readOnly = true)
  Set<ProtocolHistory> getProtocolHistory(ProtocolType protocolType);
}
