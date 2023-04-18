package com.cardano.explorer.service;

import com.cardano.explorer.common.enumeration.ProtocolType;
import com.cardano.explorer.model.response.protocol.ProtocolHistory;
import java.util.List;

public interface ProtocolParamService {

  List<ProtocolHistory> getProtocolHistory(ProtocolType protocolType);
}
