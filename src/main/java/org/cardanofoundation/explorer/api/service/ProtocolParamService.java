package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.model.response.protocol.FixedProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;

public interface ProtocolParamService {

  /**
   * Find history change of protocol parameters
   * @param protocolTypes
   * @param startTime
   * @param endTime
   * @return
   */
  @Transactional(readOnly = true)
  HistoriesProtocol getHistoryProtocolParameters(List<ProtocolType> protocolTypes,
                                                 BigInteger startTime, BigInteger endTime);

  /**
   * Find latest protocol param have changed
   *
   * @return
   */
  @Transactional(readOnly = true)
  Protocols getLatestChange();

  @Transactional(readOnly = true)
  FixedProtocol getFixedProtocols();

}
