package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.service.GenesisService;
import org.cardanofoundation.ledgersync.common.model.ByronGenesis;
import org.cardanofoundation.ledgersync.common.model.ShelleyGenesis;
import org.cardanofoundation.ledgersync.common.util.GenesisUtils;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Profile("!submit-api")
@Service
@RequiredArgsConstructor
public class GenesisFileServiceImpl implements GenesisService {

  @Override
  public ShelleyGenesis fillContentShelley(String url) {
    return GenesisUtils.fillContentFileToShelley(url);
  }

  @Override
  public ByronGenesis fillContentByron(String url) {
    return GenesisUtils.fillContentFileToByron(url);
  }
}
