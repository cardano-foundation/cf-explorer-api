package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.ledgersync.common.model.ByronGenesis;
import org.cardanofoundation.ledgersync.common.model.ShelleyGenesis;

public interface GenesisService {

  ShelleyGenesis fillContentShelley(String url);

  ByronGenesis fillContentByron(String url);
}
