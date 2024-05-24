package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.common.model.ByronGenesis;
import org.cardanofoundation.explorer.common.model.ConwayGenesis;
import org.cardanofoundation.explorer.common.model.ShelleyGenesis;

public interface GenesisService {

  ShelleyGenesis fillContentShelley(String url);

  ByronGenesis fillContentByron(String url);

  ConwayGenesis fillContentConway(String url);
}
