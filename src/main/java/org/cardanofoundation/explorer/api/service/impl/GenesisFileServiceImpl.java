package org.cardanofoundation.explorer.api.service.impl;

import java.util.regex.Pattern;

import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.service.GenesisService;
import org.cardanofoundation.ledgersync.common.model.ByronGenesis;
import org.cardanofoundation.ledgersync.common.model.ShelleyGenesis;
import org.cardanofoundation.ledgersync.common.util.GenesisUtils;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class GenesisFileServiceImpl implements GenesisService {

  @Override
  public ShelleyGenesis fillContentShelley(String source) {
    if(isURL(source)){
      return GenesisUtils.fillContentUrlToShelley(source);
    }
    return GenesisUtils.fillContentFileToShelley(source);
  }

  @Override
  public ByronGenesis fillContentByron(String source) {
    if(isURL(source)){
      return GenesisUtils.fillContentUrlToByron(source);
    }
    return GenesisUtils.fillContentFileToByron(source);
  }

  private boolean isURL(String input) {
    return Pattern.compile("^https?://.*").matcher(input).matches();
  }
}
