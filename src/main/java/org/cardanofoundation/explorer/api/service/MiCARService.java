package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.response.micar.AddressCarbonEmissionResponse;

public interface MiCARService {
  AddressCarbonEmissionResponse getCarbonEmissionsByAddressAndPool(String address);

  Object getCarbonEmissionsOverview(String responseType, String key);

  Object getCarbonEmissionsHistorical(String key);
}
