package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.CertDetailsData;
import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.MetadataBolnisi;
import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.WineryData;

public interface BolnisiMetadataService {

  MetadataBolnisi getBolnisiMetadata(String jsonMetadata);

  WineryData getWineryData(String txHash, String wineryId);

  CertDetailsData getCertDetailsData(String txHash, String certNo);
}
