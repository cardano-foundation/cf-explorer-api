package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.model.response.search.ScriptSearchResponse;

public interface ScriptService {

  SmartContractDetailResponse getSmartContractDetail(String scriptHash);

  BaseFilterResponse<SmartContractTxResponse> getSmartContractTxs(String scriptHash, Pageable pageable);

  ScriptSearchResponse searchScript(String scriptHash);
}