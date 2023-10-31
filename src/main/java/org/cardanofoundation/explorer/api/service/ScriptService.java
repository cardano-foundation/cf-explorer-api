package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.springframework.data.domain.Pageable;

public interface ScriptService {

  BaseFilterResponse<NativeScriptFilterResponse> getNativeScripts(Pageable pageable);

  NativeScriptResponse getNativeScripts(String scriptHash);

  BaseFilterResponse<TokenFilterResponse> getNativeScriptTokens(String scriptHash, Pageable pageable);

  BaseFilterResponse<TokenAddressResponse> getNativeScriptHolders(String scriptHash, Pageable pageable);

  BaseFilterResponse<SmartContractFilterResponse> getSmartContracts(Pageable pageable);

  SmartContractDetailResponse getSmartContractDetail(String scriptHash);

  BaseFilterResponse<SmartContractTxResponse> getSmartContractTxs(String scriptHash, Pageable pageable);
}
