package org.cardanofoundation.explorer.api.service;

import java.util.Set;

import org.cardanofoundation.explorer.api.model.request.script.nativescript.NativeScriptFilterRequest;
import org.cardanofoundation.explorer.api.model.request.script.smartcontract.SmartContractFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.model.response.search.ScriptSearchResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.springframework.data.domain.Pageable;

public interface ScriptService {

  BaseFilterResponse<NativeScriptFilterResponse> getNativeScripts(NativeScriptFilterRequest filterRequest,
                                                                  Pageable pageable);

  NativeScriptResponse getNativeScriptDetail(String scriptHash);

  String verifyNativeScript(String scriptHash, String scriptJson);

  BaseFilterResponse<TokenFilterResponse> getNativeScriptTokens(String scriptHash, Pageable pageable);

  BaseFilterResponse<TokenAddressResponse> getNativeScriptHolders(String scriptHash, Pageable pageable);

  BaseFilterResponse<SmartContractFilterResponse> getSmartContracts(
      SmartContractFilterRequest filterRequest, Pageable pageable);

  SmartContractDetailResponse getSmartContractDetail(String scriptHash);

  BaseFilterResponse<SmartContractTxResponse> getSmartContractTxs(String scriptHash, Pageable pageable);

  ScriptSearchResponse searchScript(String scriptHash);

  Set<String> getContractExecutions(String txHash, String scriptHash);
}