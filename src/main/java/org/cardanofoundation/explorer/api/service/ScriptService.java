package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.springframework.data.domain.Pageable;

public interface ScriptService {

  BaseFilterResponse<NativeScriptFilterResponse> getNativeScripts(Pageable pageable);

  BaseFilterResponse<SmartContractFilterResponse> getSmartContracts(Pageable pageable);
}
