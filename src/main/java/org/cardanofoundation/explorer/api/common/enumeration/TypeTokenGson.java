package org.cardanofoundation.explorer.api.common.enumeration;

import java.lang.reflect.Type;
import java.util.List;
import java.util.function.Supplier;

import lombok.AllArgsConstructor;
import lombok.Getter;

import com.google.gson.reflect.TypeToken;

import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.MetadataBolnisi;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;

@Getter
@AllArgsConstructor
public enum TypeTokenGson {
  TOKEN_FILTER(() -> new TypeToken<BaseFilterResponse<TokenFilterResponse>>() {}.getType()),

  TOKEN_DETAIL(() -> new TypeToken<TokenResponse>() {}.getType()),
  NEWS(() -> new TypeToken<>() {}.getType()),
  BOLNISI_METADATA(() -> new TypeToken<MetadataBolnisi>() {}.getType()),
  TX_CHART(() -> new TypeToken<List<TxGraph>>() {}.getType());

  private final Supplier<Type> type;
}
