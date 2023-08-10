package org.cardanofoundation.explorer.api.common.enumeration;

import com.google.gson.reflect.TypeToken;
import java.lang.reflect.Type;
import java.util.function.Supplier;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;

@Getter
@AllArgsConstructor
public enum TypeTokenGson {
  TOKEN_FILTER(
      () -> new TypeToken<BaseFilterResponse<TokenFilterResponse>>() {
      }.getType()
  ),

  TOKEN_DETAIL(
      () -> new TypeToken<TokenResponse>() {
      }.getType()
  ),
  NEWS(
      () -> new TypeToken<>() {
      }.getType()
  );

  private final Supplier<Type> type;

}
