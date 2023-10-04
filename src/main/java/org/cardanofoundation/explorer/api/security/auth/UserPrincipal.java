package org.cardanofoundation.explorer.api.security.auth;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserPrincipal {
  Map<String, Map<String,Object>> roleDescription;
  String username;
}
