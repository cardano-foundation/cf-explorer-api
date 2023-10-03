package org.cardanofoundation.explorer.api.security.auth;

import java.util.Map;

import lombok.Data;

@Data
public class UserPrincipal {
  Map<String, Map<String,Object>> roleDescription;
  String username;
}
