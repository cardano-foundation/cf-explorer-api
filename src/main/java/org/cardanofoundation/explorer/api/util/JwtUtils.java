package org.cardanofoundation.explorer.api.util;

import java.security.PublicKey;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import jakarta.servlet.http.HttpServletRequest;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.util.StringUtils;

import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.UnsupportedJwtException;
import io.jsonwebtoken.security.SignatureException;

import org.cardanofoundation.explorer.common.exception.BusinessException;
import org.cardanofoundation.explorer.common.exception.CommonErrorCode;

@Log4j2
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class JwtUtils {

  /*
   * @since: 11/11/2022
   * description: get accountId from token with rsa public key
   * @update: 22/05/2023
   */
  public static String getAccountIdFromJwtToken(String token, PublicKey publicKey) {
    return Jwts.parserBuilder()
        .setSigningKey(publicKey)
        .build()
        .parseClaimsJws(token)
        .getBody()
        .getSubject();
  }

  /*
   * @since: 11/11/2022
   * description: parse token from header request
   * @update:
   */
  public static String parseJwt(HttpServletRequest request) {
    final String headerAuthentication = request.getHeader("Authorization");

    if (StringUtils.hasText(headerAuthentication) && headerAuthentication.startsWith("Bearer ")) {
      return headerAuthentication.substring(7);
    }
    return null;
  }

  /*
   * @since: 11/11/2022
   * description: validate token with rsa public key
   * @update:
   */
  public static void validateJwtToken(String token, PublicKey publicKey) {
    try {
      Jwts.parserBuilder().setSigningKey(publicKey).build().parseClaimsJws(token);
    } catch (SignatureException e) {
      log.error("Invalid JWT signature: {}", e.getMessage());
      throw new BusinessException(CommonErrorCode.TOKEN_INVALID_SIGNATURE);
    } catch (MalformedJwtException e) {
      log.error("Invalid JWT token: {}", e.getMessage());
      throw new BusinessException(CommonErrorCode.INVALID_TOKEN);
    } catch (ExpiredJwtException e) {
      log.error("JWT token is expired: {}", e.getMessage());
      throw new BusinessException(CommonErrorCode.TOKEN_EXPIRED);
    } catch (UnsupportedJwtException e) {
      log.error("JWT token is unsupported: {}", e.getMessage());
      throw new BusinessException(CommonErrorCode.TOKEN_UNSUPPORTED);
    } catch (IllegalArgumentException e) {
      log.error("JWT claims string is empty: {}", e.getMessage());
      throw new BusinessException(CommonErrorCode.TOKEN_IS_EMPTY);
    }
  }

  public static Set<String> getRolesFromToken(String token, PublicKey publicKey) {
    var claims =
        Jwts.parserBuilder().setSigningKey(publicKey).build().parseClaimsJws(token).getBody();
    try {
      Map<String, List<String>> realmAccessMap =
          (Map<String, List<String>>) claims.get("realm_access");
      if (Objects.nonNull(realmAccessMap) && Objects.nonNull(realmAccessMap.get("roles"))) {
        return new HashSet<>(realmAccessMap.get("roles"));
      }
      return Collections.emptySet();
    } catch (Exception e) {
      return Collections.emptySet();
    }
  }
}
