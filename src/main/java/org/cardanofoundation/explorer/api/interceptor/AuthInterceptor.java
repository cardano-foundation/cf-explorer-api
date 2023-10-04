package org.cardanofoundation.explorer.api.interceptor;

import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.RsaConfig;
import org.cardanofoundation.explorer.api.security.auth.RoleConfigurationMapper;
import org.cardanofoundation.explorer.api.security.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.security.auth.RoleFunction;
import org.cardanofoundation.explorer.api.security.auth.UserPrincipal;
import org.cardanofoundation.explorer.common.annotation.IgnoreAuthentication;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.InvalidAccessTokenException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.common.utils.JwtUtils;
import org.cardanofoundation.explorer.common.utils.StringUtils;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerExceptionResolver;
import org.springframework.web.servlet.HandlerInterceptor;

@Component
@RequiredArgsConstructor
@Log4j2
public class AuthInterceptor implements HandlerInterceptor {

  private final RoleFilterMapper roleConf;

  private final RsaConfig rsaConfig;

  private final RedisTemplate<String, Object> redisTemplate;

  private List<AntPathRequestMatcher> matchers;

  @PostConstruct
  public void initAuth() {
    matchers = roleConf.getAuth().stream()
        .map(request -> {
          if (org.springframework.util.StringUtils.hasText(request.getMethod())
              || request.getMethod().equals("*")) {
            return new AntPathRequestMatcher(request.getUri());
          }
          return new AntPathRequestMatcher(request.getUri(), request.getMethod());
        }).toList();
  }


  @Override
  public boolean preHandle(
      HttpServletRequest request, HttpServletResponse httpServletResponse,
      Object handler)
      throws Exception {
    log.info("Authentication Interceptor is running...");
    HandlerMethod handlerMethod;
    try {
      handlerMethod = (HandlerMethod) handler;
    } catch (ClassCastException e) {
      log.error("Error handler: " + e.getMessage());
      return HandlerInterceptor.super.preHandle(request, httpServletResponse, handler);
    }
    Method method = handlerMethod.getMethod();
    log.info("Authentication Interceptor: {}", method);
    if (method.isAnnotationPresent(IgnoreAuthentication.class)) {
      log.info("Ignore method if marked IgnoreAuthentication annotation");
      return true;
    }
    if (method.getDeclaringClass().isAnnotationPresent(IgnoreAuthentication.class)) {
      log.info("Ignore class if marked IgnoreAuthentication annotation");
      return true;
    }

    if (matchers.stream().noneMatch(matcher -> matcher.matches(request))) {
      return true;
    }
    UserPrincipal userPrincipal = new UserPrincipal();
    String token = getToken(request, userPrincipal);
    Set<String> roles = getRoles(token);
    Map<String, Map<String, Object>> roleDescription = new HashMap<>();
    for (RoleConfigurationMapper roleMapper : roleConf.getRoles()) {
      if (roles.contains(roleMapper.getName())) {
        String roleKey = roleMapper.getName();
        Map<String, Object> desc = roleMapper.getFunction().stream()
            .filter(Objects::nonNull)
            .findAny().orElse(new RoleFunction()).getDescription();

        if (Objects.nonNull(desc)) {
          roleDescription.put(roleKey, desc);
        }
      }
    }
    userPrincipal.setRoleDescription(roleDescription);
    request.setAttribute("user", userPrincipal);
    return true;

  }


  private String getToken(HttpServletRequest request, UserPrincipal userPrincipal) {
    String token = JwtUtils.parseJwt(request);
    try {
      JwtUtils.validateJwtToken(token, rsaConfig.getPublicKey());
    } catch (Exception e) {
      throw new InvalidAccessTokenException();
    }
    if (isTokenBlacklisted(token)) {
      throw new InvalidAccessTokenException();
    }
    String username = JwtUtils.getAccountIdFromJwtToken(token, rsaConfig.getPublicKey());
    userPrincipal.setUsername(username);
    return token;
  }

  private Set<String> getRoles(String token) {
    return JwtUtils.getRolesFromToken(token, rsaConfig.getPublicKey());
  }

  private boolean isTokenBlacklisted(String token) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(token))) {
      throw new BusinessException(CommonErrorCode.INVALID_TOKEN);
    }
    return redisTemplate.opsForValue().get(CommonConstant.JWT + token) != null;
  }
}
