package org.cardanofoundation.explorer.api.interceptor;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.RsaConfig;
import org.cardanofoundation.explorer.api.interceptor.auth.Request;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleConfigurationMapper;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFunction;
import org.cardanofoundation.explorer.api.interceptor.auth.UserPrincipal;

import org.cardanofoundation.explorer.common.annotation.IgnoreAuthentication;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.InvalidAccessTokenException;
import org.cardanofoundation.explorer.common.exceptions.UnauthorizedException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.common.utils.JwtUtils;
import org.cardanofoundation.explorer.common.utils.StringUtils;

@Component
@Log4j2
public class AuthInterceptor implements HandlerInterceptor {

  private final RoleFilterMapper roleConf;

  private final RsaConfig rsaConfig;

  private final RedisTemplate<String, Object> redisTemplate;

  private List<AntPathRequestMatcher> matchers;

  private Map<AntPathRequestMatcher, Request> authorEndpoint;

  public AuthInterceptor(RoleFilterMapper roleFilterMapper, RsaConfig rsaConfig,
                         RedisTemplate<String, Object> redisTemplate) {
    this.roleConf = roleFilterMapper;
    this.rsaConfig = rsaConfig;
    this.redisTemplate = redisTemplate;
  }

  @PostConstruct
  public void initAuth() {
    matchers = roleConf.getAuth().stream()
        .map(this::convertRequestToAntPathRequestMatcher).toList();

    authorEndpoint = roleConf.getAuth().stream()
        .collect(Collectors.toMap(this::convertRequestToAntPathRequestMatcher,
            Function.identity()));
  }

  private AntPathRequestMatcher convertRequestToAntPathRequestMatcher(Request request) {
    if (org.springframework.util.StringUtils.hasText(request.getMethod())
        || request.getMethod().equals("*")) {
      return new AntPathRequestMatcher(request.getUri());
    }
    return new AntPathRequestMatcher(request.getUri(), request.getMethod());
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

    var matcherOpt = matchers.stream().filter(matchers -> matchers.matches(request)).findFirst();
    if (matcherOpt.isEmpty()) {// if request need auth
      return true;
    }

    UserPrincipal userPrincipal = new UserPrincipal();
    String token = getToken(request, userPrincipal);
    Set<String> roles = getRoles(token);

    checkRequestAllow(matcherOpt.get(), roles);//check author

    Map<String, Map<String, Object>> roleDescription = new HashMap<>();
    for (RoleConfigurationMapper roleMapper : roleConf.getRoles()) {
      if (roles.contains(roleMapper.getName())) {
        String roleKey = roleMapper.getName();

        if (Objects.nonNull(roleMapper.getAttributes())) {
          roleDescription.put(roleKey, new HashMap<>(roleMapper.getAttributes()));
        }
      }
    }
    userPrincipal.setRoleDescription(roleDescription);
    request.setAttribute("user", userPrincipal);
    return true;

  }


  private void checkRequestAllow(AntPathRequestMatcher matcher, Set<String> roles) {
    boolean isAllowed = false;
    if (authorEndpoint.containsKey(matcher)) {
      Request requestAuthor = authorEndpoint.get(matcher);
      var rolesAuth = getRoles(requestAuthor.getRoles());
      if (rolesAuth.length == 0) {// This feature doesn't require authorization
        isAllowed = true;
      }
      for (String role : rolesAuth) {
        if (roles.contains(role)) {
          isAllowed = true;
        }
      }

    } else {// don't need to auth
      isAllowed = true;
    }
    if (!isAllowed) {
      throw new UnauthorizedException();
    }
  }

  private String[] getRoles(String[] roles) {
    if (Objects.isNull(roles)) {
      return new String[]{};
    }
    return roles;
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
    return redisTemplate.hasKey(CommonConstant.JWT + token);
  }
}
