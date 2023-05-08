package com.cardano.explorer.interceptor;

import com.cardano.explorer.common.constant.CommonConstant;
import com.cardano.explorer.config.RsaConfig;
import com.sotatek.cardanocommonapi.annotation.IgnoreAuthentication;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import com.sotatek.cardanocommonapi.exceptions.InvalidAccessTokenException;
import com.sotatek.cardanocommonapi.exceptions.enums.CommonErrorCode;
import com.sotatek.cardanocommonapi.utils.JwtUtils;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.lang.reflect.Method;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

@Component
@RequiredArgsConstructor
@Log4j2
public class AuthInterceptor implements HandlerInterceptor {

  private final RsaConfig rsaConfig;

  private final RedisTemplate<String, Object> redisTemplate;

  private boolean isTokenBlacklisted(String token) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(token))) {
      throw new BusinessException(CommonErrorCode.INVALID_TOKEN);
    }
    return redisTemplate.opsForValue().get(CommonConstant.JWT + token) != null;
  }

  @Override
  public boolean preHandle(
      HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse,
      Object handler)
      throws Exception {
    log.info("Authentication Interceptor is running...");
    HandlerMethod handlerMethod;
    try {
      handlerMethod = (HandlerMethod) handler;
    } catch (ClassCastException e) {
      log.error("Error handler: " + e.getMessage());
      return HandlerInterceptor.super.preHandle(httpServletRequest, httpServletResponse, handler);
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
    String token = JwtUtils.parseJwt(httpServletRequest);
    JwtUtils.validateJwtToken(token, rsaConfig.getPublicKey());
    if (isTokenBlacklisted(token)) {
      throw new InvalidAccessTokenException();
    }
    String username = JwtUtils.getUserNameFromJwtToken(token, rsaConfig.getPublicKey());
    httpServletRequest.setAttribute("username", username);
    return true;
  }
}
