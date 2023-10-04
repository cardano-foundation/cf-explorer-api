package org.cardanofoundation.explorer.api.security.filter;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.servlet.HandlerExceptionResolver;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.RsaConfig;
import org.cardanofoundation.explorer.api.security.auth.RoleConfigurationMapper;
import org.cardanofoundation.explorer.api.security.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.security.auth.RoleFunction;
import org.cardanofoundation.explorer.api.security.auth.UserPrincipal;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.InvalidAccessTokenException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.common.utils.JwtUtils;
import org.cardanofoundation.explorer.common.utils.StringUtils;

@Component
public class DynamicFilter extends OncePerRequestFilter {

  private RoleFilterMapper roleConf;

  private RsaConfig rsaConfig;

  private RedisTemplate<String, Object> redisTemplate;

  private List<AntPathRequestMatcher> matchers;

  @Qualifier("handlerExceptionResolver")
  private HandlerExceptionResolver resolver;

  public DynamicFilter(RoleFilterMapper roleConf, RsaConfig rsaConfig,
                       RedisTemplate<String, Object> redisTemplate,
                       HandlerExceptionResolver handlerExceptionResolver) {
    this.roleConf = roleConf;
    this.rsaConfig = rsaConfig;
    this.redisTemplate = redisTemplate;
    this.resolver = handlerExceptionResolver;
    matchers = roleConf.getAuth().stream()
        .map(request -> {
          if (org.springframework.util.StringUtils.hasText(request.getMethod())
              || request.getMethod().equals("*")) {
            return new AntPathRequestMatcher(request.getUri());
          }
          return new AntPathRequestMatcher(request.getUri(), request.getMethod());
        }).toList();
  }

  private boolean isTokenBlacklisted(String token) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(token))) {
      throw new BusinessException(CommonErrorCode.INVALID_TOKEN);
    }
    return redisTemplate.opsForValue().get(CommonConstant.JWT + token) != null;
  }


  @Override
  public void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
                               FilterChain filterChain)
      throws IOException, ServletException, InvalidAccessTokenException {
    if (matchers.stream().noneMatch(matcher -> matcher.matches(request))) {
      filterChain.doFilter(request, response);
      return;
    }
    try {
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
      SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(
          userPrincipal,
          null
      ));
      filterChain.doFilter(request, response);
    } catch (Exception e) {
      resolver.resolveException(request, response, null, e);
    }
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

}