package org.cardanofoundation.explorer.api.security.filter;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import jakarta.annotation.PostConstruct;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import lombok.AllArgsConstructor;

import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.OncePerRequestFilter;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.config.RsaConfig;
import org.cardanofoundation.explorer.api.security.auth.Request;
import org.cardanofoundation.explorer.api.security.auth.RoleConfigurationMapper;
import org.cardanofoundation.explorer.api.security.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.security.auth.RoleFunction;
import org.cardanofoundation.explorer.api.security.auth.UserPrincipal;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.InvalidAccessTokenException;
import org.cardanofoundation.explorer.common.exceptions.enums.CommonErrorCode;
import org.cardanofoundation.explorer.common.utils.JwtUtils;
import org.cardanofoundation.explorer.common.utils.StringUtils;

import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.USERNAME;

@AllArgsConstructor
public class DynamicFilter extends OncePerRequestFilter {


  private final RoleFilterMapper roleConf;

  private Set<Request> authRequests;

  private final RsaConfig rsaConfig;

  private final RedisTemplate<String, Object> redisTemplate;


  private boolean isTokenBlacklisted(String token) {
    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(token))) {
      throw new BusinessException(CommonErrorCode.INVALID_TOKEN);
    }
    return redisTemplate.opsForValue().get(CommonConstant.JWT + token) != null;
  }

//  @PostConstruct
//  public void loadAuthRequests() {
//    authRequests = roleConf.getRoles().stream()
//        .flatMap(roleConfigurationMapper -> roleConfigurationMapper.getFunction().stream())
//        .map(roleFunction -> Request.builder().uri(roleFunction.getUri())
//            .method(roleFunction.getMethod()).build())
//        .collect(Collectors.toSet());
//
//    authRequests.addAll(roleConf.getAuth());
//  }


  @Override
  public void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
                               FilterChain filterChain) throws IOException, ServletException {
    System.out.println("ggggggggggggggggggggggggggggg");
    UserPrincipal userPrincipal = new UserPrincipal();
    String token = getToken(request,userPrincipal);
    Set<String> roles = getRoles(token);
    Map<String, Map<String,Object>> roleDescription = new HashMap<>();
    for (RoleConfigurationMapper roleMapper : roleConf.getRoles()) {
      if (roles.contains(roleMapper.getName())) {
        String roleKey = roleMapper.getName();
        Map<String, Object> desc = roleMapper.getFunction().stream()
            .filter(Objects::nonNull)
            .findAny().orElse(new RoleFunction()).getDescription();

        if (Objects.nonNull(desc)) {
          roleDescription.put(roleKey,desc);
        }
      }
    }
    userPrincipal.setRoleDescription(roleDescription);
    SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(
        userPrincipal,
        null
    ));
    filterChain.doFilter(request, response);
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