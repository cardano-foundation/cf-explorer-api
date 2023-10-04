package org.cardanofoundation.explorer.api.security.configuration;

import java.util.List;
import java.util.Set;

import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;
import org.springframework.security.web.header.writers.ReferrerPolicyHeaderWriter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.web.servlet.HandlerExceptionResolver;

import org.cardanofoundation.explorer.api.config.RsaConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.security.auth.Request;
import org.cardanofoundation.explorer.api.security.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.security.filter.DynamicFilter;

@Configuration
@EnableWebSecurity
@AllArgsConstructor
public class SpringWebSecurityConfig {

  private RoleFilterMapper roleConf;

  private RsaConfig rsaConfig;

  private RedisTemplate<String, Object> redisTemplate;


  @Qualifier("handlerExceptionResolver")
  private HandlerExceptionResolver handlerExceptionResolver;

  @Bean
  public SecurityFilterChain filterChain(final HttpSecurity http) throws Exception {
    http.csrf((csrf) -> csrf.disable())
        .authorizeHttpRequests((matcherRegistry) -> matcherRegistry.anyRequest().permitAll())
        .headers(
            (headers) ->
                headers
                    .contentSecurityPolicy(
                        (policy) ->
                            policy.policyDirectives(
                                "default-src 'self'; script-src 'self' 'unsafe-inline'; script-src-elem 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'; style-src-elem 'self' 'unsafe-inline'; img-src 'self' 'unsafe-inline'"))
                    .referrerPolicy(
                        (policy) ->
                            policy.policy(ReferrerPolicyHeaderWriter.ReferrerPolicy.SAME_ORIGIN))
                    .permissionsPolicy((policy) -> policy.policy("geolocation=(self)")));
    http.addFilterAfter(
        new DynamicFilter(roleConf, rsaConfig, redisTemplate, handlerExceptionResolver),
        BasicAuthenticationFilter.class);
    return http.build();
  }


}
