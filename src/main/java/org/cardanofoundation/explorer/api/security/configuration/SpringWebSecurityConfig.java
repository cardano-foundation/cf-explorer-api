package org.cardanofoundation.explorer.api.security.configuration;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.access.channel.ChannelProcessingFilter;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;
import org.springframework.security.web.header.writers.ReferrerPolicyHeaderWriter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.security.web.util.matcher.RequestMatcher;

import org.cardanofoundation.explorer.api.config.RsaConfig;
import org.cardanofoundation.explorer.api.security.auth.Request;
import org.cardanofoundation.explorer.api.security.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.security.filter.DynamicFilter;

@Configuration
@EnableWebSecurity
@AllArgsConstructor
public class SpringWebSecurityConfig {

  private final RoleFilterMapper roleConf;
  private Set<Request> authRequests;
  private final RsaConfig rsaConfig;
  private final RedisTemplate<String, Object> redisTemplate;

  @Bean
  public SecurityFilterChain filterChain(final HttpSecurity http) throws Exception {
//    http.csrf((csrf) -> csrf.disable())
//        .headers(
//            (headers) ->
//                headers
//                    .contentSecurityPolicy(
//                        (policy) ->
//                            policy.policyDirectives(
//                                "default-src 'self'; script-src 'self' 'unsafe-inline'; script-src-elem 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'; style-src-elem 'self' 'unsafe-inline'; img-src 'self' 'unsafe-inline'"))
//                    .referrerPolicy(
//                        (policy) ->
//                            policy.policy(ReferrerPolicyHeaderWriter.ReferrerPolicy.SAME_ORIGIN))
//                    .permissionsPolicy((policy) -> policy.policy("geolocation=(self)")));
    AntPathRequestMatcher[] ants = roleConf.getAuth().stream()
        .map(request -> new AntPathRequestMatcher(request.getUri(),
            request.getMethod())).toArray(AntPathRequestMatcher[]::new);

//    AntPathRequestMatcher[]  ants = new AntPathRequestMatcher[]{};

    if (ants.length > 0) {
      http
          .addFilterAfter(new DynamicFilter(roleConf, authRequests, rsaConfig, redisTemplate),
              BasicAuthenticationFilter.class)
          .authorizeHttpRequests(request ->
              request.requestMatchers(ants).permitAll()
          );
    }
    http.authorizeHttpRequests().anyRequest().permitAll();

    return http.build();
  }


}
