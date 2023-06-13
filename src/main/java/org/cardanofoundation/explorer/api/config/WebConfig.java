package org.cardanofoundation.explorer.api.config;

import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.common.validation.date.param.DateValidArgumentResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.util.List;

@Configuration
@Log4j2
@RequiredArgsConstructor
public class WebConfig implements WebMvcConfigurer {

  private final AuthInterceptor authInterceptor;

  @Override
  public void addCorsMappings(CorsRegistry registry) {
    registry
        .addMapping("/**")
        .allowedOrigins("*")
        .allowedMethods("GET", "POST", "PUT", "PATCH", "DELETE", "HEAD");
  }

  @Bean
  public RestTemplate getRestTemplate() {
    return new RestTemplate();
  }

  @Override
  public void addInterceptors(InterceptorRegistry registry) {
    log.info("Authentication interceptor is adding to the service");
    registry.addInterceptor(authInterceptor)
        .addPathPatterns("/api/v1/pool-report/**", "/api/v1/staking-lifecycle/report/**");
  }

  @Bean
  public DateValidArgumentResolver getDateValidArgumentResolver() {
    return new DateValidArgumentResolver();
  }

  @Override
  public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
    resolvers.add(this.getDateValidArgumentResolver());
  }
}
