package com.cardano.explorer.config;

import com.cardano.explorer.interceptor.AuthInterceptor;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
@Log4j2
public class WebConfig implements WebMvcConfigurer {

  @Autowired
  private AuthInterceptor authInterceptor;

  @Override
  public void addInterceptors(InterceptorRegistry registry) {
    log.info("Authentication interceptor is adding to the service");
    registry.addInterceptor(authInterceptor).addPathPatterns("/api/**");
  }
  @Override
  public void addCorsMappings(CorsRegistry registry) {
    registry
        .addMapping("/**")
        .allowedOrigins("*")
        .allowedMethods("GET", "POST", "PUT", "PATCH", "DELETE", "HEAD");
  }

}
