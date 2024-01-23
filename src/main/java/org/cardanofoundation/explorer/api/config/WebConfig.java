package org.cardanofoundation.explorer.api.config;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import io.netty.channel.ChannelOption;
import io.netty.handler.timeout.ReadTimeoutHandler;
import io.netty.handler.timeout.WriteTimeoutHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.Request;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.common.validation.date.param.DateValidArgumentResolver;
import reactor.netty.http.client.HttpClient;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.reactive.ClientHttpConnector;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.time.Duration;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Configuration
@Log4j2
@RequiredArgsConstructor
public class WebConfig implements WebMvcConfigurer {

  private final AuthInterceptor authInterceptor;
  private final RoleFilterMapper roleConf;
  private static final int TIMEOUT = 10000;
  private static final int READ_TIMEOUT = 10000;
  private static final int WRITE_TIMEOUT = 10000;

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
        .addPathPatterns(
            roleConf.getAuth().stream().map(Request::getUri).collect(Collectors.toList()));
  }

  @Bean
  public DateValidArgumentResolver getDateValidArgumentResolver() {
    return new DateValidArgumentResolver();
  }

  @Override
  public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
    resolvers.add(this.getDateValidArgumentResolver());
  }

  @Bean
  public WebClient getWebClient() {
    HttpClient httpClient =
        HttpClient.create()
            .wiretap(Boolean.FALSE)
            .followRedirect(Boolean.TRUE)
            .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, TIMEOUT)
            .responseTimeout(Duration.ofMillis(TIMEOUT))
            .doOnConnected(
                connection -> {
                  connection.addHandlerFirst(
                      new ReadTimeoutHandler(READ_TIMEOUT, TimeUnit.MILLISECONDS));
                  connection.addHandlerFirst(
                      new WriteTimeoutHandler(WRITE_TIMEOUT, TimeUnit.MILLISECONDS));
                });

    return WebClient.builder()
        .clientConnector(new ReactorClientHttpConnector(httpClient))
        .defaultHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
        .build();
  }
}
