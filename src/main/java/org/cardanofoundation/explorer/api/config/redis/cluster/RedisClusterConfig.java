package org.cardanofoundation.explorer.api.config.redis.cluster;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import lombok.extern.log4j.Log4j2;
import lombok.val;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CachingConfigurer;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.connection.RedisClusterConfiguration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.jedis.JedisClientConfiguration;
import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettuceClientConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.GenericToStringSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

@Log4j2
@Configuration
@Profile("cluster")
public class RedisClusterConfig implements CachingConfigurer {

  @Value("${spring.redis.cluster.nodes}")
  private List<String> nodes;

  @Value("${spring.redis.password}")
  private String password;

  @Bean
  RedisClusterConfiguration redisClusterConfiguration() {
    RedisClusterConfiguration redisClusterConfiguration = new RedisClusterConfiguration(nodes);
    redisClusterConfiguration.setPassword(password);
    return redisClusterConfiguration;
  }

  @Bean(name = "lettuceConnectionFactory")
  @Autowired
  LettuceConnectionFactory lettuceConnectionFactory(
      RedisClusterConfiguration redisClusterConfiguration) {
    LettuceClientConfiguration lettuceClientConfiguration =
        LettuceClientConfiguration.builder().useSsl().build();
    return new LettuceConnectionFactory(redisClusterConfiguration, lettuceClientConfiguration);
  }

  @Bean
  @Autowired
  RedisTemplate<String, ?> redisTemplate( // NOSONAR
      final LettuceConnectionFactory lettuceConnectionFactory) {
    var redisTemplate = new RedisTemplate<String, Object>();
    redisTemplate.setConnectionFactory(lettuceConnectionFactory);
    redisTemplate.setKeySerializer(new StringRedisSerializer());
    redisTemplate.setValueSerializer(new GenericJackson2JsonRedisSerializer());
    redisTemplate.setDefaultSerializer(new GenericJackson2JsonRedisSerializer());
    redisTemplate.setHashValueSerializer(new GenericJackson2JsonRedisSerializer());
    return redisTemplate;
  }

  @Bean
  @Autowired
  RedisTemplate<String, String> redisTemplateString( // NOSONAR // TODO will remove in next version
      final LettuceConnectionFactory lettuceConnectionFactory) {
    var redisTemplate = new RedisTemplate<String, String>();
    redisTemplate.setConnectionFactory(lettuceConnectionFactory);
    redisTemplate.setValueSerializer(new GenericToStringSerializer<>(Object.class));
    redisTemplate.setKeySerializer(new StringRedisSerializer());
    redisTemplate.setDefaultSerializer(new GenericJackson2JsonRedisSerializer());
    redisTemplate.setHashValueSerializer(new GenericJackson2JsonRedisSerializer());
    return redisTemplate;
  }

  @Override
  public KeyGenerator keyGenerator() {
    return (target, method, params) -> {
      val sb = new StringBuilder();
      sb.append(target.getClass().getName());
      sb.append(method.getName());
      Arrays.stream(params).sequential().forEach(sb::append);
      log.info("call Redis cache Key : " + sb);
      return sb.toString();
    };
  }

  @Bean(name = "jedisConnectionFactory")
  @Autowired
  JedisConnectionFactory jedisConnectionFactory(
      RedisClusterConfiguration redisClusterConfiguration) {
    return new JedisConnectionFactory(
        redisClusterConfiguration, JedisClientConfiguration.builder().useSsl().build());
  }

  @Bean(name = "cacheManager")
  public RedisCacheManager cacheManager(
      @Qualifier("jedisConnectionFactory") RedisConnectionFactory connectionFactory) {
    return RedisCacheManager.RedisCacheManagerBuilder.fromConnectionFactory(connectionFactory)
        .withInitialCacheConfigurations(
            Collections.singletonMap("predefined", RedisCacheConfiguration.defaultCacheConfig()))
        .build();
  }
}
