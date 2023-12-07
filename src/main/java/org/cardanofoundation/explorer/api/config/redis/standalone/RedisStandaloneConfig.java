package org.cardanofoundation.explorer.api.config.redis.standalone;

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
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.jedis.JedisClientConfiguration;
import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettuceClientConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.GenericToStringSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import java.time.Duration;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@Log4j2
@Configuration
@Profile("standalone")
public class RedisStandaloneConfig implements CachingConfigurer {

    @Value("${spring.redis.standalone.host}")
    private String hostname;

    @Value("${spring.redis.standalone.port}")
    private Integer port;

    @Value("${spring.redis.password}")
    private String password;

    @Value("${spring.redis.standalone.useSsl}")
    private boolean useSsl;

    @Value("${application.api.coin.gecko.market.interval-time}")
    private int apiMarketIntervalTime;

    @Bean
    RedisStandaloneConfiguration redisStandaloneConfiguration() {
        RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration(hostname, port);
        redisStandaloneConfiguration.setPassword(password);
        return redisStandaloneConfiguration;
    }

    @Bean(name = "lettuceConnectionFactory")
    @Autowired
    LettuceConnectionFactory lettuceConnectionFactory(RedisStandaloneConfiguration redisStandaloneConfiguration) {
        if(useSsl) {
            return new LettuceConnectionFactory(redisStandaloneConfiguration, LettuceClientConfiguration.builder().useSsl().build());
        } else {
            return new LettuceConnectionFactory(redisStandaloneConfiguration);
        }
    }

    @Bean
    @Autowired
    RedisTemplate<String, ?> redisTemplate(//NOSONAR
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
    RedisTemplate<String, String> redisTemplateString(//NOSONAR // TODO will remove in next version
                                                      final LettuceConnectionFactory lettuceConnectionFactory) {
        var redisTemplate = new RedisTemplate<String, String>();
        redisTemplate.setConnectionFactory(lettuceConnectionFactory);
        redisTemplate.setKeySerializer(new StringRedisSerializer());
        redisTemplate.setValueSerializer(new GenericToStringSerializer<>(Object.class));
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
    JedisConnectionFactory jedisConnectionFactory(RedisStandaloneConfiguration redisStandaloneConfiguration) {
        if(useSsl) {
            return new JedisConnectionFactory(redisStandaloneConfiguration, JedisClientConfiguration.builder().useSsl().build());
        } else {
            return new JedisConnectionFactory(redisStandaloneConfiguration);
        }
    }

    @Bean(name = "cacheManager")
    public RedisCacheManager cacheManager(
            @Qualifier("jedisConnectionFactory") RedisConnectionFactory connectionFactory) {
        RedisCacheConfiguration coinPriceConf = RedisCacheConfiguration.defaultCacheConfig()
                .entryTtl(Duration.ofSeconds(apiMarketIntervalTime));
        Map<String, RedisCacheConfiguration> cacheConfigurations = new HashMap<>();
        cacheConfigurations.put("market", coinPriceConf);
        return RedisCacheManager.RedisCacheManagerBuilder.fromConnectionFactory(connectionFactory)
                .withInitialCacheConfigurations(cacheConfigurations).build();
    }
}
