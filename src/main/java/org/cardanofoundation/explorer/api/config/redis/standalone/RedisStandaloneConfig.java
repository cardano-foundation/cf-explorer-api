package org.cardanofoundation.explorer.api.config.redis.standalone;

import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.GenericToStringSerializer;

@Log4j2
@Configuration
@Profile("standalone")
public class RedisStandaloneConfig {

    @Value("${spring.redis.standalone.host}")
    private String hostname;

    @Value("${spring.redis.standalone.port}")
    private Integer port;

    @Value("${spring.redis.password}")
    private String password;

    @Bean(name = "lettuceConnectionFactory")
    @Autowired
    LettuceConnectionFactory lettuceConnectionFactory() {
        RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration(hostname, port);
        redisStandaloneConfiguration.setPassword(password);
        return new LettuceConnectionFactory(redisStandaloneConfiguration);
    }

    @Bean
    @Autowired
    RedisTemplate<String, ?> redisTemplate(//NOSONAR
                                           final LettuceConnectionFactory lettuceConnectionFactory) {
        var redisTemplate = new RedisTemplate<String, Object>();
        redisTemplate.setConnectionFactory(lettuceConnectionFactory);
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
        redisTemplate.setValueSerializer(new GenericToStringSerializer<>(Object.class));
        return redisTemplate;
    }
}
