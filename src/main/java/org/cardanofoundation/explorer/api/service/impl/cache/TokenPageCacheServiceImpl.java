package org.cardanofoundation.explorer.api.service.impl.cache;

import com.google.gson.*;
import com.google.gson.reflect.TypeToken;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.cardanofoundation.explorer.api.common.enumeration.RedisKey;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.service.cache.TokenPageCacheService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

@Service
@RequiredArgsConstructor
@Slf4j
public class TokenPageCacheServiceImpl implements TokenPageCacheService {
  private final RedisTemplate<String, Object> redisTemplate;

  @Value("${application.network}")
  private String network;

  private static final Gson GSON = new GsonBuilder()
      .registerTypeAdapter(LocalDate.class,
          (JsonSerializer<LocalDate>) (value, type, context) ->
              new JsonPrimitive(value.format(DateTimeFormatter.ISO_LOCAL_DATE))
      )
      .registerTypeAdapter(LocalDateTime.class,
          (JsonSerializer<LocalDateTime>) (value, type, context) ->
              new JsonPrimitive(value.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      )
      .registerTypeAdapter(LocalDate.class,
          (JsonDeserializer<LocalDate>) (jsonElement, type, context) ->
              LocalDate.parse(
                  jsonElement.getAsJsonPrimitive().getAsString(),
                  DateTimeFormatter.ISO_LOCAL_DATE
              )
      )
      .registerTypeAdapter(LocalDateTime.class,
          (JsonDeserializer<LocalDateTime>) (jsonElement, type, context) ->
              LocalDateTime.parse(
                  jsonElement.getAsJsonPrimitive().getAsString(),
                  DateTimeFormatter.ISO_LOCAL_DATE_TIME
              )
      )
      .create();

  @Override
  public Optional<BaseFilterResponse<TokenFilterResponse>> getTokePageCache(Pageable pageable) {
    String redisKey = RedisKey.REDIS_TOKEN_PAGE.name() + ":" + network + ":" + toStr(pageable);
    Object cacheData = redisTemplate.opsForValue().get(redisKey);
    if (cacheData == null) {
      return Optional.empty();
    }
    try {
      return Optional.of(GSON.fromJson(cacheData.toString(), new TypeToken<BaseFilterResponse<TokenFilterResponse>>() {
      }.getType()));
    } catch (Exception e) {
      log.error("Exception when getTokenFilterResponseSchedulerCache key: {}", redisKey, e);
      return Optional.empty();
    }
  }

  private String toStr(Pageable pageable) {
    return pageable.toString().replace(" ", "").replace(":", "_");
  }
}
